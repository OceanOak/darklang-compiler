// CompilerLibrary.fs - Library API for the Dark compiler
//
// Exposes the compiler as a library for use in tests and other tools.
// Provides clean functions that can be called without spawning processes.

module CompilerLibrary

open CodeGen
open IRPrinter

open System
open System.IO
open System.Diagnostics
open System.Reflection
open Output
open Trace

/// Result of compilation
type CompileResult = Result<byte array, string>

/// Result of execution
type ExecutionResult = {
    ExitCode: int
    Stdout: string
    Stderr: string
}

/// Result of execution with timing breakdown
type TimedExecutionResult = {
    ExitCode: int
    Stdout: string
    Stderr: string
    CompileTime: TimeSpan
    RuntimeTime: TimeSpan
}

/// Compiler options for controlling optimization behavior
type CompilerOptions = {
    /// Disable free list memory reuse (always bump allocate)
    DisableFreeList: bool
    /// Disable ANF-level optimizations (constant folding, propagation, etc.)
    DisableANFOpt: bool
    /// Disable ANF constant folding (includes algebraic identities and constant branches)
    DisableANFConstFolding: bool
    /// Disable ANF constant propagation
    DisableANFConstProp: bool
    /// Disable ANF copy propagation
    DisableANFCopyProp: bool
    /// Disable ANF dead code elimination
    DisableANFDCE: bool
    /// Disable ANF strength reduction (pow2 mul/div/mod)
    DisableANFStrengthReduction: bool
    /// Disable ANF function inlining
    DisableInlining: bool
    /// Disable tail call optimization
    DisableTCO: bool
    /// Disable MIR-level optimizations (DCE, copy/constant propagation on SSA)
    DisableMIROpt: bool
    /// Disable MIR constant folding
    DisableMIRConstFolding: bool
    /// Disable MIR common subexpression elimination
    DisableMIRCSE: bool
    /// Disable MIR copy propagation
    DisableMIRCopyProp: bool
    /// Disable MIR dead code elimination
    DisableMIRDCE: bool
    /// Disable MIR CFG simplification
    DisableMIRCFGSimplify: bool
    /// Disable MIR loop-invariant code motion
    DisableMIRLICM: bool
    /// Disable LIR-level optimizations (peephole optimizations)
    DisableLIROpt: bool
    /// Disable LIR peephole optimizations
    DisableLIRPeephole: bool
    /// Disable function tree shaking (pruning unused stdlib/user functions)
    DisableFunctionTreeShaking: bool
    /// Enable runtime expression coverage tracking
    EnableCoverage: bool
    /// Enable leak checking (debug only)
    EnableLeakCheck: bool
    /// Dump ANF representations to stdout
    DumpANF: bool
    /// Dump MIR representations to stdout
    DumpMIR: bool
    /// Dump LIR representations to stdout (before and after register allocation)
    DumpLIR: bool
}

/// Default compiler options
let defaultOptions : CompilerOptions = {
    DisableFreeList = false
    DisableANFOpt = false
    DisableANFConstFolding = false
    DisableANFConstProp = false
    DisableANFCopyProp = false
    DisableANFDCE = false
    DisableANFStrengthReduction = false
    DisableInlining = false
    DisableTCO = false
    DisableMIROpt = false
    DisableMIRConstFolding = false
    DisableMIRCSE = false
    DisableMIRCopyProp = false
    DisableMIRDCE = false
    DisableMIRCFGSimplify = false
    DisableMIRLICM = false
    DisableLIROpt = false
    DisableLIRPeephole = false
    DisableFunctionTreeShaking = false
    EnableCoverage = false
    EnableLeakCheck = false
    DumpANF = false
    DumpMIR = false
    DumpLIR = false
}

/// Determine whether to dump a specific IR, based on verbosity or explicit option
let shouldDumpIR (verbosity: int) (enabled: bool) : bool =
    verbosity >= 3 || enabled

let private buildANFOptimizeOptions (options: CompilerOptions) : ANF_Optimize.OptimizeOptions =
    let enabled = not options.DisableANFOpt
    {
        EnableConstFolding = enabled && not options.DisableANFConstFolding
        EnableConstProp = enabled && not options.DisableANFConstProp
        EnableCopyProp = enabled && not options.DisableANFCopyProp
        EnableDCE = enabled && not options.DisableANFDCE
        EnableStrengthReduction = enabled && not options.DisableANFStrengthReduction
    }

let private shouldRunANFOptimize (anfOptions: ANF_Optimize.OptimizeOptions) : bool =
    anfOptions.EnableConstFolding
    || anfOptions.EnableConstProp
    || anfOptions.EnableCopyProp
    || anfOptions.EnableDCE
    || anfOptions.EnableStrengthReduction

let private buildMIROptimizeOptions (options: CompilerOptions) : MIR_Optimize.OptimizeOptions =
    let enabled = not options.DisableMIROpt
    {
        EnableConstFolding = enabled && not options.DisableMIRConstFolding
        EnableCSE = enabled && not options.DisableMIRCSE
        EnableCopyProp = enabled && not options.DisableMIRCopyProp
        EnableDCE = enabled && not options.DisableMIRDCE
        EnableCFGSimplify = enabled && not options.DisableMIRCFGSimplify
        EnableLICM = enabled && not options.DisableMIRLICM
    }

let private shouldRunMIROptimize (mirOptions: MIR_Optimize.OptimizeOptions) : bool =
    mirOptions.EnableConstFolding
    || mirOptions.EnableCSE
    || mirOptions.EnableCopyProp
    || mirOptions.EnableDCE
    || mirOptions.EnableCFGSimplify
    || mirOptions.EnableLICM

let private formatPassGroup (label: string) (passes: (string * bool) list) : string =
    let enabled =
        passes
        |> List.choose (fun (name, isEnabled) -> if isEnabled then Some name else None)
    let enabledNames = String.concat ", " enabled
    match enabled with
    | [] -> $"{label} (disabled)"
    | _ -> $"{label} ({enabledNames})"

/// Print ANF program in a consistent, human-readable format
let printANFProgram (title: string) (program: ANF.Program) : unit =
    println title
    println (formatANF program)
    println ""

/// Print MIR program (with CFG) in a consistent format
let printMIRProgram (title: string) (program: MIR.Program) : unit =
    println title
    println (formatMIR program)
    println ""

/// Print LIR program (with CFG) in a consistent format
let printLIRProgram (title: string) (program: LIR.Program) : unit =
    println title
    println (formatLIR program)
    println ""

/// Print symbolic LIR program (with CFG) in a consistent format
let printLIRSymbolicProgram (title: string) (program: LIRSymbolic.Program) : unit =
    println title
    println (formatLIRSymbolic program)
    println ""

/// Run SSA + MIR/LIR optimizations, returning an optimized LIR program
let private compileMirToLir
    (verbosity: int)
    (options: CompilerOptions)
    (sw: Stopwatch)
    (stageSuffix: string)
    (mirProgram: MIR.Program)
    : Result<LIRSymbolic.Program, string> =

    let suffix = if stageSuffix = "" then "" else $" ({stageSuffix})"

    if verbosity >= 1 then println $"  [3.1/8] SSA Construction{suffix}..."
    let ssaStart = sw.Elapsed.TotalMilliseconds
    let ssaProgram = SSA_Construction.convertToSSA mirProgram
    if verbosity >= 2 then
        let t = System.Math.Round(sw.Elapsed.TotalMilliseconds - ssaStart, 1)
        println $"        {t}ms"

    let mirOptions = buildMIROptimizeOptions options
    let mirPassLabel =
        formatPassGroup
            "MIR Optimizations"
            [
                ("const_folding", mirOptions.EnableConstFolding)
                ("cse", mirOptions.EnableCSE)
                ("copy_prop", mirOptions.EnableCopyProp)
                ("dce", mirOptions.EnableDCE)
                ("cfg_simplify", mirOptions.EnableCFGSimplify)
                ("licm", mirOptions.EnableLICM)
            ]
    if verbosity >= 1 then println $"  [3.5/8] {mirPassLabel}{suffix}..."
    let mirOptStart = sw.Elapsed.TotalMilliseconds
    let optimizedProgram =
        if shouldRunMIROptimize mirOptions then
            MIR_Optimize.optimizeProgramWithOptions mirOptions ssaProgram
        else
            ssaProgram
    if verbosity >= 2 then
        let t = System.Math.Round(sw.Elapsed.TotalMilliseconds - mirOptStart, 1)
        println $"        {t}ms"

    if verbosity >= 1 then println $"  [4/8] MIR → LIR{suffix}..."
    let lirStart = sw.Elapsed.TotalMilliseconds
    let lirResult = MIR_to_LIR.toLIR optimizedProgram
    match lirResult with
    | Error err -> Error $"LIR conversion error: {err}"
    | Ok lirProgram ->
        if shouldDumpIR verbosity options.DumpLIR then
            printLIRSymbolicProgram "=== LIR (Low-level IR with CFG) ===" lirProgram
        if verbosity >= 2 then
            let t = System.Math.Round(sw.Elapsed.TotalMilliseconds - lirStart, 1)
            println $"        {t}ms"

        let lirPassLabel =
            formatPassGroup
                "LIR Peephole"
                [("peephole", not options.DisableLIROpt && not options.DisableLIRPeephole)]
        if verbosity >= 1 then println $"  [4.5/8] {lirPassLabel}{suffix}..."
        let lirOptStart = sw.Elapsed.TotalMilliseconds
        let optimizedLir =
            if options.DisableLIROpt || options.DisableLIRPeephole then lirProgram
            else LIR_Peephole.optimizeProgram lirProgram
        if verbosity >= 2 then
            let t = System.Math.Round(sw.Elapsed.TotalMilliseconds - lirOptStart, 1)
            println $"        {t}ms"
        Ok optimizedLir

/// Run MIR+LIR passes from ANF, returning an optimized LIR program
let private compileAnfToLir
    (verbosity: int)
    (options: CompilerOptions)
    (sw: Stopwatch)
    (stageSuffix: string)
    (anfProgram: ANF.Program)
    (typeMap: ANF.TypeMap)
    (convResult: AST_to_ANF.ConversionResult)
    (programType: AST.Type)
    (externalReturnTypes: Map<string, AST.Type>)
    : Result<LIRSymbolic.Program, string> =

    let suffix = if stageSuffix = "" then "" else $" ({stageSuffix})"

    if verbosity >= 1 then println $"  [3/8] ANF → MIR{suffix}..."
    let mirStart = sw.Elapsed.TotalMilliseconds
    let mirResult =
        ANF_to_MIR.toMIR
            anfProgram
            typeMap
            convResult.FuncParams
            programType
            convResult.VariantLookup
            convResult.TypeReg
            options.EnableCoverage
            externalReturnTypes
    match mirResult with
    | Error err -> Error $"MIR conversion error: {err}"
    | Ok mirProgram ->
        if shouldDumpIR verbosity options.DumpMIR then
            printMIRProgram "=== MIR (Control Flow Graph) ===" mirProgram
        if verbosity >= 2 then
            let t = System.Math.Round(sw.Elapsed.TotalMilliseconds - mirStart, 1)
            println $"        {t}ms"
        compileMirToLir verbosity options sw stageSuffix mirProgram

/// Run MIR+LIR passes from ANF functions only, returning an optimized LIR program
let private compileAnfFunctionsOnlyToLir
    (verbosity: int)
    (options: CompilerOptions)
    (sw: Stopwatch)
    (stageSuffix: string)
    (anfProgram: ANF.Program)
    (typeMap: ANF.TypeMap)
    (funcParams: Map<string, (string * AST.Type) list>)
    (variantLookup: AST_to_ANF.VariantLookup)
    (recordRegistry: Map<string, (string * AST.Type) list>)
    (externalReturnTypes: Map<string, AST.Type>)
    : Result<LIRSymbolic.Program, string> =

    let suffix = if stageSuffix = "" then "" else $" ({stageSuffix})"

    if verbosity >= 1 then println $"  [3/8] ANF → MIR{suffix}..."
    let mirStart = sw.Elapsed.TotalMilliseconds
    let mirResult =
        ANF_to_MIR.toMIRFunctionsOnly
            anfProgram
            typeMap
            funcParams
            variantLookup
            recordRegistry
            false
            externalReturnTypes
    match mirResult with
    | Error err -> Error $"MIR conversion error: {err}"
    | Ok (mirFuncs, variantRegistry, mirRecordRegistry) ->
        let mirProgram = MIR.Program (mirFuncs, variantRegistry, mirRecordRegistry)
        if shouldDumpIR verbosity options.DumpMIR then
            printMIRProgram "=== MIR (Control Flow Graph) ===" mirProgram
        if verbosity >= 2 then
            let t = System.Math.Round(sw.Elapsed.TotalMilliseconds - mirStart, 1)
            println $"        {t}ms"
        compileMirToLir verbosity options sw stageSuffix mirProgram

/// Allocate registers for a list of symbolic LIR functions
let private allocateRegistersForFunctions
    (functions: LIRSymbolic.Function list)
    : LIRSymbolic.Function list =
    functions |> List.map RegisterAllocation.allocateRegisters

type private AnfPipelineResult = {
    Program: ANF.Program
    TypeMap: ANF.TypeMap
    ConvResult: AST_to_ANF.ConversionResult
}

/// Run ANF optimization + RC/TCO, returning a final ANF program and type map
let private buildAnfProgramCore
    (verbosity: int)
    (options: CompilerOptions)
    (sw: Stopwatch)
    (userFunctions: ANF.Function list)
    (mainExpr: ANF.AExpr)
    (userOnly: AST_to_ANF.UserOnlyResult)
    : Result<AnfPipelineResult, string> =

    let anfOptions = buildANFOptimizeOptions options
    let anfPassLabel =
        formatPassGroup
            "ANF Optimizations"
            [
                ("const_folding", anfOptions.EnableConstFolding)
                ("const_prop", anfOptions.EnableConstProp)
                ("copy_prop", anfOptions.EnableCopyProp)
                ("dce", anfOptions.EnableDCE)
                ("strength_reduction", anfOptions.EnableStrengthReduction)
            ]
    if verbosity >= 1 then println $"  [2.3/8] {anfPassLabel}..."
    let anfProgram = ANF.Program (userFunctions, mainExpr)
    if shouldDumpIR verbosity options.DumpANF then
        printANFProgram "=== ANF (before optimization) ===" anfProgram
    let anfOptStart = sw.Elapsed.TotalMilliseconds
    let anfOptimized =
        if shouldRunANFOptimize anfOptions then
            ANF_Optimize.optimizeProgramWithOptions anfOptions anfProgram
        else
            anfProgram
    if verbosity >= 2 then
        let t = System.Math.Round(sw.Elapsed.TotalMilliseconds - anfOptStart, 1)
        println $"        {t}ms"
    if shouldDumpIR verbosity options.DumpANF then
        printANFProgram "=== ANF (after optimization) ===" anfOptimized

    if verbosity >= 1 then println "  [2.4/8] ANF Inlining..."
    let inlineStart = sw.Elapsed.TotalMilliseconds
    let anfInlined =
        if options.DisableInlining then anfOptimized
        else ANF_Inlining.inlineProgramDefault anfOptimized
    if verbosity >= 2 then
        let t = System.Math.Round(sw.Elapsed.TotalMilliseconds - inlineStart, 1)
        println $"        {t}ms"

    let convResult : AST_to_ANF.ConversionResult = {
        Program = anfInlined
        TypeReg = userOnly.TypeReg
        VariantLookup = userOnly.VariantLookup
        FuncReg = userOnly.FuncReg
        FuncParams = userOnly.FuncParams
        ModuleRegistry = userOnly.ModuleRegistry
    }

    if verbosity >= 1 then println "  [2.5/8] Reference Count Insertion..."
    let rcStart = sw.Elapsed.TotalMilliseconds
    match RefCountInsertion.insertRCInProgram convResult with
    | Error err -> Error $"Reference count insertion error: {err}"
    | Ok (anfAfterRC, typeMap) ->
        if verbosity >= 2 then
            let t = System.Math.Round(sw.Elapsed.TotalMilliseconds - rcStart, 1)
            println $"        {t}ms"
        if shouldDumpIR verbosity options.DumpANF then
            printANFProgram "=== ANF (after RC insertion) ===" anfAfterRC

        if verbosity >= 1 then println "  [2.7/8] Tail Call Detection..."
        let tcoStart = sw.Elapsed.TotalMilliseconds
        let anfAfterTCO =
            if options.DisableTCO then anfAfterRC
            else TailCallDetection.detectTailCallsInProgram anfAfterRC
        if verbosity >= 2 then
            let t = System.Math.Round(sw.Elapsed.TotalMilliseconds - tcoStart, 1)
            println $"        {t}ms"
        if shouldDumpIR verbosity options.DumpANF then
            printANFProgram "=== ANF (after Tail Call Detection) ===" anfAfterTCO

        Ok { Program = anfAfterTCO; TypeMap = typeMap; ConvResult = convResult }

/// Run ANF optimization + RC/TCO/print insertion, returning a final ANF program and type map
let private buildAnfProgram
    (verbosity: int)
    (options: CompilerOptions)
    (sw: Stopwatch)
    (programType: AST.Type)
    (userFunctions: ANF.Function list)
    (mainExpr: ANF.AExpr)
    (userOnly: AST_to_ANF.UserOnlyResult)
    (extraFunctions: ANF.Function list)
    (extraTypeMap: ANF.TypeMap)
    : Result<ANF.Program * ANF.TypeMap * AST_to_ANF.ConversionResult, string> =

    match buildAnfProgramCore verbosity options sw userFunctions mainExpr userOnly with
    | Error err -> Error err
    | Ok pipeline ->
        let (ANF.Program (postTcoFunctions, postTcoMain)) = pipeline.Program
        let mergedFunctions = extraFunctions @ postTcoFunctions
        let mergedTypeMap = Map.fold (fun acc k v -> Map.add k v acc) extraTypeMap pipeline.TypeMap

        if verbosity >= 1 then println "  [2.8/8] Print Insertion..."
        let printStart = sw.Elapsed.TotalMilliseconds
        let mergedProgram =
            PrintInsertion.insertPrint mergedFunctions postTcoMain programType
        if verbosity >= 2 then
            let t = System.Math.Round(sw.Elapsed.TotalMilliseconds - printStart, 1)
            println $"        {t}ms"
        if shouldDumpIR verbosity options.DumpANF then
            printANFProgram "=== ANF (after Print insertion) ===" mergedProgram
        Ok (mergedProgram, mergedTypeMap, pipeline.ConvResult)

/// Run codegen, encoding, and binary generation
let private generateBinary
    (verbosity: int)
    (options: CompilerOptions)
    (sw: Stopwatch)
    (codegenLabel: string)
    (encodeLabel: string)
    (binaryLabel: string)
    (dumpAsm: bool)
    (dumpMachineCode: bool)
    (allocatedProgram: LIR.Program)
    : Result<byte array, string> =

    if verbosity >= 1 then println codegenLabel
    let codegenStart = sw.Elapsed.TotalMilliseconds
    let coverageExprCount = if options.EnableCoverage then LIR.countCoverageHits allocatedProgram else 0
    let codegenOptions : CodeGen.CodeGenOptions = {
        DisableFreeList = options.DisableFreeList
        EnableCoverage = options.EnableCoverage
        CoverageExprCount = coverageExprCount
        EnableLeakCheck = options.EnableLeakCheck
    }
    let codegenResult = CodeGen.generateARM64WithOptions codegenOptions allocatedProgram
    match codegenResult with
    | Error err -> Error $"Code generation error: {err}"
    | Ok arm64Instructions ->
        if verbosity >= 2 then
            let t = System.Math.Round(sw.Elapsed.TotalMilliseconds - codegenStart, 1)
            println $"        {t}ms"

        if dumpAsm && verbosity >= 3 then
            println "=== ARM64 Assembly Instructions ==="
            for (i, instr) in List.indexed arm64Instructions do
                println $"  {i}: {instr}"
            println ""

        match Platform.detectOS () with
        | Error err -> Error $"Platform detection error: {err}"
        | Ok os ->
            if verbosity >= 1 then println encodeLabel
            let encodeStart = sw.Elapsed.TotalMilliseconds
            let (LIR.Program (_, stringPool, floatPool)) = allocatedProgram
            let machineCode =
                ARM64_Encoding.encodeAllWithPools arm64Instructions stringPool floatPool os options.EnableLeakCheck
            if verbosity >= 2 then
                let t = System.Math.Round(sw.Elapsed.TotalMilliseconds - encodeStart, 1)
                println $"        {t}ms"

            if dumpMachineCode && verbosity >= 3 then
                println "=== Machine Code (hex) ==="
                for i in 0 .. 4 .. (machineCode.Length - 1) do
                    if i + 3 < machineCode.Length then
                        let bytes = sprintf "%02x %02x %02x %02x" machineCode.[i] machineCode.[i+1] machineCode.[i+2] machineCode.[i+3]
                        println $"  {i:X4}: {bytes}"
                println $"Total: {machineCode.Length} bytes\n"

            let formatName = match os with | Platform.MacOS -> "Mach-O" | Platform.Linux -> "ELF"
            if verbosity >= 1 then println (binaryLabel.Replace("{format}", formatName))
            let binaryStart = sw.Elapsed.TotalMilliseconds
            let binary =
                match os with
                | Platform.MacOS -> Binary_Generation_MachO.createExecutableWithPools machineCode stringPool floatPool options.EnableLeakCheck
                | Platform.Linux -> Binary_Generation_ELF.createExecutableWithPools machineCode stringPool floatPool options.EnableLeakCheck
            if verbosity >= 2 then
                let t = System.Math.Round(sw.Elapsed.TotalMilliseconds - binaryStart, 1)
                println $"        {t}ms"

            Ok binary


/// Compiled preamble context - extends stdlib for a test file
/// Preamble functions are compiled ONCE per file, then reused for all tests in that file
type PreambleContext = {
    /// Extended type checking environment (stdlib + preamble types/functions)
    TypeCheckEnv: TypeChecking.TypeCheckEnv
    /// Preamble's generic function definitions for monomorphization
    GenericFuncDefs: AST_to_ANF.GenericFuncDefs
    /// Preamble's ANF functions (after mono, inline, lift, ANF, RC, TCO)
    ANFFunctions: ANF.Function list
    /// Type map from RC insertion (merged with stdlib's TypeMap)
    TypeMap: ANF.TypeMap
    /// Preamble's registries for ANF conversion
    ANFResult: AST_to_ANF.ConversionResult
    /// Preamble's symbolic LIR functions after register allocation
    SymbolicFunctions: LIRSymbolic.Function list
}

/// Result of compiling stdlib - can be reused across compilations
type StdlibResult = {
    /// Parsed stdlib AST (for merging with user AST)
    AST: AST.Program
    /// Type-checked stdlib with inferred types
    TypedAST: AST.Program
    /// Type checking environment (registries for types, variants, functions)
    TypeCheckEnv: TypeChecking.TypeCheckEnv
    /// ANF conversion result
    ANFResult: AST_to_ANF.ConversionResult
    /// Generic function definitions for on-demand monomorphization
    /// (e.g., Stdlib.List.length<t> needs to be specialized when user calls it with Int64)
    GenericFuncDefs: AST_to_ANF.GenericFuncDefs
    /// Module registry for stdlib intrinsics (built once, reused)
    ModuleRegistry: AST.ModuleRegistry
    /// Precompiled LIR program (used for string/float pools)
    LIRProgram: LIR.Program
    /// Pre-allocated stdlib functions (physical registers assigned, ready for merge)
    AllocatedFunctions: LIR.Function list
    /// Call graph for dead code elimination (which stdlib funcs call which other funcs)
    StdlibCallGraph: Map<string, Set<string>>
    /// Stdlib ANF functions indexed by name (for coverage analysis)
    StdlibANFFunctions: Map<string, ANF.Function>
    /// Call graph at ANF level (for coverage analysis reachability)
    StdlibANFCallGraph: Map<string, Set<string>>
    /// TypeMap from RC insertion (needed for getReachableStdlibFunctions)
    StdlibTypeMap: ANF.TypeMap
}


// Helper functions for exception-to-Result conversion (Darklang compatibility)

/// Extract return types from a FuncReg (FunctionRegistry maps func name -> full type)
/// This is needed because buildReturnTypeReg only includes functions in the current program,
/// but we need return types for all callable functions (including stdlib)
let extractReturnTypes (funcReg: Map<string, AST.Type>) : Map<string, AST.Type> =
    funcReg
    |> Map.toSeq
    |> Seq.choose (fun (name, typ) ->
        match typ with
        | AST.TFunction (_, retType) -> Some (name, retType)
        | other -> Crash.crash $"extractReturnTypes: Non-function type '{other}' found in FuncReg for '{name}'")
    |> Map.ofSeq

/// Try to delete a file, ignoring any errors
let tryDeleteFile (path: string) : unit =
    try File.Delete(path) with _ -> ()

/// Try to start a process, returning Result instead of throwing
let tryStartProcess (info: ProcessStartInfo) : Result<Process, string> =
    try Ok (Process.Start(info))
    with ex -> Error ex.Message

/// Locate a .dark file from possible paths
let tryFindDarkFile (filename: string) : string option =
    let exePath = Assembly.GetExecutingAssembly().Location
    let exeDir = Path.GetDirectoryName(exePath)
    let possiblePaths = [
        Path.Combine(exeDir, filename)
        Path.Combine(exeDir, "..", "..", "..", "..", "src", "DarkCompiler", filename)
        Path.Combine(Environment.CurrentDirectory, "src", "DarkCompiler", filename)
    ]
    possiblePaths |> List.tryFind File.Exists

/// Load a .dark file from possible paths
let loadDarkFile (filename: string) : Result<AST.Program, string> =
    let exePath = Assembly.GetExecutingAssembly().Location
    let exeDir = Path.GetDirectoryName(exePath)
    let possiblePaths = [
        Path.Combine(exeDir, filename)
        Path.Combine(exeDir, "..", "..", "..", "..", "src", "DarkCompiler", filename)
        Path.Combine(Environment.CurrentDirectory, "src", "DarkCompiler", filename)
    ]
    let filePath = possiblePaths |> List.tryFind File.Exists
    match filePath with
    | None ->
        let pathsStr = String.Join(", ", possiblePaths)
        Error $"Could not find {filename} in any of: {pathsStr}"
    | Some path ->
        let source = File.ReadAllText(path)
        Parser.parseString source
        |> Result.mapError (fun err -> $"Error parsing {filename}: {err}")

/// Load a .dark file allowing internal identifiers (for stdlib sources)
let loadDarkFileAllowInternal (filename: string) : Result<AST.Program, string> =
    let exePath = Assembly.GetExecutingAssembly().Location
    let exeDir = Path.GetDirectoryName(exePath)
    let possiblePaths = [
        Path.Combine(exeDir, filename)
        Path.Combine(exeDir, "..", "..", "..", "..", "src", "DarkCompiler", filename)
        Path.Combine(Environment.CurrentDirectory, "src", "DarkCompiler", filename)
    ]
    let filePath = possiblePaths |> List.tryFind File.Exists
    match filePath with
    | None ->
        let pathsStr = String.Join(", ", possiblePaths)
        Error $"Could not find {filename} in any of: {pathsStr}"
    | Some path ->
        let source = File.ReadAllText(path)
        Parser.parseStringAllowInternal source
        |> Result.mapError (fun err -> $"Error parsing {filename}: {err}")

/// Load the stdlib and unicode_data.dark files
/// Returns the merged stdlib AST or an error message
let loadStdlib () : Result<AST.Program, string> =
    let stdlibFiles = [
        "stdlib/Int8.dark"
        "stdlib/Int16.dark"
        "stdlib/Int32.dark"
        "stdlib/Int64.dark"
        "stdlib/UInt8.dark"
        "stdlib/UInt16.dark"
        "stdlib/UInt32.dark"
        "stdlib/UInt64.dark"
        "stdlib/Bool.dark"
        "stdlib/Tuple2.dark"
        "stdlib/Tuple3.dark"
        "stdlib/Result.dark"
        "stdlib/Option.dark"
        "stdlib/List.dark"
        "stdlib/Float.dark"
        "stdlib/Path.dark"
        "stdlib/Platform.dark"
        "stdlib/String.dark"
        "stdlib/__Hash.dark"
        "stdlib/Dict.dark"
        "stdlib/__HAMT.dark"
        "stdlib/Uuid.dark"
        "stdlib/Date.dark"
        "stdlib/Bytes.dark"
        "stdlib/Char.dark"
        "stdlib/Base64.dark"
        "stdlib/Crypto.dark"
        "stdlib/Math.dark"
        "stdlib/__FingerTree.dark"
    ]
    let loadStdlibFiles (filenames: string list) : Result<AST.Program, string> =
        let mergeFile (acc: AST.TopLevel list) (filename: string) : Result<AST.TopLevel list, string> =
            match loadDarkFileAllowInternal filename with
            | Error err -> Error err
            | Ok (AST.Program items) -> Ok (acc @ items)
        match List.fold (fun acc filename -> Result.bind (fun items -> mergeFile items filename) acc) (Ok []) filenames with
        | Error err -> Error err
        | Ok items -> Ok (AST.Program items)

    let stdlibResult = loadStdlibFiles stdlibFiles

    match stdlibResult with
    | Error e -> Error e
    | Ok (AST.Program stdlibItems) ->
        // Try to load unicode_data.dark (optional, may not exist in all environments)
        match loadDarkFileAllowInternal "unicode_data.dark" with
        | Error _ ->
            // Unicode data not available, return stdlib only
            Ok (AST.Program stdlibItems)
        | Ok (AST.Program unicodeItems) ->
            // Merge stdlib and unicode data
            Ok (AST.Program (stdlibItems @ unicodeItems))

/// Merge typed stdlib with typed user program for ANF conversion
/// Excludes stdlib's dummy expression - only user's main expression is kept
let mergeTypedPrograms (stdlibTyped: AST.Program) (userTyped: AST.Program) : AST.Program =
    let (AST.Program stdlibItems) = stdlibTyped
    let (AST.Program userItems) = userTyped
    // Filter out any Expression items from stdlib (dummy main)
    let stdlibDefsOnly = stdlibItems |> List.filter (function AST.Expression _ -> false | _ -> true)
    AST.Program (stdlibDefsOnly @ userItems)

/// Compile stdlib in isolation, returning reusable result
/// This can be called once and the result reused for multiple user program compilations
let compileStdlib () : Result<StdlibResult, string> =
    emit "stdlib.compile.start" []
    match loadStdlib() with
    | Error e ->
        emit "stdlib.compile.error" [("message", e)]
        Error e
    | Ok stdlibAst ->
        // Add dummy main expression for type checking (stdlib has no main)
        let (AST.Program items) = stdlibAst
        let withMain = AST.Program (items @ [AST.Expression AST.UnitLiteral])

        match TypeChecking.checkProgramWithEnv withMain with
        | Error e ->
            let msg = TypeChecking.typeErrorToString e
            emit "stdlib.compile.error" [("message", msg)]
            Error msg
        | Ok (_, typedStdlib, typeCheckEnv) ->
            // Extract generic function definitions for on-demand monomorphization
            let genericFuncDefs = AST_to_ANF.extractGenericFuncDefs typedStdlib
            // Build module registry once (reused across all compilations)
            let moduleRegistry = Stdlib.buildModuleRegistry ()
            match AST_to_ANF.convertProgramWithTypes typedStdlib with
            | Error e ->
                emit "stdlib.compile.error" [("message", e)]
                Error e
            | Ok anfResult ->
                // Run RC insertion on stdlib ANF (stdlib functions need ref counting too)
                match RefCountInsertion.insertRCInProgram anfResult with
                | Error e ->
                    emit "stdlib.compile.error" [("message", e)]
                    Error e
                | Ok (anfAfterRC, typeMap) ->
                    // Pass 2.7: Tail Call Detection
                    let anfAfterTCO = TailCallDetection.detectTailCallsInProgram anfAfterRC
                    // Extract stdlib ANF functions for coverage analysis
                    let (ANF.Program (stdlibFuncs, _)) = anfAfterTCO
                    let stdlibFuncMap =
                        stdlibFuncs
                        |> List.map (fun f -> f.Name, f)
                        |> Map.ofList
                    // Build ANF-level call graph for coverage analysis
                    let stdlibANFCallGraph = ANFDeadCodeElimination.buildCallGraph stdlibFuncs

                    // Compile stdlib functions through shared MIR/LIR pipeline
                    let externalReturnTypes = extractReturnTypes anfResult.FuncReg
                    let stdlibOptions = defaultOptions
                    let sw = Stopwatch.StartNew()
                    match compileAnfFunctionsOnlyToLir
                        0
                        stdlibOptions
                        sw
                        "stdlib"
                        anfAfterTCO
                        typeMap
                        anfResult.FuncParams
                        anfResult.VariantLookup
                        Map.empty
                        externalReturnTypes with
                    | Error e ->
                        emit "stdlib.compile.error" [("message", e)]
                        Error e
                    | Ok optimizedLirProgram ->
                        let (LIRSymbolic.Program lirFuncs) = optimizedLirProgram
                        let allocatedFuncs = allocateRegistersForFunctions lirFuncs
                        let allocatedSymbolic = LIRSymbolic.Program allocatedFuncs
                        match LIRSymbolic.toLIR allocatedSymbolic with
                        | Error err ->
                            emit "stdlib.compile.error" [("message", err)]
                            Error err
                        | Ok allocatedProgram ->
                            let (LIR.Program (resolvedFuncs, _, _)) = allocatedProgram
                            // Build call graph for dead code elimination
                            let stdlibCallGraph = DeadCodeElimination.buildCallGraph resolvedFuncs
                            emit "stdlib.compile.finish" [("functions", string resolvedFuncs.Length)]
                            Ok {
                                AST = stdlibAst
                                TypedAST = typedStdlib
                                TypeCheckEnv = typeCheckEnv
                                ANFResult = anfResult
                                GenericFuncDefs = genericFuncDefs
                                ModuleRegistry = moduleRegistry
                                LIRProgram = allocatedProgram
                                AllocatedFunctions = resolvedFuncs
                                StdlibCallGraph = stdlibCallGraph
                                StdlibANFFunctions = stdlibFuncMap
                                StdlibANFCallGraph = stdlibANFCallGraph
                                StdlibTypeMap = typeMap
                            }

let private parseSourceWithInternal (allowInternal: bool) (source: string) : Result<AST.Program, string> =
    if allowInternal then
        Parser.parseStringAllowInternal source
    else
        Parser.parseString source

type private UserCompileLabels = {
    Parse: string
    TypeCheck: string
    Anf: string
    StageSuffix: string
}

type private UserCompilePlan = {
    AllowInternal: bool
    Verbosity: int
    Options: CompilerOptions
    Stdlib: StdlibResult
    BaseTypeCheckEnv: TypeChecking.TypeCheckEnv
    BaseGenericFuncDefs: AST_to_ANF.GenericFuncDefs
    BaseANFResult: AST_to_ANF.ConversionResult
    PrebuiltSymbolicFunctions: LIRSymbolic.Function list
    SkipFunctionNames: Set<string>
    EmitFunctionEvents: bool
    TreeShakeUserFunctions: bool
    Labels: UserCompileLabels
    SourceFile: string
    Source: string
}

/// Compile a user/test program against a prebuilt stdlib/preamble context
let private compileUserWithPlan (plan: UserCompilePlan) : CompileResult =
    let sw = Stopwatch.StartNew()
    try
        // Pass 1: Parse user code only
        if plan.Verbosity >= 1 then println plan.Labels.Parse
        let parseResult = parseSourceWithInternal plan.AllowInternal plan.Source
        let parseTime = sw.Elapsed.TotalMilliseconds
        if plan.Verbosity >= 2 then
            let t = System.Math.Round(parseTime, 1)
            println $"        {t}ms"

        match parseResult with
        | Error err -> Error $"Parse error: {err}"
        | Ok userAst ->
            // Pass 1.5: Type Checking (user code with base TypeCheckEnv)
            if plan.Verbosity >= 1 then println plan.Labels.TypeCheck
            let typeCheckResult = TypeChecking.checkProgramWithBaseEnv plan.BaseTypeCheckEnv userAst
            let typeCheckTime = sw.Elapsed.TotalMilliseconds - parseTime
            if plan.Verbosity >= 2 then
                let t = System.Math.Round(typeCheckTime, 1)
                println $"        {t}ms"

            match typeCheckResult with
            | Error typeErr -> Error $"Type error: {TypeChecking.typeErrorToString typeErr}"
            | Ok (programType, typedUserAst, _userEnv) ->
                if plan.Verbosity >= 3 then
                    println $"Program type: {TypeChecking.typeToString programType}"
                    println ""

                // Pass 2: AST → ANF (user only)
                if plan.Verbosity >= 1 then println plan.Labels.Anf
                let userOnlyResult =
                    AST_to_ANF.convertUserOnly
                        plan.BaseGenericFuncDefs
                        plan.BaseANFResult
                        typedUserAst
                let anfTime = sw.Elapsed.TotalMilliseconds - parseTime - typeCheckTime
                if plan.Verbosity >= 2 then
                    let t = System.Math.Round(anfTime, 1)
                    println $"        {t}ms"

                match userOnlyResult with
                | Error err -> Error $"ANF conversion error: {err}"
                | Ok userOnly ->
                    let functionsToCompile =
                        userOnly.UserFunctions
                        |> List.filter (fun f -> not (Set.contains f.Name plan.SkipFunctionNames))

                    if plan.EmitFunctionEvents && isEnabled () then
                        emit "test.compile.functions" [("source", plan.SourceFile); ("count", string functionsToCompile.Length)]
                        for func in functionsToCompile do
                            emit "test.compile.function" [("source", plan.SourceFile); ("function", func.Name)]

                    if plan.EmitFunctionEvents && plan.Verbosity >= 3 then
                        println $"  [COMPILE] {functionsToCompile.Length} user functions compiled fresh"
                        for f in functionsToCompile do
                            println $"    - {f.Name}"

                    let anfResult =
                        buildAnfProgram
                            plan.Verbosity
                            plan.Options
                            sw
                            programType
                            functionsToCompile
                            userOnly.MainExpr
                            userOnly
                            []
                            Map.empty
                    match anfResult with
                    | Error err -> Error err
                    | Ok (userAnfProgram, typeMap, userConvResult) ->
                        let externalReturnTypes = extractReturnTypes userOnly.FuncReg
                        let userLirResult =
                            compileAnfToLir
                                plan.Verbosity
                                plan.Options
                                sw
                                plan.Labels.StageSuffix
                                userAnfProgram
                                typeMap
                                userConvResult
                                programType
                                externalReturnTypes
                        match userLirResult with
                        | Error err -> Error err
                        | Ok userOptimizedLirProgram ->
                            // Pass 5: Register Allocation
                            if plan.Verbosity >= 1 then println "  [5/8] Register Allocation..."
                            let allocStart = sw.Elapsed.TotalMilliseconds
                            let (LIRSymbolic.Program userFuncs) = userOptimizedLirProgram
                            let (LIR.Program (_, stdlibStrings, stdlibFloats)) = plan.Stdlib.LIRProgram

                            let allocatedUserFuncs = allocateRegistersForFunctions userFuncs
                            let allSymbolicUserFuncs = plan.PrebuiltSymbolicFunctions @ allocatedUserFuncs

                            match LIRSymbolic.toLIRWithPools stdlibStrings stdlibFloats allSymbolicUserFuncs with
                            | Error err -> Error $"LIR pool resolution error: {err}"
                            | Ok (LIR.Program (allUserFuncs, mergedStrings, mergedFloats)) ->
                                if plan.Verbosity >= 2 then
                                    let t = System.Math.Round(sw.Elapsed.TotalMilliseconds - allocStart, 1)
                                    println $"        {t}ms"

                                let finalUserFuncs =
                                    if plan.TreeShakeUserFunctions then
                                        if plan.Verbosity >= 1 then println "  [5.5/8] Function Tree Shaking..."
                                        if plan.Options.DisableFunctionTreeShaking then allUserFuncs
                                        else FunctionTreeShaking.filterUserFunctions allUserFuncs
                                    else
                                        allUserFuncs

                                if plan.EmitFunctionEvents && plan.Verbosity >= 3 then
                                    println $"  [COMBINED] fresh: {allocatedUserFuncs.Length}, total: {allUserFuncs.Length}"
                                    for f in allUserFuncs do
                                        println $"    - {f.Name}"
                                    println $"  [TreeShaking] user funcs: {finalUserFuncs.Length}"

                                // Filter stdlib functions to only include reachable ones (dead code elimination)
                                let reachableStdlib =
                                    if plan.Options.DisableFunctionTreeShaking then plan.Stdlib.AllocatedFunctions
                                    else
                                        FunctionTreeShaking.filterStdlibFunctions
                                            plan.Stdlib.StdlibCallGraph
                                            finalUserFuncs
                                            plan.Stdlib.AllocatedFunctions

                                // Combine reachable stdlib functions with user functions
                                let allFuncs = reachableStdlib @ finalUserFuncs
                                let allocatedProgram = LIR.Program (allFuncs, mergedStrings, mergedFloats)
                                if shouldDumpIR plan.Verbosity plan.Options.DumpLIR then
                                    printLIRProgram "=== LIR (After Register Allocation) ===" allocatedProgram

                                let binaryResult =
                                    generateBinary
                                        plan.Verbosity
                                        plan.Options
                                        sw
                                        "  [6/8] Code Generation..."
                                        "  [7/7] ARM64 Encoding..."
                                        "  [7/7] Binary Generation ({format})..."
                                        false
                                        false
                                        allocatedProgram
                                match binaryResult with
                                | Error err -> Error err
                                | Ok binary ->
                                    sw.Stop()
                                    if plan.Verbosity >= 1 then
                                        println $"  ✓ Compilation complete ({System.Math.Round(sw.Elapsed.TotalMilliseconds, 1)}ms)"
                                    Ok binary
    with
    | ex ->
        Error $"Compilation failed: {ex.Message}"

/// Compile preamble with stdlib as base, returning extended context for test compilation
/// Preamble functions go through the full pipeline (parse → typecheck → mono → inline → lift → ANF → RC → TCO)
/// The result is built once per file and reused for all tests in that file
let compilePreambleWithOptions (allowInternal: bool) (stdlib: StdlibResult) (preamble: string) (sourceFile: string) (funcLineMap: Map<string, int>) : Result<PreambleContext, string> =
    emit "preamble.compile.start" [("source", sourceFile); ("length", string preamble.Length)]
    // Handle empty preamble - return a context that just wraps stdlib
    if String.IsNullOrWhiteSpace(preamble) then
        let emptyContext = {
            TypeCheckEnv = stdlib.TypeCheckEnv
            GenericFuncDefs = stdlib.GenericFuncDefs
            ANFFunctions = []
            TypeMap = stdlib.StdlibTypeMap
            ANFResult = stdlib.ANFResult
            SymbolicFunctions = []
        }
        emit "preamble.compile.finish" [("source", sourceFile); ("functions", "0")]
        Ok emptyContext
    else
        // Parse preamble with dummy expression (parser requires a main expression)
        let preambleSource = preamble + "\n0"
        match parseSourceWithInternal allowInternal preambleSource with
        | Error err ->
            let msg = $"Preamble parse error: {err}"
            emit "preamble.compile.error" [("source", sourceFile); ("message", msg)]
            Error msg
        | Ok preambleAst ->
            // Type-check preamble with stdlib.TypeCheckEnv
            match TypeChecking.checkProgramWithBaseEnv stdlib.TypeCheckEnv preambleAst with
            | Error typeErr ->
                let msg = $"Preamble type error: {TypeChecking.typeErrorToString typeErr}"
                emit "preamble.compile.error" [("source", sourceFile); ("message", msg)]
                Error msg
            | Ok (_programType, typedPreambleAst, preambleTypeCheckEnv) ->
                // Extract generic function definitions from preamble
                let preambleGenericDefs = AST_to_ANF.extractGenericFuncDefs typedPreambleAst
                // Merge stdlib generics with preamble generics
                let mergedGenericDefs = Map.fold (fun acc k v -> Map.add k v acc) stdlib.GenericFuncDefs preambleGenericDefs

                // Convert preamble to ANF (mono → inline → lift → ANF)
                match AST_to_ANF.convertUserOnly mergedGenericDefs stdlib.ANFResult typedPreambleAst with
                | Error err ->
                    let msg = $"Preamble ANF conversion error: {err}"
                    emit "preamble.compile.error" [("source", sourceFile); ("message", msg)]
                    Error msg
                | Ok preambleUserOnly ->
                    let preambleOptions = defaultOptions
                    let anfSw = Stopwatch.StartNew()
                    match buildAnfProgramCore 0 preambleOptions anfSw preambleUserOnly.UserFunctions preambleUserOnly.MainExpr preambleUserOnly with
                    | Error err ->
                        let rcPrefix = "Reference count insertion error: "
                        let msg =
                            if err.StartsWith(rcPrefix) then
                                let suffix = err.Substring(rcPrefix.Length)
                                $"Preamble RC insertion error: {suffix}"
                            else
                                $"Preamble {err}"
                        emit "preamble.compile.error" [("source", sourceFile); ("message", msg)]
                        Error msg
                    | Ok pipeline ->
                        // Extract just the functions (ignore main expr which is dummy `0`)
                        let (ANF.Program (preambleFunctions, _dummyMainExpr)) = pipeline.Program

                        let preambleConvResult = pipeline.ConvResult
                        let preambleExternalReturnTypes = extractReturnTypes preambleConvResult.FuncReg
                        let sw = Stopwatch.StartNew()
                        match compileAnfFunctionsOnlyToLir
                            0
                            preambleOptions
                            sw
                            "preamble"
                            pipeline.Program
                            pipeline.TypeMap
                            preambleConvResult.FuncParams
                            preambleConvResult.VariantLookup
                            Map.empty
                            preambleExternalReturnTypes with
                        | Error err ->
                            let msg = $"Preamble {err}"
                            emit "preamble.compile.error" [("source", sourceFile); ("message", msg)]
                            Error msg
                        | Ok optimizedLir ->
                            let (LIRSymbolic.Program lirFuncs) = optimizedLir
                            let stdlibFuncNames =
                                stdlib.AllocatedFunctions
                                |> List.map (fun func -> func.Name)
                                |> Set.ofList
                            let isStdlibFunction (name: string) : bool =
                                Set.contains name stdlibFuncNames
                            let allocatedFuncs: LIRSymbolic.Function list =
                                allocateRegistersForFunctions lirFuncs
                            let preambleOnlyFuncs =
                                allocatedFuncs
                                |> List.filter (fun func -> not (isStdlibFunction func.Name))
                            let preambleSymbolicFuncs = preambleOnlyFuncs

                            // Merge TypeMaps (stdlib + preamble)
                            let mergedTypeMap = Map.fold (fun acc k v -> Map.add k v acc) stdlib.StdlibTypeMap pipeline.TypeMap

                            let context = {
                                TypeCheckEnv = preambleTypeCheckEnv
                                GenericFuncDefs = mergedGenericDefs
                                ANFFunctions = preambleFunctions
                                TypeMap = mergedTypeMap
                                ANFResult = preambleConvResult
                                SymbolicFunctions = preambleSymbolicFuncs
                            }
                            emit "preamble.compile.finish" [("source", sourceFile); ("functions", string preambleSymbolicFuncs.Length)]
                            Ok context

/// Compile preamble with stdlib as base (user code defaults)
let compilePreamble (stdlib: StdlibResult) (preamble: string) (sourceFile: string) (funcLineMap: Map<string, int>) : Result<PreambleContext, string> =
    compilePreambleWithOptions false stdlib preamble sourceFile funcLineMap

/// Compile test expression with a prebuilt preamble context
/// Only the tiny test expression is parsed/compiled - preamble functions are merged in
let compileTestWithPreambleWithOptions (allowInternal: bool) (verbosity: int) (options: CompilerOptions) (stdlib: StdlibResult)
                                       (preambleCtx: PreambleContext) (sourceFile: string) (testExpr: string) : CompileResult =
    let preambleFuncs = preambleCtx.SymbolicFunctions
    let preambleFuncNameSet =
        preambleFuncs |> List.map (fun f -> f.Name) |> Set.ofList
    let labels = {
        Parse = "  [1/8] Parse (test expr only)..."
        TypeCheck = "  [1.5/8] Type Checking (with preamble env)..."
        Anf = "  [2/8] AST → ANF (test expr only)..."
        StageSuffix = ""
    }
    let plan = {
        AllowInternal = allowInternal
        Verbosity = verbosity
        Options = options
        Stdlib = stdlib
        BaseTypeCheckEnv = preambleCtx.TypeCheckEnv
        BaseGenericFuncDefs = preambleCtx.GenericFuncDefs
        BaseANFResult = preambleCtx.ANFResult
        PrebuiltSymbolicFunctions = preambleFuncs
        SkipFunctionNames = preambleFuncNameSet
        EmitFunctionEvents = true
        TreeShakeUserFunctions = true
        Labels = labels
        SourceFile = sourceFile
        Source = testExpr
    }
    compileUserWithPlan plan

/// Compile test expression with a prebuilt preamble context (user code defaults)
let compileTestWithPreamble (verbosity: int) (options: CompilerOptions) (stdlib: StdlibResult)
                            (preambleCtx: PreambleContext) (sourceFile: string) (testExpr: string) : CompileResult =
    compileTestWithPreambleWithOptions false verbosity options stdlib preambleCtx sourceFile testExpr

/// Compile user code with prebuilt stdlib and tree-shake unused functions
/// This keeps expensive passes focused on user code while reusing stdlib output
let private compileWithStdlib (verbosity: int) (options: CompilerOptions) (stdlib: StdlibResult) (source: string) : CompileResult =
    let labels = {
        Parse = "  [1/8] Parse..."
        TypeCheck = "  [1.5/8] Type Checking (with stdlib env)..."
        Anf = "  [2/8] AST → ANF (user only)..."
        StageSuffix = "user only"
    }
    let plan = {
        AllowInternal = false
        Verbosity = verbosity
        Options = options
        Stdlib = stdlib
        BaseTypeCheckEnv = stdlib.TypeCheckEnv
        BaseGenericFuncDefs = stdlib.GenericFuncDefs
        BaseANFResult = stdlib.ANFResult
        PrebuiltSymbolicFunctions = []
        SkipFunctionNames = Set.empty
        EmitFunctionEvents = false
        TreeShakeUserFunctions = false
        Labels = labels
        SourceFile = ""
        Source = source
    }
    compileUserWithPlan plan

/// Compile source code to binary (in-memory, no file I/O)
let compileWithOptions (verbosity: int) (options: CompilerOptions) (source: string) : CompileResult =
    match compileStdlib() with
    | Error err -> Error err
    | Ok stdlib ->
        compileWithStdlib verbosity options stdlib source

/// Compile source code to binary (uses default options)
/// Execute compiled binary and capture output
let private execute (verbosity: int) (binary: byte array) : ExecutionResult =
    let sw = Stopwatch.StartNew()

    if verbosity >= 1 then println ""
    if verbosity >= 1 then println "  Execution:"

    // Write binary to temp file
    if verbosity >= 1 then println "    • Writing binary to temp file..."
    let tempPath = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString("N"))

    // Write and flush to disk to minimize (but not eliminate) "Text file busy" race
    do
        use stream = new IO.FileStream(tempPath, IO.FileMode.Create, IO.FileAccess.Write, IO.FileShare.None)
        stream.Write(binary, 0, binary.Length)
        stream.Flush(true)  // Flush both stream and OS buffers to disk

    let writeTime = sw.Elapsed.TotalMilliseconds
    if verbosity >= 2 then println $"      {System.Math.Round(writeTime, 1)}ms"

    try
        // Make executable using Unix file mode
        if verbosity >= 1 then println "    • Setting executable permissions..."
        let permissions = File.GetUnixFileMode(tempPath)
        File.SetUnixFileMode(tempPath, permissions ||| IO.UnixFileMode.UserExecute)
        let chmodTime = sw.Elapsed.TotalMilliseconds - writeTime
        if verbosity >= 2 then println $"      {System.Math.Round(chmodTime, 1)}ms"

        // Code sign with adhoc signature (required for macOS only)
        let codesignResult =
            match Platform.detectOS () with
            | Error err ->
                // Platform detection failed
                Some $"Platform detection failed: {err}"
            | Ok os ->
                if Platform.requiresCodeSigning os then
                    if verbosity >= 1 then println "    • Code signing (adhoc)..."
                    let codesignStart = sw.Elapsed.TotalMilliseconds
                    let codesignInfo = ProcessStartInfo("codesign")
                    codesignInfo.Arguments <- $"-s - \"{tempPath}\""
                    codesignInfo.UseShellExecute <- false
                    codesignInfo.RedirectStandardOutput <- true
                    codesignInfo.RedirectStandardError <- true
                    let codesignProc = Process.Start(codesignInfo)
                    codesignProc.WaitForExit()

                    if codesignProc.ExitCode <> 0 then
                        let stderr = codesignProc.StandardError.ReadToEnd()
                        Some $"Code signing failed: {stderr}"
                    else
                        let codesignTime = sw.Elapsed.TotalMilliseconds - codesignStart
                        if verbosity >= 2 then println $"      {System.Math.Round(codesignTime, 1)}ms"
                        None
                else
                    if verbosity >= 1 then println "    • Code signing skipped (not required on Linux)"
                    None

        match codesignResult with
        | Some errorMsg ->
            // Code signing or platform detection failed - return error
            { ExitCode = -1
              Stdout = ""
              Stderr = errorMsg }
        | None ->
            // Execute (with retry for "Text file busy" race condition)
            // Even with flush, kernel may not have fully synced file/permissions in fast test runs
            if verbosity >= 1 then println "    • Running binary..."
            let execStart = sw.Elapsed.TotalMilliseconds
            let execInfo = ProcessStartInfo(tempPath)
            execInfo.RedirectStandardOutput <- true
            execInfo.RedirectStandardError <- true
            execInfo.UseShellExecute <- false

            // Retry up to 3 times with small delay if we get "Text file busy"
            let rec startWithRetry attempts =
                match tryStartProcess execInfo with
                | Ok proc -> Ok proc
                | Error msg when msg.Contains("Text file busy") && attempts > 0 ->
                    Threading.Thread.Sleep(10)  // Wait 10ms before retry
                    startWithRetry (attempts - 1)
                | Error msg -> Error msg

            match startWithRetry 3 with
            | Error msg ->
                { ExitCode = -1
                  Stdout = ""
                  Stderr = $"Failed to start process: {msg}" }
            | Ok execProc ->
                use proc = execProc
                // Start async reads immediately to avoid blocking
                let stdoutTask = proc.StandardOutput.ReadToEndAsync()
                let stderrTask = proc.StandardError.ReadToEndAsync()

                // Wait for process to complete
                proc.WaitForExit()

                // Now wait for output to be fully read
                let stdout = stdoutTask.Result
                let stderr = stderrTask.Result

                let execTime = sw.Elapsed.TotalMilliseconds - execStart
                if verbosity >= 2 then println $"      {System.Math.Round(execTime, 1)}ms"

                sw.Stop()

                if verbosity >= 1 then
                    println $"  ✓ Execution complete ({System.Math.Round(sw.Elapsed.TotalMilliseconds, 1)}ms)"

                { ExitCode = proc.ExitCode
                  Stdout = stdout
                  Stderr = stderr }
    finally
        // Cleanup - ignore deletion errors
        tryDeleteFile tempPath

let private compileResultToExecution (verbosity: int) (compileResult: CompileResult) : ExecutionResult =
    match compileResult with
    | Error err ->
        { ExitCode = 1
          Stdout = ""
          Stderr = err }
    | Ok binary ->
        execute verbosity binary

/// Compile and run source code with options
let compileAndRunWithOptions (verbosity: int) (options: CompilerOptions) (source: string) : ExecutionResult =
    compileWithOptions verbosity options source |> compileResultToExecution verbosity

/// Compile and run with timing breakdown using a prebuilt preamble context
let compileAndRunWithPreambleContextTimedWithOptions
    (allowInternal: bool)
    (verbosity: int)
    (options: CompilerOptions)
    (stdlib: StdlibResult)
    (preambleCtx: PreambleContext)
    (testExpr: string)
    (sourceFile: string)
    : TimedExecutionResult =
    let compileTimer = Stopwatch.StartNew()
    let compileResult =
        compileTestWithPreambleWithOptions allowInternal verbosity options stdlib preambleCtx sourceFile testExpr
    compileTimer.Stop()
    let compileTime = compileTimer.Elapsed

    match compileResult with
    | Error err ->
        { ExitCode = 1
          Stdout = ""
          Stderr = err
          CompileTime = compileTime
          RuntimeTime = TimeSpan.Zero }
    | Ok binary ->
        let runtimeTimer = Stopwatch.StartNew()
        let execResult = execute verbosity binary
        runtimeTimer.Stop()
        { ExitCode = execResult.ExitCode
          Stdout = execResult.Stdout
          Stderr = execResult.Stderr
          CompileTime = compileTime
          RuntimeTime = runtimeTimer.Elapsed }

/// Compile and run with timing breakdown, building a preamble context for each test
let compileAndRunWithPreambleTimedWithOptions (allowInternal: bool) (verbosity: int) (options: CompilerOptions) (stdlib: StdlibResult) (testExpr: string) (preamble: string) (sourceFile: string) (funcLineMap: Map<string, int>) : TimedExecutionResult =
    match compilePreambleWithOptions allowInternal stdlib preamble sourceFile funcLineMap with
    | Error err ->
        { ExitCode = 1
          Stdout = ""
          Stderr = err
          CompileTime = TimeSpan.Zero
          RuntimeTime = TimeSpan.Zero }
    | Ok preambleCtx ->
        compileAndRunWithPreambleContextTimedWithOptions allowInternal verbosity options stdlib preambleCtx testExpr sourceFile

/// Compile and run with timing breakdown, building a preamble context for each test
let compileAndRunWithPreambleTimed (verbosity: int) (options: CompilerOptions) (stdlib: StdlibResult) (testExpr: string) (preamble: string) (sourceFile: string) (funcLineMap: Map<string, int>) : TimedExecutionResult =
    compileAndRunWithPreambleTimedWithOptions false verbosity options stdlib testExpr preamble sourceFile funcLineMap

/// Get all stdlib function names from the prebuilt stdlib
let getAllStdlibFunctionNamesFromStdlib (stdlib: StdlibResult) : Set<string> =
    stdlib.StdlibANFFunctions |> Map.keys |> Set.ofSeq

/// Get the set of stdlib function names reachable from user code (using prebuilt stdlib)
/// Used for coverage analysis without re-compiling stdlib
let getReachableStdlibFunctionsFromStdlib (stdlib: StdlibResult) (source: string) : Result<Set<string>, string> =
    // Parse user code
    match Parser.parseString source with
    | Error err -> Error $"Parse error: {err}"
    | Ok userAst ->
        // Type check with stdlib environment
        match TypeChecking.checkProgramWithBaseEnv stdlib.TypeCheckEnv userAst with
        | Error typeErr -> Error $"Type error: {TypeChecking.typeErrorToString typeErr}"
        | Ok (programType, typedUserAst, _) ->
            // Convert to ANF
            match AST_to_ANF.convertUserOnly stdlib.GenericFuncDefs stdlib.ANFResult typedUserAst with
            | Error err -> Error $"ANF conversion error: {err}"
            | Ok userOnly ->
                let coverageOptions = { defaultOptions with DisableANFOpt = true; DisableInlining = true }
                let sw = Stopwatch.StartNew()
                match buildAnfProgram 0 coverageOptions sw programType userOnly.UserFunctions userOnly.MainExpr userOnly [] Map.empty with
                | Error err -> Error err
                | Ok (userAnfProgram, _typeMap, _userConvResult) ->
                    // Extract reachable stdlib functions using tree-shaking infrastructure
                    let reachableStdlibNames =
                        FunctionTreeShaking.getReachableStdlibNames stdlib.StdlibANFCallGraph userAnfProgram
                    Ok reachableStdlibNames
