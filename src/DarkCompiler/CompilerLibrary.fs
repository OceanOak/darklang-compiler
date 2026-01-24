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

/// Result of compilation with timing
type CompileReport = {
    Result: Result<byte array, string>
    CompileTime: TimeSpan
}

/// Timing for a single compiler pass
type PassTiming = {
    Pass: string
    Elapsed: TimeSpan
}

/// Recorder for compiler pass timings
type PassTimingRecorder = PassTiming -> unit

/// Cache hit/miss details for a compilation stage
type CacheMissInfo = {
    Stage: string
    Hits: int
    Misses: int
}

/// Recorder for cache hit/miss stats
type CacheMissRecorder = CacheMissInfo -> unit

/// Result of execution with timing
type ExecutionOutput = {
    ExitCode: int
    Stdout: string
    Stderr: string
    RuntimeTime: TimeSpan
}

/// Compilation mode for labeling and test behavior
type CompileMode =
    | FullProgram
    | TestExpression

/// Cache settings for compilation
type CacheSettings = {
    Enabled: bool
    CompilerKey: string
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

/// Cacheable compiler options (excludes debug-only dump flags)
type CacheOptions = {
    DisableFreeList: bool
    DisableANFOpt: bool
    DisableANFConstFolding: bool
    DisableANFConstProp: bool
    DisableANFCopyProp: bool
    DisableANFDCE: bool
    DisableANFStrengthReduction: bool
    DisableInlining: bool
    DisableTCO: bool
    DisableMIROpt: bool
    DisableMIRConstFolding: bool
    DisableMIRCSE: bool
    DisableMIRCopyProp: bool
    DisableMIRDCE: bool
    DisableMIRCFGSimplify: bool
    DisableMIRLICM: bool
    DisableLIROpt: bool
    DisableLIRPeephole: bool
    DisableFunctionTreeShaking: bool
    EnableCoverage: bool
    EnableLeakCheck: bool
}

let private buildCacheOptions (options: CompilerOptions) : CacheOptions = {
    DisableFreeList = options.DisableFreeList
    DisableANFOpt = options.DisableANFOpt
    DisableANFConstFolding = options.DisableANFConstFolding
    DisableANFConstProp = options.DisableANFConstProp
    DisableANFCopyProp = options.DisableANFCopyProp
    DisableANFDCE = options.DisableANFDCE
    DisableANFStrengthReduction = options.DisableANFStrengthReduction
    DisableInlining = options.DisableInlining
    DisableTCO = options.DisableTCO
    DisableMIROpt = options.DisableMIROpt
    DisableMIRConstFolding = options.DisableMIRConstFolding
    DisableMIRCSE = options.DisableMIRCSE
    DisableMIRCopyProp = options.DisableMIRCopyProp
    DisableMIRDCE = options.DisableMIRDCE
    DisableMIRCFGSimplify = options.DisableMIRCFGSimplify
    DisableMIRLICM = options.DisableMIRLICM
    DisableLIROpt = options.DisableLIROpt
    DisableLIRPeephole = options.DisableLIRPeephole
    DisableFunctionTreeShaking = options.DisableFunctionTreeShaking
    EnableCoverage = options.EnableCoverage
    EnableLeakCheck = options.EnableLeakCheck
}

let private hashValue (value: 'T) : string =
    // Use MessagePack encoding for deterministic cache hashes.
    match Cache.hashData value with
    | Ok hash -> hash
    | Error err -> Crash.crash $"Cache hash error: {err}"

let private recordPassTiming
    (recorder: PassTimingRecorder option)
    (pass: string)
    (elapsedMs: float)
    : unit =
    match recorder with
    | None -> ()
    | Some record ->
        record { Pass = pass; Elapsed = TimeSpan.FromMilliseconds(elapsedMs) }

// Cache helpers

let private defaultCacheSettings () : Result<CacheSettings, string> =
    Cache.getCompilerKey ()
    |> Result.map (fun key -> { Enabled = true; CompilerKey = key })

let private combineHashes (hashes: string list) : string =
    Cache.hashString (String.concat "|" hashes)

let private hashCacheOptions (options: CompilerOptions) : string =
    hashValue (buildCacheOptions options)

let private hashTypedProgram (program: AST.Program) : string =
    hashValue program

let private hashAnfFunctionBody (func: ANF.Function) : string =
    // Hash the post-inlining function body (including signature) so inlining changes invalidate cache keys.
    hashValue (func.TypedParams, func.ReturnType, func.Body)

let private collectTempIdsInAtom (atom: ANF.Atom) : Set<ANF.TempId> =
    match atom with
    | ANF.Var tid -> Set.singleton tid
    | _ -> Set.empty

let private collectTempIdsInAtoms (atoms: ANF.Atom list) : Set<ANF.TempId> =
    atoms
    |> List.fold (fun acc atom -> Set.union acc (collectTempIdsInAtom atom)) Set.empty

let private collectTempIdsInCExpr (cexpr: ANF.CExpr) : Set<ANF.TempId> =
    let atomIds = collectTempIdsInAtom
    let atomListIds = collectTempIdsInAtoms
    match cexpr with
    | ANF.Atom atom -> atomIds atom
    | ANF.TypedAtom (atom, _) -> atomIds atom
    | ANF.Prim (_, left, right) -> Set.union (atomIds left) (atomIds right)
    | ANF.UnaryPrim (_, atom) -> atomIds atom
    | ANF.IfValue (cond, thenVal, elseVal) ->
        atomIds cond |> Set.union (atomIds thenVal) |> Set.union (atomIds elseVal)
    | ANF.Call (_, args) -> atomListIds args
    | ANF.TailCall (_, args) -> atomListIds args
    | ANF.IndirectCall (func, args) -> Set.union (atomIds func) (atomListIds args)
    | ANF.IndirectTailCall (func, args) -> Set.union (atomIds func) (atomListIds args)
    | ANF.ClosureAlloc (_, captures) -> atomListIds captures
    | ANF.ClosureCall (closure, args) -> Set.union (atomIds closure) (atomListIds args)
    | ANF.ClosureTailCall (closure, args) -> Set.union (atomIds closure) (atomListIds args)
    | ANF.TupleAlloc atoms -> atomListIds atoms
    | ANF.TupleGet (tuple, _) -> atomIds tuple
    | ANF.StringConcat (left, right) -> Set.union (atomIds left) (atomIds right)
    | ANF.RefCountInc (atom, _) -> atomIds atom
    | ANF.RefCountDec (atom, _) -> atomIds atom
    | ANF.Print (atom, _) -> atomIds atom
    | ANF.FileReadText path -> atomIds path
    | ANF.FileExists path -> atomIds path
    | ANF.FileWriteText (path, content) -> Set.union (atomIds path) (atomIds content)
    | ANF.FileAppendText (path, content) -> Set.union (atomIds path) (atomIds content)
    | ANF.FileDelete path -> atomIds path
    | ANF.FileSetExecutable path -> atomIds path
    | ANF.FileWriteFromPtr (path, ptr, length) ->
        atomIds path |> Set.union (atomIds ptr) |> Set.union (atomIds length)
    | ANF.FloatSqrt atom -> atomIds atom
    | ANF.FloatAbs atom -> atomIds atom
    | ANF.FloatNeg atom -> atomIds atom
    | ANF.Int64ToFloat atom -> atomIds atom
    | ANF.FloatToInt64 atom -> atomIds atom
    | ANF.FloatToBits atom -> atomIds atom
    | ANF.RawAlloc numBytes -> atomIds numBytes
    | ANF.RawFree ptr -> atomIds ptr
    | ANF.RawGet (ptr, offset, _) -> Set.union (atomIds ptr) (atomIds offset)
    | ANF.RawGetByte (ptr, offset) -> Set.union (atomIds ptr) (atomIds offset)
    | ANF.RawSet (ptr, offset, value, _) ->
        atomIds ptr |> Set.union (atomIds offset) |> Set.union (atomIds value)
    | ANF.RawSetByte (ptr, offset, value) ->
        atomIds ptr |> Set.union (atomIds offset) |> Set.union (atomIds value)
    | ANF.RefCountIncString atom -> atomIds atom
    | ANF.RefCountDecString atom -> atomIds atom
    | ANF.RandomInt64 -> Set.empty
    | ANF.DateNow -> Set.empty
    | ANF.FloatToString atom -> atomIds atom

let rec private collectTempIdsInAExpr (expr: ANF.AExpr) : Set<ANF.TempId> =
    match expr with
    | ANF.Let (tempId, cexpr, body) ->
        Set.add tempId (Set.union (collectTempIdsInCExpr cexpr) (collectTempIdsInAExpr body))
    | ANF.Return atom -> collectTempIdsInAtom atom
    | ANF.If (cond, thenBranch, elseBranch) ->
        collectTempIdsInAtom cond
        |> Set.union (collectTempIdsInAExpr thenBranch)
        |> Set.union (collectTempIdsInAExpr elseBranch)

let private collectTypesInCExpr (cexpr: ANF.CExpr) : Set<AST.Type> =
    match cexpr with
    | ANF.TypedAtom (_, t) -> Set.singleton t
    | ANF.Print (_, t) -> Set.singleton t
    | ANF.RawGet (_, _, Some t) -> Set.singleton t
    | ANF.RawSet (_, _, _, Some t) -> Set.singleton t
    | _ -> Set.empty

let rec private collectTypesInAExpr (expr: ANF.AExpr) : Set<AST.Type> =
    match expr with
    | ANF.Let (_, cexpr, body) ->
        Set.union (collectTypesInCExpr cexpr) (collectTypesInAExpr body)
    | ANF.Return _ -> Set.empty
    | ANF.If (_, thenBranch, elseBranch) ->
        Set.union (collectTypesInAExpr thenBranch) (collectTypesInAExpr elseBranch)

let private collectTypesUsedByFunction (func: ANF.Function) (typeMap: ANF.TypeMap) : Set<AST.Type> =
    let paramTypes = func.TypedParams |> List.map (fun p -> p.Type) |> Set.ofList
    let returnTypes = Set.singleton func.ReturnType
    let exprTypes = collectTypesInAExpr func.Body
    let tempIds = collectTempIdsInAExpr func.Body
    let tempTypes =
        // TypeMap is from RC insertion; temps introduced after that (e.g., PrintInsertion)
        // are safe to ignore for cache keying because they don't affect codegen types.
        tempIds
        |> Set.toList
        |> List.choose (fun tid -> Map.tryFind tid typeMap)
        |> Set.ofList
    [paramTypes; returnTypes; exprTypes; tempTypes]
    |> List.fold Set.union Set.empty

let rec private collectNamedTypes (t: AST.Type) : Set<string> * Set<string> =
    let combine (records: Set<string>, sums: Set<string>) (r2: Set<string>, s2: Set<string>) =
        (Set.union records r2, Set.union sums s2)
    match t with
    | AST.TRecord (name, typeArgs) ->
        let (recArgs, sumArgs) =
            typeArgs
            |> List.fold (fun acc arg -> combine acc (collectNamedTypes arg)) (Set.empty, Set.empty)
        (Set.add name recArgs, sumArgs)
    | AST.TSum (name, typeArgs) ->
        let (recArgs, sumArgs) =
            typeArgs
            |> List.fold (fun acc arg -> combine acc (collectNamedTypes arg)) (Set.empty, Set.empty)
        (recArgs, Set.add name sumArgs)
    | AST.TFunction (args, ret) ->
        let (recArgs, sumArgs) =
            args
            |> List.fold (fun acc arg -> combine acc (collectNamedTypes arg)) (Set.empty, Set.empty)
        combine (recArgs, sumArgs) (collectNamedTypes ret)
    | AST.TTuple elems ->
        elems
        |> List.fold (fun acc elem -> combine acc (collectNamedTypes elem)) (Set.empty, Set.empty)
    | AST.TList elem -> collectNamedTypes elem
    | AST.TDict (keyType, valueType) -> combine (collectNamedTypes keyType) (collectNamedTypes valueType)
    | _ -> (Set.empty, Set.empty)

let private buildSumTypeDefs (variantLookup: AST_to_ANF.VariantLookup)
    : Map<string, string list * (string * int * AST.Type option) list> =
    variantLookup
    |> Map.toList
    |> List.fold (fun acc (variantName, (typeName, typeParams, tag, payload)) ->
        let (typeParams', variants) =
            match Map.tryFind typeName acc with
            | Some (existingParams, existingVariants) ->
                if existingParams <> typeParams then
                    Crash.crash $"Cache: sum type '{typeName}' has inconsistent type params"
                (existingParams, existingVariants)
            | None -> (typeParams, [])
        Map.add typeName (typeParams', (variantName, tag, payload) :: variants) acc
    ) Map.empty
    |> Map.map (fun _ (typeParams', variants) ->
        let sorted = variants |> List.sortBy (fun (name, tag, _) -> (tag, name))
        (typeParams', sorted))

let private buildTypeHash
    (typesUsed: Set<AST.Type>)
    (registries: AST_to_ANF.Registries)
    (sumTypeDefs: Map<string, string list * (string * int * AST.Type option) list>)
    : string =
    let (recordNames, sumNames) =
        typesUsed
        |> Set.toList
        |> List.fold (fun acc t ->
            let (recNames, sumNames) = collectNamedTypes t
            let (recAcc, sumAcc) = acc
            (Set.union recAcc recNames, Set.union sumAcc sumNames)) (Set.empty, Set.empty)

    let recordDefs =
        recordNames
        |> Set.toList
        |> List.sort
        |> List.map (fun name ->
            match Map.tryFind name registries.TypeReg with
            | Some fields -> (name, fields)
            | None -> Crash.crash $"Cache: record type '{name}' not found in registry")

    let sumDefs =
        sumNames
        |> Set.toList
        |> List.sort
        |> List.map (fun name ->
            match Map.tryFind name sumTypeDefs with
            | Some (typeParams, variants) -> (name, typeParams, variants)
            | None -> Crash.crash $"Cache: sum type '{name}' not found in variant registry")

    let typesSorted = typesUsed |> Set.toList |> List.sort
    hashValue (typesSorted, recordDefs, sumDefs)

let private buildFunctionSignatureHash
    (funcType: AST.Type)
    (registries: AST_to_ANF.Registries)
    (sumTypeDefs: Map<string, string list * (string * int * AST.Type option) list>)
    : string =
    let typeHash = buildTypeHash (Set.singleton funcType) registries sumTypeDefs
    hashValue (funcType, typeHash)

let private buildFunctionDependencyHashes
    (functions: ANF.Function list)
    (typeMap: ANF.TypeMap)
    (registries: AST_to_ANF.Registries)
    (_externalFunctionHashes: Map<string, string>)
    : Map<string, string> =
    // Compute dependency hashes from function bodies, types, and callee signatures.
    if List.isEmpty functions then
        Map.empty
    else
        let funcNames = functions |> List.map (fun f -> f.Name) |> Set.ofList
        let callGraph = ANFDeadCodeElimination.buildCallGraph functions
        let internalCallGraph =
            callGraph
            |> Map.map (fun _ callees -> callees |> Set.filter (fun name -> Set.contains name funcNames))
        let externalCallGraph =
            callGraph
            |> Map.map (fun _ callees -> callees |> Set.filter (fun name -> not (Set.contains name funcNames)))

        let sumTypeDefs = buildSumTypeDefs registries.VariantLookup

        let bodyHashes =
            functions
            |> List.map (fun func -> func.Name, hashAnfFunctionBody func)
            |> Map.ofList

        let typeHashes =
            functions
            |> List.map (fun func ->
                let typesUsed = collectTypesUsedByFunction func typeMap
                func.Name, buildTypeHash typesUsed registries sumTypeDefs)
            |> Map.ofList

        let signatureHashes =
            functions
            |> List.map (fun func ->
                let paramTypes = func.TypedParams |> List.map (fun p -> p.Type)
                let funcType = AST.TFunction (paramTypes, func.ReturnType)
                func.Name, buildFunctionSignatureHash funcType registries sumTypeDefs)
            |> Map.ofList

        let externalNames =
            externalCallGraph
            |> Map.fold (fun acc _ names -> Set.union acc names) Set.empty

        let externalSignatureHashes =
            externalNames
            |> Set.toList
            |> List.sort
            |> List.map (fun name ->
                match Map.tryFind name registries.FuncReg with
                | Some funcType -> (name, buildFunctionSignatureHash funcType registries sumTypeDefs)
                | None -> Crash.crash $"Cache: external function '{name}' not found in registry")
            |> Map.ofList

        let buildDeps (name: string) : string =
            let bodyHash =
                match Map.tryFind name bodyHashes with
                | Some hash -> hash
                | None -> Crash.crash $"Cache: missing body hash for {name}"
            let typeHash =
                match Map.tryFind name typeHashes with
                | Some hash -> hash
                | None -> Crash.crash $"Cache: missing type hash for {name}"
            let internalDeps =
                Map.tryFind name internalCallGraph
                |> Option.defaultValue Set.empty
                |> Set.toList
                |> List.sort
                |> List.map (fun callee ->
                    match Map.tryFind callee signatureHashes with
                    | Some hash -> (callee, hash)
                    | None -> Crash.crash $"Cache: missing signature hash for internal callee {callee}")
            let externalDeps =
                Map.tryFind name externalCallGraph
                |> Option.defaultValue Set.empty
                |> Set.toList
                |> List.sort
                |> List.map (fun callee ->
                    match Map.tryFind callee externalSignatureHashes with
                    | Some hash -> (callee, hash)
                    | None -> Crash.crash $"Cache: missing signature hash for external callee {callee}")
            hashValue (name, bodyHash, typeHash, internalDeps, externalDeps)

        functions
        |> List.map (fun func -> func.Name, buildDeps func.Name)
        |> Map.ofList

/// Determine whether to dump a specific IR, based on verbosity or explicit option
let private shouldDumpIR (verbosity: int) (enabled: bool) : bool =
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
let private printANFProgram (title: string) (program: ANF.Program) : unit =
    println title
    println (formatANF program)
    println ""

/// Print MIR program (with CFG) in a consistent format
let private printMIRProgram (title: string) (program: MIR.Program) : unit =
    println title
    println (formatMIR program)
    println ""

/// Print LIR program (with CFG) in a consistent format
let private printLIRProgram (title: string) (program: LIR.Program) : unit =
    println title
    println (formatLIR program)
    println ""

/// Print symbolic LIR program (with CFG) in a consistent format
let private printLIRSymbolicProgram (title: string) (program: LIRSymbolic.Program) : unit =
    println title
    println (formatLIRSymbolic program)
    println ""

/// Run SSA + MIR/LIR optimizations, returning an optimized LIR program
let private compileMirToLir
    (verbosity: int)
    (options: CompilerOptions)
    (sw: Stopwatch)
    (passTimingRecorder: PassTimingRecorder option)
    (stageSuffix: string)
    (mirProgram: MIR.Program)
    : Result<LIRSymbolic.Program, string> =

    let suffix = if stageSuffix = "" then "" else $" ({stageSuffix})"

    if verbosity >= 1 then println $"  [3.1/8] SSA Construction{suffix}..."
    let ssaStart = sw.Elapsed.TotalMilliseconds
    let ssaProgram = SSA_Construction.convertToSSA mirProgram
    let ssaElapsed = sw.Elapsed.TotalMilliseconds - ssaStart
    recordPassTiming passTimingRecorder "SSA Construction" ssaElapsed
    if verbosity >= 2 then
        let t = System.Math.Round(ssaElapsed, 1)
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
    let mirOptElapsed = sw.Elapsed.TotalMilliseconds - mirOptStart
    recordPassTiming passTimingRecorder "MIR Optimizations" mirOptElapsed
    if verbosity >= 2 then
        let t = System.Math.Round(mirOptElapsed, 1)
        println $"        {t}ms"

    if verbosity >= 1 then println $"  [4/8] MIR → LIR{suffix}..."
    let lirStart = sw.Elapsed.TotalMilliseconds
    let lirResult = MIR_to_LIR.toLIR optimizedProgram
    match lirResult with
    | Error err -> Error $"LIR conversion error: {err}"
    | Ok lirProgram ->
        let lirElapsed = sw.Elapsed.TotalMilliseconds - lirStart
        recordPassTiming passTimingRecorder "MIR -> LIR" lirElapsed
        if shouldDumpIR verbosity options.DumpLIR then
            printLIRSymbolicProgram "=== LIR (Low-level IR with CFG) ===" lirProgram
        if verbosity >= 2 then
            let t = System.Math.Round(lirElapsed, 1)
            println $"        {t}ms"

        let lirPassLabel =
            formatPassGroup
                "LIR Peephole"
                [("peephole", not options.DisableLIROpt && not options.DisableLIRPeephole)]
        if verbosity >= 1 then println $"  [4.5/8] {lirPassLabel}{suffix}..."
        let lirOptStart = sw.Elapsed.TotalMilliseconds
        let optimizedLir =
            if options.DisableLIROpt || options.DisableLIRPeephole then
                lirProgram
            else
                LIR_Peephole.optimizeProgram lirProgram
        let lirOptElapsed = sw.Elapsed.TotalMilliseconds - lirOptStart
        recordPassTiming passTimingRecorder "LIR Peephole" lirOptElapsed
        if verbosity >= 2 then
            let t = System.Math.Round(lirOptElapsed, 1)
            println $"        {t}ms"
        Ok optimizedLir

/// Allocate registers for a list of symbolic LIR functions
let private allocateRegistersForFunctions
    (functions: LIRSymbolic.Function list)
    : LIRSymbolic.Function list =
    functions |> List.map RegisterAllocation.allocateRegisters

/// Run MIR+LIR passes (including register allocation) from ANF functions
let private lowerToAllocatedLir
    (cacheSettings: CacheSettings)
    (verbosity: int)
    (options: CompilerOptions)
    (sw: Stopwatch)
    (passTimingRecorder: PassTimingRecorder option)
    (cacheMissRecorder: CacheMissRecorder option)
    (stageSuffix: string)
    (functions: ANF.Function list)
    (typeMap: ANF.TypeMap)
    (registries: AST_to_ANF.Registries)
    (externalReturnTypes: Map<string, AST.Type>)
    (dependencyHashes: Map<string, string>)
    : Result<LIRSymbolic.Function list, string> =

    let suffix = if stageSuffix = "" then "" else $" ({stageSuffix})"

    let optionsHashOpt =
        if cacheSettings.Enabled then
            Some (hashCacheOptions options)
        else
            None

    let functionOrder = functions |> List.map (fun f -> f.Name)
    let isCacheableFunctionName (name: string) : bool =
        // _start is synthesized per compile; caching it is noisy and unstable.
        name <> "_start"

    let recordCacheStats
        (cachedMap: Map<string, LIRSymbolic.Function>)
        (functionsToCompile: ANF.Function list)
        : unit =
        match cacheMissRecorder with
        | None -> ()
        | Some record ->
            if cacheSettings.Enabled then
                let missNames =
                    functionsToCompile
                    |> List.map (fun f -> f.Name)
                    |> List.filter isCacheableFunctionName
                    |> List.sort
                let hitCount = Map.count cachedMap
                let missCount = missNames.Length
                if hitCount > 0 || missCount > 0 then
                    let stageLabel = if stageSuffix = "" then "user" else stageSuffix
                    record { Stage = stageLabel; Hits = hitCount; Misses = missCount }

    let cachePlanResult : Result<string option * Map<string, LIRSymbolic.Function> * Map<string, string> * ANF.Function list, string> =
        match optionsHashOpt with
        | None -> Ok (None, Map.empty, Map.empty, functions)
        | Some optionsHash ->
            let buildKey (func: ANF.Function) (funcHash: string) : Cache.FunctionCacheKey = {
                CacheVersion = Cache.cacheVersion
                CompilerKey = cacheSettings.CompilerKey
                OptionsHash = optionsHash
                FunctionName = func.Name
                FunctionHash = funcHash
            }

            let keyedFunctions =
                functions
                |> List.map (fun func ->
                    let funcHash =
                        match Map.tryFind func.Name dependencyHashes with
                        | Some hash -> hash
                        | None -> Crash.crash $"Cache: missing dependency hash for {func.Name}"
                    (func, funcHash, buildKey func funcHash))

            let funcHashMap =
                keyedFunctions
                |> List.fold (fun acc (func, funcHash, _) -> Map.add func.Name funcHash acc) Map.empty

            let keys =
                keyedFunctions
                |> List.choose (fun (func, _, key) ->
                    if isCacheableFunctionName func.Name then Some key else None)
            Cache.getFunctions<LIRSymbolic.Function> keys
            |> Result.mapError (fun err -> $"Cache read error: {err}")
            |> Result.map (fun cachedMap ->
                let functionsToCompile =
                    keyedFunctions
                    |> List.choose (fun (func, _, _) ->
                        if not (isCacheableFunctionName func.Name) then
                            Some func
                        else
                            match Map.tryFind func.Name cachedMap with
                            | Some _ -> None
                            | None -> Some func)
                (Some optionsHash, cachedMap, funcHashMap, functionsToCompile))

    let compileMissing (functionsToCompile: ANF.Function list) : Result<LIRSymbolic.Function list, string> =
        if List.isEmpty functionsToCompile then
            Ok []
        else
            if verbosity >= 1 then println $"  [3/8] ANF → MIR{suffix}..."
            let mirStart = sw.Elapsed.TotalMilliseconds
            let anfProgram = ANF.Program (functionsToCompile, ANF.Return ANF.UnitLiteral)
            let mirResult =
                ANF_to_MIR.toMIRFunctionsOnly
                    anfProgram
                    typeMap
                    registries.FuncParams
                    registries.VariantLookup
                    registries.TypeReg
                    options.EnableCoverage
                    externalReturnTypes
            match mirResult with
            | Error err -> Error $"MIR conversion error: {err}"
            | Ok (mirFuncs, variantRegistry, mirRecordRegistry) ->
                let mirProgram = MIR.Program (mirFuncs, variantRegistry, mirRecordRegistry)
                let mirElapsed = sw.Elapsed.TotalMilliseconds - mirStart
                recordPassTiming passTimingRecorder "ANF -> MIR" mirElapsed
                if shouldDumpIR verbosity options.DumpMIR then
                    printMIRProgram "=== MIR (Control Flow Graph) ===" mirProgram
                if verbosity >= 2 then
                    let t = System.Math.Round(mirElapsed, 1)
                    println $"        {t}ms"
                compileMirToLir verbosity options sw passTimingRecorder stageSuffix mirProgram
                |> Result.bind (fun lirProgram ->
                    if verbosity >= 1 then println "  [5/8] Register Allocation..."
                    let allocStart = sw.Elapsed.TotalMilliseconds
                    let (LIRSymbolic.Program lirFuncs) = lirProgram
                    let allocatedFuncs = allocateRegistersForFunctions lirFuncs
                    let allocElapsed = sw.Elapsed.TotalMilliseconds - allocStart
                    recordPassTiming passTimingRecorder "Register Allocation" allocElapsed
                    if verbosity >= 2 then
                        let t = System.Math.Round(allocElapsed, 1)
                        println $"        {t}ms"
                    Ok allocatedFuncs)

    let compileMissingWithTiming
        (label: string)
        (functionsToCompile: ANF.Function list)
        : Result<LIRSymbolic.Function list, string> =
        if List.isEmpty functionsToCompile then
            Ok []
        else
            let startTime = sw.Elapsed.TotalMilliseconds
            compileMissing functionsToCompile
            |> Result.map (fun compiled ->
                let elapsed = sw.Elapsed.TotalMilliseconds - startTime
                recordPassTiming passTimingRecorder label elapsed
                compiled)

    cachePlanResult
    |> Result.map (fun (optionsHashOpt, cachedMap, funcHashes, functionsToCompile) ->
        recordCacheStats cachedMap functionsToCompile
        (optionsHashOpt, cachedMap, funcHashes, functionsToCompile))
    |> Result.bind (fun (optionsHashOpt, cachedMap, funcHashes, functionsToCompile) ->
        let (startFunctions, otherFunctions) =
            functionsToCompile |> List.partition (fun func -> func.Name = "_start")

        let compileResult =
            match passTimingRecorder, startFunctions with
            | Some _, _ :: _ ->
                compileMissingWithTiming "Start Function Compilation" startFunctions
                |> Result.bind (fun compiledStart ->
                    compileMissing otherFunctions
                    |> Result.map (fun compiledOther -> compiledStart @ compiledOther))
            | _ ->
                compileMissing functionsToCompile

        compileResult
        |> Result.bind (fun compiledFuncs ->
            let compiledMap =
                compiledFuncs
                |> List.fold (fun acc func -> Map.add func.Name func acc) Map.empty

            let writeResult =
                match optionsHashOpt with
                | Some optionsHash ->
                    let entries =
                        compiledFuncs
                        |> List.choose (fun func ->
                            if not (isCacheableFunctionName func.Name) then
                                None
                            else
                                match Map.tryFind func.Name funcHashes with
                                | None -> None
                                | Some hash ->
                                    let key : Cache.FunctionCacheKey = {
                                        CacheVersion = Cache.cacheVersion
                                        CompilerKey = cacheSettings.CompilerKey
                                        OptionsHash = optionsHash
                                        FunctionName = func.Name
                                        FunctionHash = hash
                                    }
                                    Some (key, func))
                    Cache.setFunctions entries
                    |> Result.mapError (fun err -> $"Cache write error: {err}")
                | _ -> Ok ()

            writeResult
            |> Result.map (fun () ->
                functionOrder
                |> List.choose (fun name ->
                    match Map.tryFind name compiledMap with
                    | Some func -> Some func
                    | None -> Map.tryFind name cachedMap))))

let private buildConversionResult
    (program: ANF.Program)
    (registries: AST_to_ANF.Registries)
    : AST_to_ANF.ConversionResult =
    {
        Program = program
        TypeReg = registries.TypeReg
        VariantLookup = registries.VariantLookup
        FuncReg = registries.FuncReg
        FuncParams = registries.FuncParams
        ModuleRegistry = registries.ModuleRegistry
    }

/// Run ANF optimization + RC insertion, returning a final ANF function list and type map
let private buildAnf
    (verbosity: int)
    (options: CompilerOptions)
    (sw: Stopwatch)
    (registries: AST_to_ANF.Registries)
    (functions: ANF.Function list)
    (passTimingRecorder: PassTimingRecorder option)
    : Result<ANF.Function list * ANF.TypeMap, string> =

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
    let anfProgram = ANF.Program (functions, ANF.Return ANF.UnitLiteral)
    if shouldDumpIR verbosity options.DumpANF then
        printANFProgram "=== ANF (before optimization) ===" anfProgram
    let anfOptStart = sw.Elapsed.TotalMilliseconds
    let anfOptimized =
        if shouldRunANFOptimize anfOptions then
            ANF_Optimize.optimizeProgramWithOptions anfOptions anfProgram
        else
            anfProgram
    let anfOptElapsed = sw.Elapsed.TotalMilliseconds - anfOptStart
    recordPassTiming passTimingRecorder "ANF Optimizations" anfOptElapsed
    if verbosity >= 2 then
        let t = System.Math.Round(anfOptElapsed, 1)
        println $"        {t}ms"
    if shouldDumpIR verbosity options.DumpANF then
        printANFProgram "=== ANF (after optimization) ===" anfOptimized

    if verbosity >= 1 then println "  [2.4/8] ANF Inlining..."
    let inlineStart = sw.Elapsed.TotalMilliseconds
    let anfInlined =
        if options.DisableInlining then
            anfOptimized
        else
            ANF_Inlining.inlineProgramDefault anfOptimized
    let inlineElapsed = sw.Elapsed.TotalMilliseconds - inlineStart
    recordPassTiming passTimingRecorder "ANF Inlining" inlineElapsed
    if verbosity >= 2 then
        let t = System.Math.Round(inlineElapsed, 1)
        println $"        {t}ms"

    let convResult = buildConversionResult anfInlined registries

    if verbosity >= 1 then println "  [2.5/8] Reference Count Insertion..."
    let rcStart = sw.Elapsed.TotalMilliseconds
    match RefCountInsertion.insertRCInProgram convResult with
    | Error err -> Error $"Reference count insertion error: {err}"
    | Ok (anfAfterRC, typeMap) ->
        let rcElapsed = sw.Elapsed.TotalMilliseconds - rcStart
        recordPassTiming passTimingRecorder "Reference Count Insertion" rcElapsed
        if verbosity >= 2 then
            let t = System.Math.Round(rcElapsed, 1)
            println $"        {t}ms"
        if shouldDumpIR verbosity options.DumpANF then
            printANFProgram "=== ANF (after RC insertion) ===" anfAfterRC

        let (ANF.Program (finalFunctions, _)) = anfAfterRC
        Ok (finalFunctions, typeMap)

/// Run tail call detection on a function list (for post-print insertion TCO)
let private applyTco
    (verbosity: int)
    (options: CompilerOptions)
    (sw: Stopwatch)
    (functions: ANF.Function list)
    (passTimingRecorder: PassTimingRecorder option)
    : ANF.Function list =
    if verbosity >= 1 then println "  [2.7/8] Tail Call Detection..."
    let tcoStart = sw.Elapsed.TotalMilliseconds
    let anfProgram = ANF.Program (functions, ANF.Return ANF.UnitLiteral)
    let anfAfterTCO =
        if options.DisableTCO then
            anfProgram
        else
            TailCallDetection.detectTailCallsInProgram anfProgram
    let tcoElapsed = sw.Elapsed.TotalMilliseconds - tcoStart
    recordPassTiming passTimingRecorder "Tail Call Detection" tcoElapsed
    if verbosity >= 2 then
        let t = System.Math.Round(tcoElapsed, 1)
        println $"        {t}ms"
    if shouldDumpIR verbosity options.DumpANF then
        printANFProgram "=== ANF (after Tail Call Detection) ===" anfAfterTCO
    let (ANF.Program (tcoFunctions, _)) = anfAfterTCO
    tcoFunctions

/// Run codegen, encoding, and binary generation
let private generateBinary
    (verbosity: int)
    (options: CompilerOptions)
    (sw: Stopwatch)
    (passTimingRecorder: PassTimingRecorder option)
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
        let codegenElapsed = sw.Elapsed.TotalMilliseconds - codegenStart
        recordPassTiming passTimingRecorder "Code Generation" codegenElapsed
        if verbosity >= 2 then
            let t = System.Math.Round(codegenElapsed, 1)
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
            let encodeElapsed = sw.Elapsed.TotalMilliseconds - encodeStart
            recordPassTiming passTimingRecorder "ARM64 Encoding" encodeElapsed
            if verbosity >= 2 then
                let t = System.Math.Round(encodeElapsed, 1)
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
            let binaryElapsed = sw.Elapsed.TotalMilliseconds - binaryStart
            recordPassTiming passTimingRecorder "Binary Generation" binaryElapsed
            if verbosity >= 2 then
                let t = System.Math.Round(binaryElapsed, 1)
                println $"        {t}ms"

            Ok binary


/// Shared compilation context used across pipeline steps
type PipelineContext = {
    TypeCheckEnv: TypeChecking.TypeCheckEnv
    GenericFuncDefs: AST_to_ANF.GenericFuncDefs
    SpecRegistry: AST_to_ANF.SpecRegistry
    Registries: AST_to_ANF.Registries
}

let private buildContext
    (typeCheckEnv: TypeChecking.TypeCheckEnv)
    (genericFuncDefs: AST_to_ANF.GenericFuncDefs)
    (specRegistry: AST_to_ANF.SpecRegistry)
    (registries: AST_to_ANF.Registries)
    : PipelineContext =
    {
        TypeCheckEnv = typeCheckEnv
        GenericFuncDefs = genericFuncDefs
        SpecRegistry = specRegistry
        Registries = registries
    }

/// Compiled preamble context - extends stdlib for a test file
/// Preamble functions are compiled ONCE per file, then reused for all tests in that file
type PreambleContext = {
    /// Extended compilation context (stdlib + preamble)
    Context: PipelineContext
    /// Preamble's ANF functions (after mono, inline, lift, ANF, RC, TCO)
    ANFFunctions: ANF.Function list
    /// Type map from RC insertion (merged with stdlib's TypeMap)
    TypeMap: ANF.TypeMap
    /// Preamble's symbolic LIR functions after register allocation
    SymbolicFunctions: LIRSymbolic.Function list
    /// Hash of the typed preamble (including any specializations)
    PreambleHash: string
    /// Dependency hashes for preamble functions (post-inlining)
    PreambleFunctionDependencyHashes: Map<string, string>
}

/// Parsed and typechecked preamble analysis for suite-level specialization
type PreambleAnalysis = {
    TypedAST: AST.Program
    TypeCheckEnv: TypeChecking.TypeCheckEnv
    GenericFuncDefs: AST_to_ANF.GenericFuncDefs
}

/// Result of compiling stdlib - can be reused across compilations
type StdlibResult = {
    /// Parsed stdlib AST (for merging with user AST)
    AST: AST.Program
    /// Type-checked stdlib with inferred types
    TypedAST: AST.Program
    /// Shared compilation context (typecheck env + registries)
    Context: PipelineContext
    /// Cache settings in effect for this stdlib build
    CacheSettings: CacheSettings
    /// Hash of stdlib source (typed AST)
    StdlibSourceHash: string
    /// Hash of stdlib source + specialization registry
    StdlibHash: string
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
    /// Dependency hashes for stdlib functions (post-inlining)
    StdlibFunctionDependencyHashes: Map<string, string>
}

/// Context for compiling user code
type CompileContext =
    | StdlibOnly of StdlibResult
    | StdlibWithPreamble of StdlibResult * PreambleContext

/// Request for compiling source code
type CompileRequest = {
    Context: CompileContext
    Mode: CompileMode
    Source: string
    SourceFile: string
    AllowInternal: bool
    Verbosity: int
    Options: CompilerOptions
    PassTimingRecorder: PassTimingRecorder option
    CacheMissRecorder: CacheMissRecorder option
}


// Helper functions for exception-to-Result conversion (Darklang compatibility)

/// Extract return types from a FuncReg (FunctionRegistry maps func name -> full type)
/// This is needed because buildReturnTypeReg only includes functions in the current program,
/// but we need return types for all callable functions (including stdlib)
let private extractReturnTypes (funcReg: Map<string, AST.Type>) : Map<string, AST.Type> =
    funcReg
    |> Map.toSeq
    |> Seq.choose (fun (name, typ) ->
        match typ with
        | AST.TFunction (_, retType) -> Some (name, retType)
        | other -> Crash.crash $"extractReturnTypes: Non-function type '{other}' found in FuncReg for '{name}'")
    |> Map.ofSeq

let private emptyRegistries (moduleRegistry: AST.ModuleRegistry) : AST_to_ANF.Registries =
    {
        TypeReg = Map.empty
        VariantLookup = Map.empty
        FuncReg = Map.empty
        FuncParams = Map.empty
        ModuleRegistry = moduleRegistry
    }

let private liftLambdasWithBase
    (baseRegistries: AST_to_ANF.Registries)
    (program: AST.Program)
    : Result<AST.Program, string> =
    let baseFuncReturnTypes = extractReturnTypes baseRegistries.FuncReg
    AST_to_ANF.liftLambdasInProgram
        baseRegistries.TypeReg
        baseRegistries.VariantLookup
        baseRegistries.FuncParams
        baseFuncReturnTypes
        program

let private mergeSpecRegistries
    (baseRegistry: AST_to_ANF.SpecRegistry)
    (overlayRegistry: AST_to_ANF.SpecRegistry)
    : AST_to_ANF.SpecRegistry =
    Map.fold (fun acc key value -> Map.add key value acc) baseRegistry overlayRegistry

let private collectLocalSpecs
    (genericDefs: AST_to_ANF.GenericFuncDefs)
    (program: AST.Program)
    : Set<AST_to_ANF.SpecKey> =
    let (AST.Program topLevels) = program
    let allSpecs =
        topLevels
        |> List.map (function
            | AST.FunctionDef f when List.isEmpty f.TypeParams -> AST_to_ANF.collectTypeAppsFromFunc f
            | AST.Expression e -> AST_to_ANF.collectTypeApps e
            | _ -> Set.empty)
        |> List.fold Set.union Set.empty
    allSpecs
    |> Set.filter (fun (funcName, _) -> Map.containsKey funcName genericDefs)

type private MonomorphizationMode =
    | Monomorphize of AST_to_ANF.GenericFuncDefs option
    | ReplaceTypeApps of AST_to_ANF.SpecRegistry
    | SpecializeLocalAndReplace of AST_to_ANF.SpecRegistry

let private prepareProgramForAnf
    (monomorphization: MonomorphizationMode)
    (baseRegistries: AST_to_ANF.Registries)
    (program: AST.Program)
    : Result<AST.Program, string> =
    let monomorphizedResult =
        match monomorphization with
        | Monomorphize None ->
            Ok (AST_to_ANF.monomorphize program)
        | Monomorphize (Some defs) ->
            Ok (AST_to_ANF.monomorphizeWithExternalDefs defs program)
        | ReplaceTypeApps specRegistry ->
            AST_to_ANF.replaceTypeAppsInProgramWithRegistry specRegistry program
        | SpecializeLocalAndReplace specRegistry ->
            let localGenericDefs = AST_to_ANF.extractGenericFuncDefs program
            if Map.isEmpty localGenericDefs then
                AST_to_ANF.replaceTypeAppsInProgramWithRegistry specRegistry program
            else
                let localSpecs = collectLocalSpecs localGenericDefs program
                let specialization = AST_to_ANF.specializeFromSpecs localGenericDefs localSpecs
                let combinedSpecRegistry =
                    mergeSpecRegistries specRegistry specialization.SpecRegistry
                let (AST.Program items) = program
                let specializedTopLevels = specialization.SpecializedFuncs |> List.map AST.FunctionDef
                let programWithSpecializations = AST.Program (specializedTopLevels @ items)
                AST_to_ANF.replaceTypeAppsInProgramWithRegistry combinedSpecRegistry programWithSpecializations
    monomorphizedResult
    |> Result.bind (fun monomorphized ->
        let inlined = AST_to_ANF.inlineLambdasInProgram monomorphized
        liftLambdasWithBase baseRegistries inlined)

let private buildRegistriesForProgram
    (moduleRegistry: AST.ModuleRegistry)
    (baseRegistries: AST_to_ANF.Registries)
    (typeDefs: AST.TypeDef list)
    (functions: AST.FunctionDef list)
    : AST_to_ANF.Registries * AST.FunctionDef list =
    let aliasReg = AST_to_ANF.buildAliasRegistry typeDefs
    let resolvedFunctions = AST_to_ANF.resolveAliasesInFunctions aliasReg functions
    let localRegistries = AST_to_ANF.buildRegistries moduleRegistry typeDefs aliasReg resolvedFunctions
    let mergedRegistries = AST_to_ANF.mergeRegistries baseRegistries localRegistries
    (mergedRegistries, resolvedFunctions)

let private convertTypedProgramToConversionResult
    (moduleRegistry: AST.ModuleRegistry)
    (typedProgram: AST.Program)
    : Result<AST_to_ANF.ConversionResult, string> =
    let baseRegistries = emptyRegistries moduleRegistry
    prepareProgramForAnf (Monomorphize None) baseRegistries typedProgram
    |> Result.bind (fun liftedProgram ->
        AST_to_ANF.splitTopLevels liftedProgram
        |> Result.bind (fun (typeDefs, functions, expr) ->
            let (registries, resolvedFunctions) =
                buildRegistriesForProgram moduleRegistry baseRegistries typeDefs functions
            let varGen = ANF.VarGen 0
            AST_to_ANF.convertFunctions registries varGen resolvedFunctions
            |> Result.bind (fun (anfFuncs, varGen1) ->
                AST_to_ANF.convertExprToAnf registries varGen1 expr
                |> Result.map (fun (anfExpr, _) ->
                    buildConversionResult (ANF.Program (anfFuncs, anfExpr)) registries))))

let private convertTypedProgramToUserOnlyWithMode
    (baseContext: PipelineContext)
    (monomorphization: MonomorphizationMode)
    (typedProgram: AST.Program)
    : Result<AST_to_ANF.UserOnlyResult, string> =
    prepareProgramForAnf monomorphization baseContext.Registries typedProgram
    |> Result.bind (fun liftedProgram ->
        AST_to_ANF.splitTopLevels liftedProgram
        |> Result.bind (fun (typeDefs, functions, expr) ->
            let (registries, resolvedFunctions) =
                buildRegistriesForProgram baseContext.Registries.ModuleRegistry baseContext.Registries typeDefs functions
            let varGen = ANF.VarGen 0
            AST_to_ANF.convertFunctions registries varGen resolvedFunctions
            |> Result.bind (fun (anfFuncs, varGen1) ->
                AST_to_ANF.convertExprToAnf registries varGen1 expr
                |> Result.map (fun (anfExpr, _) ->
                    {
                        UserFunctions = anfFuncs
                        MainExpr = anfExpr
                        TypeReg = registries.TypeReg
                        VariantLookup = registries.VariantLookup
                        FuncReg = registries.FuncReg
                        FuncParams = registries.FuncParams
                        ModuleRegistry = registries.ModuleRegistry
                    }))))

let private convertTypedProgramToUserOnly
    (baseContext: PipelineContext)
    (typedProgram: AST.Program)
    : Result<AST_to_ANF.UserOnlyResult, string> =
    convertTypedProgramToUserOnlyWithMode
        baseContext
        (Monomorphize (Some baseContext.GenericFuncDefs))
        typedProgram

/// Try to delete a file, ignoring any errors
let private tryDeleteFile (path: string) : unit =
    try File.Delete(path) with _ -> ()

/// Try to start a process, returning Result instead of throwing
let private tryStartProcess (info: ProcessStartInfo) : Result<Process, string> =
    try Ok (Process.Start(info))
    with ex -> Error ex.Message

/// Parse and typecheck a preamble, returning typed AST + preamble typecheck env
let analyzePreamble
    (allowInternal: bool)
    (stdlib: StdlibResult)
    (preamble: string)
    : Result<PreambleAnalysis, string> =
    let preambleSource = preamble + "\n0"
    Parser.parseString allowInternal preambleSource
    |> Result.mapError (fun err -> $"Preamble parse error: {err}")
    |> Result.bind (fun preambleAst ->
        TypeChecking.checkProgramWithBaseEnv stdlib.Context.TypeCheckEnv preambleAst
        |> Result.mapError (fun typeErr -> $"Preamble type error: {TypeChecking.typeErrorToString typeErr}")
        |> Result.map (fun (_programType, typedPreambleAst, preambleTypeCheckEnv) ->
            let preambleGenericDefs = AST_to_ANF.extractGenericFuncDefs typedPreambleAst
            {
                TypedAST = typedPreambleAst
                TypeCheckEnv = preambleTypeCheckEnv
                GenericFuncDefs = preambleGenericDefs
            }))

/// Load a .dark file allowing internal identifiers (for stdlib sources)
let private loadDarkFileAllowInternal (filename: string) : Result<AST.Program, string> =
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
        Parser.parseString true source
        |> Result.mapError (fun err -> $"Error parsing {filename}: {err}")

/// Load the stdlib and unicode_data.dark files
/// Returns the merged stdlib AST or an error message
let private loadStdlib () : Result<AST.Program, string> =
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
        "unicode_data.dark"
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
    let mergeFile (acc: AST.TopLevel list) (filename: string) : Result<AST.TopLevel list, string> =
        match loadDarkFileAllowInternal filename with
        | Error err -> Error err
        | Ok (AST.Program items) ->
            Ok (acc @ items)
    stdlibFiles
    |> List.fold (fun acc filename -> Result.bind (fun items -> mergeFile items filename) acc) (Ok [])
    |> Result.bind (fun items -> Ok (AST.Program items))


/// Build stdlib in isolation, returning reusable result
/// This can be called once and the result reused for multiple user program compilations
let buildStdlibWithCache
    (cacheSettings: CacheSettings)
    (passTimingRecorder: PassTimingRecorder option)
    : Result<StdlibResult, string> =
    match loadStdlib() with
    | Error e ->
        Error e
    | Ok stdlibAst ->
        // Add dummy main expression for type checking (stdlib has no main)
        let (AST.Program items) = stdlibAst
        let withMain = AST.Program (items @ [AST.Expression AST.UnitLiteral])

        match TypeChecking.checkProgramWithEnv withMain with
        | Error e ->
            let msg = TypeChecking.typeErrorToString e
            Error msg
        | Ok (_, typedStdlib, typeCheckEnv) ->
            let stdlibSourceHash = hashTypedProgram typedStdlib
            // Extract generic function definitions for on-demand monomorphization
            let genericFuncDefs = AST_to_ANF.extractGenericFuncDefs typedStdlib
            // Build module registry once (reused across all compilations)
            let moduleRegistry = Stdlib.buildModuleRegistry ()
            match convertTypedProgramToConversionResult moduleRegistry typedStdlib with
            | Error e ->
                Error e
            | Ok anfResult ->
                let registries : AST_to_ANF.Registries = {
                    TypeReg = anfResult.TypeReg
                    VariantLookup = anfResult.VariantLookup
                    FuncReg = anfResult.FuncReg
                    FuncParams = anfResult.FuncParams
                    ModuleRegistry = anfResult.ModuleRegistry
                }
                let context = buildContext typeCheckEnv genericFuncDefs Map.empty registries
                let specHash = hashValue context.SpecRegistry
                let stdlibHash = combineHashes [stdlibSourceHash; specHash]
                let (ANF.Program (stdlibFunctions, _)) = anfResult.Program
                let stdlibOptions = { defaultOptions with DisableANFOpt = true; DisableInlining = true }
                let sw = Stopwatch.StartNew()
                match buildAnf 0 stdlibOptions sw registries stdlibFunctions passTimingRecorder with
                | Error e ->
                    Error e
                | Ok (anfFunctions, typeMap) ->
                    let tcoFunctions = applyTco 0 stdlibOptions sw anfFunctions passTimingRecorder
                    let stdlibFuncMap =
                        tcoFunctions
                        |> List.map (fun f -> f.Name, f)
                        |> Map.ofList
                    let stdlibANFCallGraph = ANFDeadCodeElimination.buildCallGraph tcoFunctions

                    let stdlibDependencyHashes =
                        if cacheSettings.Enabled then
                            buildFunctionDependencyHashes tcoFunctions typeMap registries Map.empty
                        else
                            Map.empty

                    let externalReturnTypes = extractReturnTypes registries.FuncReg
                    match lowerToAllocatedLir
                        cacheSettings
                        0
                        stdlibOptions
                        sw
                        passTimingRecorder
                        None
                        "stdlib"
                        tcoFunctions
                        typeMap
                        registries
                        externalReturnTypes
                        stdlibDependencyHashes with
                    | Error e ->
                        Error e
                    | Ok allocatedFuncs ->
                        let allocatedSymbolic = LIRSymbolic.Program allocatedFuncs
                        match LIRSymbolic.toLIR allocatedSymbolic with
                        | Error err ->
                            Error err
                        | Ok allocatedProgram ->
                            let (LIR.Program (resolvedFuncs, _, _)) = allocatedProgram
                            let stdlibCallGraph = DeadCodeElimination.buildCallGraph resolvedFuncs
                            Ok {
                                AST = stdlibAst
                                TypedAST = typedStdlib
                                Context = context
                                CacheSettings = cacheSettings
                                StdlibSourceHash = stdlibSourceHash
                                StdlibHash = stdlibHash
                                LIRProgram = allocatedProgram
                                AllocatedFunctions = resolvedFuncs
                                StdlibCallGraph = stdlibCallGraph
                                StdlibANFFunctions = stdlibFuncMap
                                StdlibANFCallGraph = stdlibANFCallGraph
                                StdlibTypeMap = typeMap
                                StdlibFunctionDependencyHashes = stdlibDependencyHashes
                            }

/// Build stdlib in isolation with default cache settings
let buildStdlibWithTrace (passTimingRecorder: PassTimingRecorder option) : Result<StdlibResult, string> =
    defaultCacheSettings ()
    |> Result.bind (fun cacheSettings -> buildStdlibWithCache cacheSettings passTimingRecorder)

/// Build stdlib in isolation with default cache settings
let buildStdlib () : Result<StdlibResult, string> =
    buildStdlibWithTrace None

/// Build stdlib specializations for a spec set and merge them into the stdlib result
let buildStdlibSpecializations
    (stdlib: StdlibResult)
    (specs: Set<AST_to_ANF.SpecKey>)
    (passTimingRecorder: PassTimingRecorder option)
    : Result<StdlibResult, string> =
    if Set.isEmpty specs then
        Ok stdlib
    else
        let specialization = AST_to_ANF.specializeFromSpecs stdlib.Context.GenericFuncDefs specs
        let combinedSpecRegistry = mergeSpecRegistries stdlib.Context.SpecRegistry specialization.SpecRegistry
        let stdlibHash = combineHashes [stdlib.StdlibSourceHash; hashValue combinedSpecRegistry]
        let existingNames =
            stdlib.StdlibANFFunctions
            |> Map.keys
            |> Set.ofSeq
        let newSpecializedFuncs =
            specialization.SpecializedFuncs
            |> List.filter (fun f -> not (Set.contains f.Name existingNames))

        if List.isEmpty newSpecializedFuncs then
            let updatedContext = { stdlib.Context with SpecRegistry = combinedSpecRegistry }
            Ok {
                stdlib with
                    Context = updatedContext
                    StdlibHash = stdlibHash
            }
        else
            let rec mapResult (f: 'a -> Result<'b, string>) (items: 'a list) : Result<'b list, string> =
                match items with
                | [] -> Ok []
                | x :: xs ->
                    f x
                    |> Result.bind (fun x' ->
                        mapResult f xs
                        |> Result.map (fun xs' -> x' :: xs'))

            AST_to_ANF.splitTopLevels stdlib.TypedAST
            |> Result.bind (fun (typeDefs, _functions, _expr) ->
                let (registries, resolvedFunctions) =
                    buildRegistriesForProgram
                        stdlib.Context.Registries.ModuleRegistry
                        stdlib.Context.Registries
                        typeDefs
                        newSpecializedFuncs

                let replacedFunctionsResult =
                    resolvedFunctions
                    |> mapResult (AST_to_ANF.replaceTypeAppsInFuncWithRegistry combinedSpecRegistry)

                replacedFunctionsResult
                |> Result.bind (fun replacedFunctions ->
                    let varGen = ANF.VarGen 0
                    AST_to_ANF.convertFunctions registries varGen replacedFunctions
                    |> Result.bind (fun (anfFuncs, _varGen1) ->
                        let stdlibOptions = { defaultOptions with DisableANFOpt = true; DisableInlining = true }
                        let sw = Stopwatch.StartNew()
                        buildAnf 0 stdlibOptions sw registries anfFuncs passTimingRecorder
                        |> Result.bind (fun (anfFunctions, typeMap) ->
                            let tcoFunctions = applyTco 0 stdlibOptions sw anfFunctions passTimingRecorder
                            let newAnfFuncMap =
                                tcoFunctions
                                |> List.map (fun f -> f.Name, f)
                                |> Map.ofList
                            let specializationDependencyHashes =
                                if stdlib.CacheSettings.Enabled then
                                    buildFunctionDependencyHashes
                                        tcoFunctions
                                        typeMap
                                        registries
                                        stdlib.StdlibFunctionDependencyHashes
                                else
                                    Map.empty
                            let externalReturnTypes = extractReturnTypes registries.FuncReg
                            lowerToAllocatedLir
                                stdlib.CacheSettings
                                0
                                stdlibOptions
                                sw
                                passTimingRecorder
                                None
                                "stdlib_specializations"
                                tcoFunctions
                                typeMap
                                registries
                                externalReturnTypes
                                specializationDependencyHashes
                            |> Result.bind (fun allocatedFuncs ->
                                let (LIR.Program (_, stdlibStrings, stdlibFloats)) = stdlib.LIRProgram
                                LIRSymbolic.toLIRWithPools stdlibStrings stdlibFloats allocatedFuncs
                                |> Result.bind (fun (LIR.Program (newLirFuncs, mergedStrings, mergedFloats)) ->
                                    let allLirFuncs = stdlib.AllocatedFunctions @ newLirFuncs
                                    let mergedStdlibTypeMap =
                                        Map.fold (fun acc k v -> Map.add k v acc) stdlib.StdlibTypeMap typeMap
                                    let mergedStdlibAnfFunctions =
                                        Map.fold (fun acc k v -> Map.add k v acc) stdlib.StdlibANFFunctions newAnfFuncMap
                                    let allAnfFunctions =
                                        mergedStdlibAnfFunctions
                                        |> Map.toList
                                        |> List.map snd
                                    let stdlibCallGraph = DeadCodeElimination.buildCallGraph allLirFuncs
                                    let stdlibAnfCallGraph = ANFDeadCodeElimination.buildCallGraph allAnfFunctions
                                    let mergedDependencyHashes =
                                        if stdlib.CacheSettings.Enabled then
                                            specializationDependencyHashes
                                            |> Map.fold (fun acc name hash -> Map.add name hash acc) stdlib.StdlibFunctionDependencyHashes
                                        else
                                            stdlib.StdlibFunctionDependencyHashes
                                    let updatedContext = {
                                        stdlib.Context with
                                            Registries = registries
                                            SpecRegistry = combinedSpecRegistry
                                    }
                                    Ok {
                                        stdlib with
                                            Context = updatedContext
                                            StdlibHash = stdlibHash
                                            LIRProgram = LIR.Program (allLirFuncs, mergedStrings, mergedFloats)
                                            AllocatedFunctions = allLirFuncs
                                            StdlibCallGraph = stdlibCallGraph
                                            StdlibANFFunctions = mergedStdlibAnfFunctions
                                            StdlibANFCallGraph = stdlibAnfCallGraph
                                            StdlibTypeMap = mergedStdlibTypeMap
                                            StdlibFunctionDependencyHashes = mergedDependencyHashes
                                    }))))))

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
    PassTimingRecorder: PassTimingRecorder option
    CacheMissRecorder: CacheMissRecorder option
    Stdlib: StdlibResult
    BaseContext: PipelineContext
    Monomorphization: MonomorphizationMode
    PrebuiltSymbolicFunctions: LIRSymbolic.Function list
    SkipFunctionNames: Set<string>
    ExternalFunctionDependencyHashes: Map<string, string>
    EmitFunctionEvents: bool
    TreeShakeUserFunctions: bool
    Labels: UserCompileLabels
    SourceFile: string
    Source: string
}

/// Compile a user/test program against a prebuilt stdlib/preamble context
let private compileUserWithPlan (plan: UserCompilePlan) : CompileReport =
    let sw = Stopwatch.StartNew()
    let result =
        try
            // Pass 1: Parse user code only
            if plan.Verbosity >= 1 then println plan.Labels.Parse
            let parseResult = Parser.parseString plan.AllowInternal plan.Source
            let parseTime = sw.Elapsed.TotalMilliseconds
            recordPassTiming plan.PassTimingRecorder "Parse" parseTime
            if plan.Verbosity >= 2 then
                let t = System.Math.Round(parseTime, 1)
                println $"        {t}ms"

            match parseResult with
            | Error err -> Error $"Parse error: {err}"
            | Ok userAst ->
                // Pass 1.5: Type Checking (user code with base TypeCheckEnv)
                if plan.Verbosity >= 1 then println plan.Labels.TypeCheck
                let typeCheckResult = TypeChecking.checkProgramWithBaseEnv plan.BaseContext.TypeCheckEnv userAst
                let typeCheckTime = sw.Elapsed.TotalMilliseconds - parseTime
                recordPassTiming plan.PassTimingRecorder "Type Checking" typeCheckTime
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
                        convertTypedProgramToUserOnlyWithMode
                            plan.BaseContext
                            plan.Monomorphization
                            typedUserAst
                    let anfTime = sw.Elapsed.TotalMilliseconds - parseTime - typeCheckTime
                    recordPassTiming plan.PassTimingRecorder "AST -> ANF" anfTime
                    if plan.Verbosity >= 2 then
                        let t = System.Math.Round(anfTime, 1)
                        println $"        {t}ms"

                    match userOnlyResult with
                    | Error err -> Error $"ANF conversion error: {err}"
                    | Ok userOnly ->
                        let functionsToCompile =
                            userOnly.UserFunctions
                            |> List.filter (fun f -> not (Set.contains f.Name plan.SkipFunctionNames))

                        if plan.EmitFunctionEvents && plan.Verbosity >= 3 then
                            println $"  [COMPILE] {functionsToCompile.Length} user functions compiled fresh"
                            for f in functionsToCompile do
                                println $"    - {f.Name}"

                        let entryFunction =
                            AST_to_ANF.synthesizeEntryFunction "_start" programType userOnly.MainExpr
                        let userRegistries : AST_to_ANF.Registries = {
                            TypeReg = userOnly.TypeReg
                            VariantLookup = userOnly.VariantLookup
                            FuncReg = userOnly.FuncReg
                            FuncParams = userOnly.FuncParams
                            ModuleRegistry = userOnly.ModuleRegistry
                        }
                        let anfResult =
                            buildAnf
                                plan.Verbosity
                                plan.Options
                                sw
                                userRegistries
                                (entryFunction :: functionsToCompile)
                                plan.PassTimingRecorder
                        match anfResult with
                        | Error err -> Error err
                        | Ok (anfFunctions, typeMap) ->
                            if plan.Verbosity >= 1 then println "  [2.6/8] Print Insertion..."
                            let printStart = sw.Elapsed.TotalMilliseconds
                            match PrintInsertion.insertPrintInEntry "_start" programType anfFunctions with
                            | Error err -> Error $"Print insertion error: {err}"
                            | Ok printedFunctions ->
                                let printElapsed = sw.Elapsed.TotalMilliseconds - printStart
                                recordPassTiming plan.PassTimingRecorder "Print Insertion" printElapsed
                                if plan.Verbosity >= 2 then
                                    let t = System.Math.Round(printElapsed, 1)
                                    println $"        {t}ms"
                                if shouldDumpIR plan.Verbosity plan.Options.DumpANF then
                                    let printProgram = ANF.Program (printedFunctions, ANF.Return ANF.UnitLiteral)
                                    printANFProgram "=== ANF (after Print insertion) ===" printProgram

                                let tcoFunctions = applyTco plan.Verbosity plan.Options sw printedFunctions plan.PassTimingRecorder
                                let userDependencyHashes =
                                    if plan.Stdlib.CacheSettings.Enabled then
                                        buildFunctionDependencyHashes
                                            tcoFunctions
                                            typeMap
                                            userRegistries
                                            plan.ExternalFunctionDependencyHashes
                                    else
                                        Map.empty
                                let externalReturnTypes = extractReturnTypes userRegistries.FuncReg
                                let userLirResult =
                                    lowerToAllocatedLir
                                        plan.Stdlib.CacheSettings
                                        plan.Verbosity
                                        plan.Options
                                        sw
                                        plan.PassTimingRecorder
                                        plan.CacheMissRecorder
                                        plan.Labels.StageSuffix
                                        tcoFunctions
                                        typeMap
                                        userRegistries
                                        externalReturnTypes
                                        userDependencyHashes
                                match userLirResult with
                                | Error err -> Error err
                                | Ok allocatedUserFuncs ->
                                    let (LIR.Program (_, stdlibStrings, stdlibFloats)) = plan.Stdlib.LIRProgram
                                    let allSymbolicUserFuncs = plan.PrebuiltSymbolicFunctions @ allocatedUserFuncs

                                    match LIRSymbolic.toLIRWithPools stdlibStrings stdlibFloats allSymbolicUserFuncs with
                                    | Error err -> Error $"LIR pool resolution error: {err}"
                                    | Ok (LIR.Program (allUserFuncs, mergedStrings, mergedFloats)) ->
                                        let finalUserFuncs =
                                            if plan.TreeShakeUserFunctions then
                                                if plan.Verbosity >= 1 then println "  [5.5/8] Function Tree Shaking..."
                                                let treeShakeStart = sw.Elapsed.TotalMilliseconds
                                                let shakenUserFuncs =
                                                    if plan.Options.DisableFunctionTreeShaking then
                                                        allUserFuncs
                                                    else
                                                        FunctionTreeShaking.filterUserFunctions (Some "_start") allUserFuncs
                                                let treeShakeElapsed = sw.Elapsed.TotalMilliseconds - treeShakeStart
                                                recordPassTiming plan.PassTimingRecorder "Function Tree Shaking" treeShakeElapsed
                                                shakenUserFuncs
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
                                                let treeShakeStart = sw.Elapsed.TotalMilliseconds
                                                FunctionTreeShaking.filterStdlibFunctions
                                                    plan.Stdlib.StdlibCallGraph
                                                    finalUserFuncs
                                                    plan.Stdlib.AllocatedFunctions
                                                |> fun shakenStdlib ->
                                                    let treeShakeElapsed = sw.Elapsed.TotalMilliseconds - treeShakeStart
                                                    recordPassTiming plan.PassTimingRecorder "Function Tree Shaking" treeShakeElapsed
                                                    shakenStdlib

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
                                                plan.PassTimingRecorder
                                                "  [6/8] Code Generation..."
                                                "  [7/7] ARM64 Encoding..."
                                                "  [7/7] Binary Generation ({format})..."
                                                false
                                                false
                                                allocatedProgram
                                        match binaryResult with
                                        | Error err -> Error err
                                        | Ok binary ->
                                            Ok binary
        with
        | ex ->
            Error $"Compilation failed: {ex.Message}"
    sw.Stop()
    match result with
    | Ok _ when plan.Verbosity >= 1 ->
        println $"  ✓ Compilation complete ({System.Math.Round(sw.Elapsed.TotalMilliseconds, 1)}ms)"
    | _ -> ()
    { Result = result; CompileTime = sw.Elapsed }

/// Build preamble with stdlib as base, returning extended context for test compilation
/// Preamble functions go through the full pipeline (parse → typecheck → mono → inline → lift → ANF → RC → TCO)
/// The result is built once per file and reused for all tests in that file
let buildPreambleContext
    (allowInternal: bool)
    (stdlib: StdlibResult)
    (preamble: string)
    (_sourceFile: string)
    (_funcLineMap: Map<string, int>)
    (passTimingRecorder: PassTimingRecorder option)
    : Result<PreambleContext, string> =
    // Handle empty preamble - return a context that just wraps stdlib
    if String.IsNullOrWhiteSpace(preamble) then
        let emptyContext = {
            Context = stdlib.Context
            ANFFunctions = []
            TypeMap = stdlib.StdlibTypeMap
            SymbolicFunctions = []
            PreambleHash = Cache.hashString ""
            PreambleFunctionDependencyHashes = Map.empty
        }
        Ok emptyContext
    else
        // Parse preamble with dummy expression (parser requires a main expression)
        let preambleSource = preamble + "\n0"
        match Parser.parseString allowInternal preambleSource with
        | Error err ->
            let msg = $"Preamble parse error: {err}"
            Error msg
        | Ok preambleAst ->
            // Type-check preamble with stdlib context
            match TypeChecking.checkProgramWithBaseEnv stdlib.Context.TypeCheckEnv preambleAst with
            | Error typeErr ->
                let msg = $"Preamble type error: {TypeChecking.typeErrorToString typeErr}"
                Error msg
            | Ok (_programType, typedPreambleAst, preambleTypeCheckEnv) ->
                let preambleHash = hashTypedProgram typedPreambleAst
                // Extract generic function definitions from preamble
                let preambleGenericDefs = AST_to_ANF.extractGenericFuncDefs typedPreambleAst
                // Merge stdlib generics with preamble generics
                let mergedGenericDefs = Map.fold (fun acc k v -> Map.add k v acc) stdlib.Context.GenericFuncDefs preambleGenericDefs

                // Convert preamble to ANF (mono → inline → lift → ANF)
                match convertTypedProgramToUserOnly stdlib.Context typedPreambleAst with
                | Error err ->
                    let msg = $"Preamble ANF conversion error: {err}"
                    Error msg
                | Ok preambleUserOnly ->
                    let preambleRegistries : AST_to_ANF.Registries = {
                        TypeReg = preambleUserOnly.TypeReg
                        VariantLookup = preambleUserOnly.VariantLookup
                        FuncReg = preambleUserOnly.FuncReg
                        FuncParams = preambleUserOnly.FuncParams
                        ModuleRegistry = preambleUserOnly.ModuleRegistry
                    }
                    let pipelineContext = buildContext preambleTypeCheckEnv mergedGenericDefs Map.empty preambleRegistries
                    let preambleOptions = defaultOptions
                    let sw = Stopwatch.StartNew()
                    match buildAnf 0 preambleOptions sw preambleRegistries preambleUserOnly.UserFunctions passTimingRecorder with
                    | Error err ->
                        let rcPrefix = "Reference count insertion error: "
                        let msg =
                            if err.StartsWith(rcPrefix) then
                                let suffix = err.Substring(rcPrefix.Length)
                                $"Preamble RC insertion error: {suffix}"
                            else
                                $"Preamble {err}"
                        Error msg
                    | Ok (preambleFunctions, typeMap) ->
                        let tcoFunctions = applyTco 0 preambleOptions sw preambleFunctions passTimingRecorder
                        let preambleDependencyHashes =
                            if stdlib.CacheSettings.Enabled then
                                buildFunctionDependencyHashes
                                    tcoFunctions
                                    typeMap
                                    preambleRegistries
                                    stdlib.StdlibFunctionDependencyHashes
                            else
                                Map.empty
                        let preambleExternalReturnTypes = extractReturnTypes preambleRegistries.FuncReg
                        match lowerToAllocatedLir
                            stdlib.CacheSettings
                            0
                            preambleOptions
                            sw
                            passTimingRecorder
                            None
                            "preamble"
                            tcoFunctions
                            typeMap
                            preambleRegistries
                            preambleExternalReturnTypes
                            preambleDependencyHashes with
                        | Error err ->
                            let msg = $"Preamble {err}"
                            Error msg
                        | Ok allocatedFuncs ->
                            let stdlibFuncNames =
                                stdlib.AllocatedFunctions
                                |> List.map (fun func -> func.Name)
                                |> Set.ofList
                            let isStdlibFunction (name: string) : bool =
                                Set.contains name stdlibFuncNames
                            let preambleOnlyFuncs =
                                allocatedFuncs
                                |> List.filter (fun func -> not (isStdlibFunction func.Name))
                            let preambleSymbolicFuncs = preambleOnlyFuncs

                            // Merge TypeMaps (stdlib + preamble)
                            let mergedTypeMap = Map.fold (fun acc k v -> Map.add k v acc) stdlib.StdlibTypeMap typeMap

                            let context = {
                                Context = pipelineContext
                                ANFFunctions = tcoFunctions
                                TypeMap = mergedTypeMap
                                SymbolicFunctions = preambleSymbolicFuncs
                                PreambleHash = preambleHash
                                PreambleFunctionDependencyHashes = preambleDependencyHashes
                            }
                            Ok context

/// Build preamble context from a typed preamble analysis and precomputed specializations
let buildPreambleContextFromAnalysis
    (stdlib: StdlibResult)
    (analysis: PreambleAnalysis)
    (specialization: AST_to_ANF.SpecializationResult)
    (_sourceFile: string)
    (_funcLineMap: Map<string, int>)
    (passTimingRecorder: PassTimingRecorder option)
    : Result<PreambleContext, string> =
    let combinedSpecRegistry = mergeSpecRegistries stdlib.Context.SpecRegistry specialization.SpecRegistry

    let mergedGenericDefs =
        Map.fold (fun acc k v -> Map.add k v acc) stdlib.Context.GenericFuncDefs analysis.GenericFuncDefs

    let (AST.Program items) = analysis.TypedAST
    let specializedTopLevels = specialization.SpecializedFuncs |> List.map AST.FunctionDef
    let programWithSpecializations = AST.Program (specializedTopLevels @ items)

    let preambleHash = hashTypedProgram programWithSpecializations
    convertTypedProgramToUserOnlyWithMode
        stdlib.Context
        (ReplaceTypeApps combinedSpecRegistry)
        programWithSpecializations
    |> Result.bind (fun preambleUserOnly ->
        let preambleRegistries : AST_to_ANF.Registries = {
            TypeReg = preambleUserOnly.TypeReg
            VariantLookup = preambleUserOnly.VariantLookup
            FuncReg = preambleUserOnly.FuncReg
            FuncParams = preambleUserOnly.FuncParams
            ModuleRegistry = preambleUserOnly.ModuleRegistry
        }
        let pipelineContext = buildContext analysis.TypeCheckEnv mergedGenericDefs combinedSpecRegistry preambleRegistries
        let preambleOptions = defaultOptions
        let sw = Stopwatch.StartNew()
        match buildAnf 0 preambleOptions sw preambleRegistries preambleUserOnly.UserFunctions passTimingRecorder with
        | Error err ->
            let rcPrefix = "Reference count insertion error: "
            let msg =
                if err.StartsWith(rcPrefix) then
                    let suffix = err.Substring(rcPrefix.Length)
                    $"Preamble RC insertion error: {suffix}"
                else
                    $"Preamble {err}"
            Error msg
        | Ok (preambleFunctions, typeMap) ->
            let tcoFunctions = applyTco 0 preambleOptions sw preambleFunctions passTimingRecorder
            let preambleDependencyHashes =
                if stdlib.CacheSettings.Enabled then
                    buildFunctionDependencyHashes
                        tcoFunctions
                        typeMap
                        preambleRegistries
                        stdlib.StdlibFunctionDependencyHashes
                else
                    Map.empty
            let preambleExternalReturnTypes = extractReturnTypes preambleRegistries.FuncReg
            match lowerToAllocatedLir
                stdlib.CacheSettings
                0
                preambleOptions
                sw
                passTimingRecorder
                None
                "preamble"
                tcoFunctions
                typeMap
                preambleRegistries
                preambleExternalReturnTypes
                preambleDependencyHashes with
            | Error err ->
                let msg = $"Preamble {err}"
                Error msg
            | Ok allocatedFuncs ->
                let stdlibFuncNames =
                    stdlib.AllocatedFunctions
                    |> List.map (fun func -> func.Name)
                    |> Set.ofList
                let isStdlibFunction (name: string) : bool =
                    Set.contains name stdlibFuncNames
                let preambleOnlyFuncs =
                    allocatedFuncs
                    |> List.filter (fun func -> not (isStdlibFunction func.Name))
                let preambleSymbolicFuncs = preambleOnlyFuncs

                let mergedTypeMap = Map.fold (fun acc k v -> Map.add k v acc) stdlib.StdlibTypeMap typeMap

                Ok {
                    Context = pipelineContext
                    ANFFunctions = tcoFunctions
                    TypeMap = mergedTypeMap
                    SymbolicFunctions = preambleSymbolicFuncs
                    PreambleHash = preambleHash
                    PreambleFunctionDependencyHashes = preambleDependencyHashes
                })

let private labelsForMode (mode: CompileMode) : UserCompileLabels =
    match mode with
    | FullProgram ->
        {
            Parse = "  [1/8] Parse..."
            TypeCheck = "  [1.5/8] Type Checking (with stdlib env)..."
            Anf = "  [2/8] AST → ANF (user only)..."
            StageSuffix = "user only"
        }
    | TestExpression ->
        {
            Parse = "  [1/8] Parse (test expr only)..."
            TypeCheck = "  [1.5/8] Type Checking (with preamble env)..."
            Anf = "  [2/8] AST → ANF (test expr only)..."
            StageSuffix = ""
        }

let private buildCompilePlan (request: CompileRequest) : UserCompilePlan =
    let (stdlib, baseContext, prebuiltSymbolic, skipNames, externalDependencyHashes) =
        match request.Context with
        | StdlibOnly stdlib ->
            let externalHashes =
                if stdlib.CacheSettings.Enabled then
                    stdlib.StdlibFunctionDependencyHashes
                else
                    Map.empty
            stdlib, stdlib.Context, [], Set.empty, externalHashes
        | StdlibWithPreamble (stdlib, preambleCtx) ->
            let preambleFuncs = preambleCtx.SymbolicFunctions
            let preambleFuncNameSet =
                preambleFuncs |> List.map (fun f -> f.Name) |> Set.ofList
            let externalHashes =
                if stdlib.CacheSettings.Enabled then
                    preambleCtx.PreambleFunctionDependencyHashes
                    |> Map.fold (fun acc name hash -> Map.add name hash acc) stdlib.StdlibFunctionDependencyHashes
                else
                    Map.empty
            stdlib, preambleCtx.Context, preambleFuncs, preambleFuncNameSet, externalHashes

    let emitFunctionEvents, treeShakeUserFunctions =
        match request.Mode with
        | FullProgram -> false, false
        | TestExpression -> true, true

    let monomorphization =
        match request.Mode with
        | FullProgram -> Monomorphize (Some baseContext.GenericFuncDefs)
        | TestExpression -> SpecializeLocalAndReplace baseContext.SpecRegistry

    {
        AllowInternal = request.AllowInternal
        Verbosity = request.Verbosity
        Options = request.Options
        PassTimingRecorder = request.PassTimingRecorder
        CacheMissRecorder = request.CacheMissRecorder
        Stdlib = stdlib
        BaseContext = baseContext
        Monomorphization = monomorphization
        PrebuiltSymbolicFunctions = prebuiltSymbolic
        SkipFunctionNames = skipNames
        ExternalFunctionDependencyHashes = externalDependencyHashes
        EmitFunctionEvents = emitFunctionEvents
        TreeShakeUserFunctions = treeShakeUserFunctions
        Labels = labelsForMode request.Mode
        SourceFile = request.SourceFile
        Source = request.Source
    }

/// Compile source code to binary (in-memory, no file I/O)
let compile (request: CompileRequest) : CompileReport =
    let plan = buildCompilePlan request
    compileUserWithPlan plan

/// Execute compiled binary and capture output
let execute (verbosity: int) (binary: byte array) : ExecutionOutput =
    let sw = Stopwatch.StartNew()
    let finish (exitCode: int) (stdout: string) (stderr: string) : ExecutionOutput =
        sw.Stop()
        { ExitCode = exitCode
          Stdout = stdout
          Stderr = stderr
          RuntimeTime = sw.Elapsed }

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

    let result =
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
                finish -1 "" errorMsg
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
                    finish -1 "" $"Failed to start process: {msg}"
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

                    if verbosity >= 1 then
                        println $"  ✓ Execution complete ({System.Math.Round(sw.Elapsed.TotalMilliseconds, 1)}ms)"

                    finish proc.ExitCode stdout stderr
        finally
            // Cleanup - ignore deletion errors
            tryDeleteFile tempPath
    result

/// Get all stdlib function names from the prebuilt stdlib
let getAllStdlibFunctionNamesFromStdlib (stdlib: StdlibResult) : Set<string> =
    stdlib.StdlibANFFunctions |> Map.keys |> Set.ofSeq

/// Get the set of stdlib function names reachable from user code (using prebuilt stdlib)
/// Used for coverage analysis without re-compiling stdlib
let getReachableStdlibFunctionsFromStdlib (stdlib: StdlibResult) (source: string) : Result<Set<string>, string> =
    // Parse user code
    match Parser.parseString false source with
    | Error err -> Error $"Parse error: {err}"
    | Ok userAst ->
        // Type check with stdlib environment
        match TypeChecking.checkProgramWithBaseEnv stdlib.Context.TypeCheckEnv userAst with
        | Error typeErr -> Error $"Type error: {TypeChecking.typeErrorToString typeErr}"
        | Ok (programType, typedUserAst, _) ->
            // Convert to ANF
            match convertTypedProgramToUserOnly stdlib.Context typedUserAst with
            | Error err -> Error $"ANF conversion error: {err}"
            | Ok userOnly ->
                let coverageOptions = { defaultOptions with DisableANFOpt = true; DisableInlining = true }
                let sw = Stopwatch.StartNew()
                let entryFunction =
                    AST_to_ANF.synthesizeEntryFunction "_start" programType userOnly.MainExpr
                let userRegistries : AST_to_ANF.Registries = {
                    TypeReg = userOnly.TypeReg
                    VariantLookup = userOnly.VariantLookup
                    FuncReg = userOnly.FuncReg
                    FuncParams = userOnly.FuncParams
                    ModuleRegistry = userOnly.ModuleRegistry
                }
                match buildAnf 0 coverageOptions sw userRegistries (entryFunction :: userOnly.UserFunctions) None with
                | Error err -> Error err
                | Ok (userFunctions, _typeMap) ->
                    match PrintInsertion.insertPrintInEntry "_start" programType userFunctions with
                    | Error err -> Error $"Print insertion error: {err}"
                    | Ok printedFunctions ->
                        let tcoFunctions = applyTco 0 coverageOptions sw printedFunctions None
                        let reachableStdlibNames =
                            ANFDeadCodeElimination.getReachableStdlib stdlib.StdlibANFCallGraph tcoFunctions
                        Ok reachableStdlibNames
