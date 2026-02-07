// E2ETestRunner.fs - End-to-end test runner
//
// Compiles source code, executes it, and validates output/exit code.

module TestDSL.E2ETestRunner

open System
open AST
open AST_to_ANF
open Parser
open TypeChecking
open TestDSL.E2EFormat

// Internal identifiers are only allowed for stdlib-internal tests.
let private isInternalTestFile (sourceFile: string) : bool =
    let normalized = sourceFile.Replace('\\', '/')
    normalized.Contains("/stdlib-internal/")

/// Result of running an E2E test
type E2ERun =
    | CompileFailed of exitCode:int * error:string * compileTime:TimeSpan
    | Ran of exitCode:int * stdout:string * stderr:string * compileTime:TimeSpan * runtimeTime:TimeSpan

type E2EFailure = {
    Run: E2ERun
    Message: string
}

type E2ETestResult = Result<E2ERun, E2EFailure>

type private PreambleBuildSpec = {
    SourceFile: string
    Preamble: string
    FunctionLineMap: Map<string, int>
    AllowInternal: bool
}

/// Map of built preamble contexts keyed by source file
type PreambleContextMap = Map<string, CompilerLibrary.PreambleContext>

type SuiteContext = {
    Stdlib: CompilerLibrary.StdlibResult
    PreambleContexts: PreambleContextMap
}

type private PreamblePlan = {
    Spec: PreambleBuildSpec
    Analysis: CompilerLibrary.PreambleAnalysis option
    Specialization: SpecializationResult
}

let private buildPreambleBuildSpec (sourceFile: string) (tests: E2ETest list) : Result<PreambleBuildSpec, string> =
    let preambles =
        tests
        |> List.map (fun test -> test.Preamble)
        |> List.distinct
    match preambles with
    | [preamble] ->
        let funcLineMaps =
            tests
            |> List.map (fun test -> test.FunctionLineMap)
            |> List.distinct
        match funcLineMaps with
        | [funcLineMap] ->
            Ok {
                SourceFile = sourceFile
                Preamble = preamble
                FunctionLineMap = funcLineMap
                AllowInternal = isInternalTestFile sourceFile
            }
        | _ ->
            Error $"Multiple function line maps found for {sourceFile}"
    | _ ->
        Error $"Multiple preambles found for {sourceFile}"

let private collectTypeAppsFromProgram (program: Program) : Set<SpecKey> =
    let (Program topLevels) = program
    topLevels
    |> List.map (function
        | FunctionDef f when List.isEmpty f.TypeParams -> collectTypeAppsFromFunc f
        | Expression e -> collectTypeApps e
        | _ -> Set.empty)
    |> List.fold Set.union Set.empty

let private filterSpecsByDefs (genericDefs: GenericFuncDefs) (specs: Set<SpecKey>) : Set<SpecKey> =
    specs |> Set.filter (fun (funcName, _) -> Map.containsKey funcName genericDefs)

let private collectSpecsFromTests
    (allowInternal: bool)
    (typeCheckEnv: TypeCheckEnv)
    (tests: E2ETest list)
    : Result<Set<SpecKey>, string> =
    let testsToAnalyze =
        tests
        |> List.filter (fun test -> not test.ExpectCompileError)
    let rec loop remaining acc =
        match remaining with
        | [] -> Ok acc
        | test :: rest ->
            parseString allowInternal test.Source
            |> Result.mapError (fun err ->
                $"Test parse error ({test.SourceFile}: {test.Name}): {err}")
            |> Result.bind (fun testAst ->
                checkProgramWithBaseEnv typeCheckEnv testAst
                |> Result.mapError (fun err ->
                    $"Test type error ({test.SourceFile}: {test.Name}): {typeErrorToString err}")
                |> Result.map (fun (_programType, typedAst, _) -> collectTypeAppsFromProgram typedAst))
            |> Result.bind (fun specs ->
                loop rest (Set.union acc specs))
    loop testsToAnalyze Set.empty

let private buildPreamblePlan
    (stdlib: CompilerLibrary.StdlibResult)
    (spec: PreambleBuildSpec)
    (tests: E2ETest list)
    : Result<PreamblePlan * Set<SpecKey>, string> =
    let preambleIsEmpty = String.IsNullOrWhiteSpace spec.Preamble
    let analysisResult =
        if preambleIsEmpty then
            Ok None
        else
            CompilerLibrary.analyzePreamble spec.AllowInternal stdlib spec.Preamble
            |> Result.map Some

    analysisResult
    |> Result.bind (fun analysisOpt ->
        let typeCheckEnv =
            match analysisOpt with
            | Some analysis -> analysis.TypeCheckEnv
            | None -> stdlib.Context.TypeCheckEnv
        collectSpecsFromTests spec.AllowInternal typeCheckEnv tests
        |> Result.bind (fun testSpecs ->
            let preambleSpecs =
                match analysisOpt with
                | None -> Set.empty
                | Some analysis -> collectTypeAppsFromProgram analysis.TypedAST
            let combinedSpecs = Set.union testSpecs preambleSpecs
            let preambleGenericDefs =
                match analysisOpt with
                | None -> Map.empty
                | Some analysis -> analysis.GenericFuncDefs
            let preambleSpecsForDefs = filterSpecsByDefs preambleGenericDefs combinedSpecs
            let stdlibSpecsFromCombined = filterSpecsByDefs stdlib.Context.GenericFuncDefs combinedSpecs
            let specialization =
                if Map.isEmpty preambleGenericDefs then
                    {
                        SpecializedFuncs = []
                        SpecRegistry = Map.empty
                        ExternalSpecs = Set.empty
                    }
                else
                    specializeFromSpecs preambleGenericDefs preambleSpecsForDefs
            let stdlibSpecsFromSpecialization =
                filterSpecsByDefs stdlib.Context.GenericFuncDefs specialization.ExternalSpecs
            let stdlibSpecs = Set.union stdlibSpecsFromCombined stdlibSpecsFromSpecialization
            let plan = {
                Spec = spec
                Analysis = analysisOpt
                Specialization = specialization
            }
            Ok (plan, stdlibSpecs)))

/// Build suite stdlib specializations and per-file preamble contexts
let buildSuiteContexts
    (stdlib: CompilerLibrary.StdlibResult)
    (tests: E2ETest array)
    (passTimingRecorder: CompilerLibrary.PassTimingRecorder option)
    : Result<SuiteContext, string> =
    let groupedTests =
        tests
        |> Array.toList
        |> List.groupBy (fun test -> test.SourceFile)

    let plansResult =
        groupedTests
        |> List.fold
            (fun acc (sourceFile, group) ->
                acc
                |> Result.bind (fun (plans, stdlibSpecs) ->
                    buildPreambleBuildSpec sourceFile group
                    |> Result.bind (fun spec ->
                        buildPreamblePlan stdlib spec group
                        |> Result.map (fun (plan, specs) ->
                            (plan :: plans, Set.union stdlibSpecs specs)))))
            (Ok ([], Set.empty))

    plansResult
    |> Result.bind (fun (plans, stdlibSpecs) ->
        CompilerLibrary.buildStdlibSpecializations stdlib stdlibSpecs passTimingRecorder
        |> Result.bind (fun stdlibWithSpecs ->
            let buildEmptyContext () : CompilerLibrary.PreambleContext =
                {
                    Context = stdlibWithSpecs.Context
                    ANFFunctions = []
                    TypeMap = stdlibWithSpecs.StdlibTypeMap
                    SymbolicFunctions = []
                }
            let contextsResult =
                plans
                |> List.fold
                    (fun acc plan ->
                        acc
                        |> Result.bind (fun (currentStdlib, contexts) ->
                            let ctxResult =
                                match plan.Analysis with
                                | None -> Ok (currentStdlib, buildEmptyContext ())
                                | Some analysis ->
                                    CompilerLibrary.buildPreambleContextFromAnalysis
                                        currentStdlib
                                        analysis
                                        plan.Specialization
                                        plan.Spec.SourceFile
                                        plan.Spec.FunctionLineMap
                                        passTimingRecorder
                                    |> Result.mapError (fun err ->
                                        $"Preamble build error ({plan.Spec.SourceFile}): {err}")
                            ctxResult
                            |> Result.map (fun (updatedStdlib, ctx) ->
                                (updatedStdlib, Map.add plan.Spec.SourceFile ctx contexts))))
                    (Ok (stdlibWithSpecs, Map.empty))
            contextsResult
            |> Result.map (fun (updatedStdlib, contexts) ->
                {
                    Stdlib = updatedStdlib
                    PreambleContexts = contexts
                })))

let private exitCodeFromRun (run: E2ERun) : int =
    match run with
    | CompileFailed (exitCode, _, _) -> exitCode
    | Ran (exitCode, _, _, _, _) -> exitCode

let private stdoutFromRun (run: E2ERun) : string =
    match run with
    | CompileFailed _ -> ""
    | Ran (_, stdout, _, _, _) -> stdout

let private stderrFromRun (run: E2ERun) : string =
    match run with
    | CompileFailed (_, error, _) -> error
    | Ran (_, _, stderr, _, _) -> stderr

let private failRun (run: E2ERun) (message: string) : E2ETestResult =
    Error { Run = run; Message = message }

let private evaluateExpectations (test: E2ETest) (run: E2ERun) : E2ETestResult =
    if test.ExpectCompileError then
        let gotError = exitCodeFromRun run <> 0
        if not gotError then
            failRun run "Expected compilation error but compilation succeeded"
        else
            match test.ExpectedErrorMessage with
            | Some expectedMsg ->
                let output = stderrFromRun run
                if output.Contains(expectedMsg) then
                    Ok run
                else
                    failRun run $"Expected error message '{expectedMsg}' not found in stderr"
            | None ->
                Ok run
    else
        let stdoutMatches =
            match test.ExpectedStdout with
            | None -> true
            | Some expected ->
                let actual = stdoutFromRun run
                actual.Trim() = expected.Trim()

        let stderrMatches =
            match test.ExpectedStderr with
            | None -> true
            | Some expected ->
                let actual = stderrFromRun run
                actual.Trim() = expected.Trim()

        let exitCodeMatches = exitCodeFromRun run = test.ExpectedExitCode

        if stdoutMatches && stderrMatches && exitCodeMatches then
            Ok run
        else
            failRun run "Output mismatch"

let private buildCompilerOptions (test: E2ETest) : CompilerLibrary.CompilerOptions =
    { CompilerLibrary.defaultOptions with
        DisableFreeList = test.DisableFreeList
        DisableANFOpt = test.DisableANFOpt
        DisableANFConstFolding = test.DisableANFConstFolding
        DisableANFConstProp = test.DisableANFConstProp
        DisableANFCopyProp = test.DisableANFCopyProp
        DisableANFDCE = test.DisableANFDCE
        DisableANFStrengthReduction = test.DisableANFStrengthReduction
        DisableInlining = test.DisableInlining
        DisableTCO = test.DisableTCO
        DisableMIROpt = test.DisableMIROpt
        DisableMIRConstFolding = test.DisableMIRConstFolding
        DisableMIRCSE = test.DisableMIRCSE
        DisableMIRCopyProp = test.DisableMIRCopyProp
        DisableMIRDCE = test.DisableMIRDCE
        DisableMIRCFGSimplify = test.DisableMIRCFGSimplify
        DisableMIRLICM = test.DisableMIRLICM
        DisableLIROpt = test.DisableLIROpt
        DisableLIRPeephole = test.DisableLIRPeephole
        DisableFunctionTreeShaking = test.DisableFunctionTreeShaking
        EnableCoverage = false
        EnableLeakCheck = not test.DisableLeakCheck
        DumpANF = false
        DumpMIR = false
        DumpLIR = false
    }

let private tryExecuteBinary (binary: byte array) : Result<CompilerLibrary.ExecutionOutput, string> =
    try Ok (CompilerLibrary.execute 0 binary)
    with ex -> Error ex.Message

let private compileAndRun (request: CompilerLibrary.CompileRequest) : E2ERun =
    let compileReport = CompilerLibrary.compile request
    match compileReport.Result with
    | Error err ->
        CompileFailed (1, err, compileReport.CompileTime)
    | Ok binary ->
        match tryExecuteBinary binary with
        | Ok execResult ->
            Ran (execResult.ExitCode, execResult.Stdout, execResult.Stderr, compileReport.CompileTime, execResult.RuntimeTime)
        | Error err ->
            Ran (-1, "", $"Execution failed: {err}", compileReport.CompileTime, TimeSpan.Zero)

/// Run E2E test using a prebuilt preamble context
let runE2ETestWithPreambleContext
    (stdlib: CompilerLibrary.StdlibResult)
    (preambleCtx: CompilerLibrary.PreambleContext)
    (test: E2ETest)
    (passTimingRecorder: CompilerLibrary.PassTimingRecorder option)
    : E2ETestResult =
    let options = buildCompilerOptions test
    let allowInternal = isInternalTestFile test.SourceFile
    let request : CompilerLibrary.CompileRequest = {
        Context = CompilerLibrary.StdlibWithPreamble (stdlib, preambleCtx)
        Mode = CompilerLibrary.CompileMode.TestExpression
        SourceSyntax = CompilerLibrary.CompilerSyntax
        Source = test.Source
        SourceFile = test.SourceFile
        AllowInternal = allowInternal
        Verbosity = 0
        Options = options
        PassTimingRecorder = passTimingRecorder
    }
    let run = compileAndRun request
    evaluateExpectations test run
