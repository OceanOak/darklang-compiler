// E2ETestRunner.fs - End-to-end test runner
//
// Compiles source code, executes it, and validates output/exit code.

module TestDSL.E2ETestRunner

open System
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

let private buildPreambleContextFromSpec
    (stdlib: CompilerLibrary.StdlibResult)
    (spec: PreambleBuildSpec)
    : Result<CompilerLibrary.PreambleContext, string> =
    match CompilerLibrary.buildPreambleContext spec.AllowInternal stdlib spec.Preamble spec.SourceFile spec.FunctionLineMap with
    | Error err ->
        Error $"Preamble build error ({spec.SourceFile}): {err}"
    | Ok ctx ->
        Ok ctx

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

/// Build all distinct preambles (by source file) and return contexts
/// Each source file must produce exactly one preamble and function line map.
let buildPreambleContexts
    (stdlib: CompilerLibrary.StdlibResult)
    (tests: E2ETest array)
    : Result<PreambleContextMap, string> =
    tests
    |> Array.toList
    |> List.groupBy (fun test -> test.SourceFile)
    |> List.fold
        (fun acc (sourceFile, group) ->
            acc
            |> Result.bind (fun contexts ->
                buildPreambleBuildSpec sourceFile group
                |> Result.bind (fun spec ->
                    buildPreambleContextFromSpec stdlib spec
                    |> Result.map (fun ctx -> Map.add sourceFile ctx contexts))))
        (Ok Map.empty)

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
    : E2ETestResult =
    let options = buildCompilerOptions test
    let allowInternal = isInternalTestFile test.SourceFile
    let request : CompilerLibrary.CompileRequest = {
        Context = CompilerLibrary.StdlibWithPreamble (stdlib, preambleCtx)
        Mode = CompilerLibrary.TestExpression
        Source = test.Source
        SourceFile = test.SourceFile
        AllowInternal = allowInternal
        Verbosity = 0
        Options = options
    }
    let run = compileAndRun request
    evaluateExpectations test run
