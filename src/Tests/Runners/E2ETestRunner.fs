// E2ETestRunner.fs - End-to-end test runner
//
// Compiles source code, executes it, and validates output/exit code.

module TestDSL.E2ETestRunner

open System
open TestDSL.E2EFormat
open StdlibTestHarness
open Trace

let private isInternalTestFile (sourceFile: string) : bool =
    let normalized = sourceFile.Replace('\\', '/')
    normalized.Contains("/stdlib-internal/")

/// Result of running an E2E test
type E2ETestResult = {
    Success: bool
    Message: string
    Stdout: string option
    Stderr: string option
    ExitCode: int option
    CompileTime: TimeSpan
    RuntimeTime: TimeSpan
}

/// Compile stdlib once (call at test startup, pass result to runE2ETest)
/// Uses prebuilt stdlib - all functions compiled to LIR upfront for maximum speed
let compileStdlib () : Result<CompilerLibrary.StdlibResult, string> =
    StdlibTestHarness.compileStdlib()

/// Preamble identity: (source file, preamble text)
type PreambleKey = string * string

/// Map of built preamble contexts keyed by preamble identity
type PreambleContextMap = Map<PreambleKey, CompilerLibrary.PreambleContext>

/// Build a single preamble context
let compilePreambleContext (stdlib: CompilerLibrary.StdlibResult) (sourceFile: string) (preamble: string) (funcLineMap: Map<string, int>) : Result<CompilerLibrary.PreambleContext, string> =
    let allowInternal = isInternalTestFile sourceFile
    emit "preamble.build.start" [("source", sourceFile)]
    match CompilerLibrary.compilePreambleWithOptions allowInternal stdlib preamble sourceFile funcLineMap with
    | Error err ->
        let msg = $"Preamble build error ({sourceFile}): {err}"
        emit "preamble.build.error" [("source", sourceFile); ("message", msg)]
        Error msg
    | Ok ctx ->
        emit "preamble.build.finish" [("source", sourceFile)]
        Ok ctx

/// Build all distinct preambles (by file + preamble text) and return contexts
let buildPreambleContexts (stdlib: CompilerLibrary.StdlibResult) (tests: E2ETest array) : Result<PreambleContextMap, string> =
    let grouped =
        tests
        |> Array.toList
        |> List.groupBy (fun test -> (test.SourceFile, test.Preamble))
    let rec compileAll remaining acc =
        match remaining with
        | [] -> Ok (Map.ofList acc)
        | ((sourceFile, preamble), group) :: rest ->
            let funcLineMap =
                group
                |> List.tryHead
                |> Option.map (fun test -> test.FunctionLineMap)
                |> Option.defaultValue Map.empty
            match compilePreambleContext stdlib sourceFile preamble funcLineMap with
            | Error err -> Error err
            | Ok ctx ->
                compileAll rest (((sourceFile, preamble), ctx) :: acc)
    compileAll grouped []

/// Build all distinct preambles (by file + preamble text) and discard contexts
let buildPreambles (stdlib: CompilerLibrary.StdlibResult) (tests: E2ETest list) : Result<unit, string> =
    match buildPreambleContexts stdlib (List.toArray tests) with
    | Error err -> Error err
    | Ok _ -> Ok ()

let private buildE2EResult (test: E2ETest) (execResult: CompilerLibrary.TimedExecutionResult) : E2ETestResult =
    if test.ExpectCompileError then
        let gotError = execResult.ExitCode <> 0
        if not gotError then
            { Success = false
              Message = "Expected compilation error but compilation succeeded"
              Stdout = Some execResult.Stdout
              Stderr = Some execResult.Stderr
              ExitCode = Some execResult.ExitCode
              CompileTime = execResult.CompileTime
              RuntimeTime = execResult.RuntimeTime }
        else
            match test.ExpectedErrorMessage with
            | Some expectedMsg ->
                let output = execResult.Stderr
                if output.Contains(expectedMsg) then
                    { Success = true
                      Message = "Compilation failed with expected error message"
                      Stdout = Some execResult.Stdout
                      Stderr = Some execResult.Stderr
                      ExitCode = Some execResult.ExitCode
                      CompileTime = execResult.CompileTime
                      RuntimeTime = execResult.RuntimeTime }
                else
                    { Success = false
                      Message = $"Expected error message '{expectedMsg}' not found in stderr"
                      Stdout = Some execResult.Stdout
                      Stderr = Some execResult.Stderr
                      ExitCode = Some execResult.ExitCode
                      CompileTime = execResult.CompileTime
                      RuntimeTime = execResult.RuntimeTime }
            | None ->
                { Success = true
                  Message = "Compilation failed as expected"
                  Stdout = Some execResult.Stdout
                  Stderr = Some execResult.Stderr
                  ExitCode = Some execResult.ExitCode
                  CompileTime = execResult.CompileTime
                  RuntimeTime = execResult.RuntimeTime }
    else
        let stdoutMatches =
            match test.ExpectedStdout with
            | None -> true
            | Some expected -> execResult.Stdout.Trim() = expected.Trim()

        let stderrMatches =
            match test.ExpectedStderr with
            | None -> true
            | Some expected -> execResult.Stderr.Trim() = expected.Trim()

        let exitCodeMatches = execResult.ExitCode = test.ExpectedExitCode

        let success = stdoutMatches && stderrMatches && exitCodeMatches

        { Success = success
          Message = if success then "Test passed" else "Output mismatch"
          Stdout = Some execResult.Stdout
          Stderr = Some execResult.Stderr
          ExitCode = Some execResult.ExitCode
          CompileTime = execResult.CompileTime
          RuntimeTime = execResult.RuntimeTime }

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

/// Run E2E test (internal implementation)
let private runE2ETestInternal (stdlib: CompilerLibrary.StdlibResult) (test: E2ETest) : E2ETestResult =
    try
        emit "test.start" [("source", test.SourceFile); ("name", test.Name)]
        let options = buildCompilerOptions test
        let allowInternal = isInternalTestFile test.SourceFile
        let execResult =
            CompilerLibrary.compileAndRunWithPreambleTimedWithOptions allowInternal 0 options stdlib test.Source test.Preamble test.SourceFile test.FunctionLineMap

        let result = buildE2EResult test execResult
        emit "test.finish" [("source", test.SourceFile); ("name", test.Name); ("success", string result.Success)]
        result
    with
    | ex ->
        let result =
            { Success = false
              Message = $"Test execution failed: {ex.Message}"
              Stdout = None
              Stderr = None
              ExitCode = None
              CompileTime = TimeSpan.Zero
              RuntimeTime = TimeSpan.Zero }
        emit "test.finish" [("source", test.SourceFile); ("name", test.Name); ("success", string result.Success)]
        result

/// Run E2E test using a prebuilt preamble context
let runE2ETestWithPreambleContext
    (stdlib: CompilerLibrary.StdlibResult)
    (preambleCtx: CompilerLibrary.PreambleContext)
    (test: E2ETest)
    : E2ETestResult =
    try
        emit "test.start" [("source", test.SourceFile); ("name", test.Name)]
        let options = buildCompilerOptions test
        let allowInternal = isInternalTestFile test.SourceFile
        let execResult =
            CompilerLibrary.compileAndRunWithPreambleContextTimedWithOptions allowInternal 0 options stdlib preambleCtx test.Source test.SourceFile
        let result = buildE2EResult test execResult
        emit "test.finish" [("source", test.SourceFile); ("name", test.Name); ("success", string result.Success)]
        result
    with
    | ex ->
        let result =
            { Success = false
              Message = $"Test execution failed: {ex.Message}"
              Stdout = None
              Stderr = None
              ExitCode = None
              CompileTime = TimeSpan.Zero
              RuntimeTime = TimeSpan.Zero }
        emit "test.finish" [("source", test.SourceFile); ("name", test.Name); ("success", string result.Success)]
        result

/// Run E2E test with prebuilt stdlib
let runE2ETest (stdlib: CompilerLibrary.StdlibResult) (test: E2ETest) : E2ETestResult =
    runE2ETestInternal stdlib test

/// Run E2E tests with built preamble contexts
let runE2ETestsWithPreambleContexts
    (stdlib: CompilerLibrary.StdlibResult)
    (tests: E2ETest array)
    : Result<(E2ETest * E2ETestResult) array, string> =
    match buildPreambleContexts stdlib tests with
    | Error err -> Error err
    | Ok preambleContexts ->
        let rec runAll remaining acc =
            match remaining with
            | [] -> Ok (List.rev acc |> List.toArray)
            | test :: rest ->
                let key = (test.SourceFile, test.Preamble)
                match Map.tryFind key preambleContexts with
                | None -> Error $"Missing built preamble context for {test.SourceFile}"
                | Some ctx ->
                    let result = runE2ETestWithPreambleContext stdlib ctx test
                    runAll rest ((test, result) :: acc)
        runAll (tests |> Array.toList) []
