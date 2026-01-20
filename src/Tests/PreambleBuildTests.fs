// PreambleBuildTests.fs - Unit tests for building E2E preamble contexts
//
// Ensures preamble contexts are built once per file and reused during tests.

module PreambleBuildTests

open System
open System.IO
open CompilerLibrary
open TestDSL.E2EFormat
open TestDSL.E2ETestRunner
open StdlibTestHarness

/// Test result type
type TestResult = Result<unit, string>

let private makeTest (name: string) (source: string) (preamble: string) (sourceFile: string) : E2ETest =
    {
        Name = name
        Source = source
        Preamble = preamble
        ExpectedStdout = None
        ExpectedStderr = None
        ExpectedExitCode = 0
        ExpectCompileError = false
        ExpectedErrorMessage = None
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
        DisableLeakCheck = false
        SourceFile = sourceFile
        FunctionLineMap = Map.empty
    }

let testPreambleBuildSucceeds (sharedStdlib: StdlibResult) : TestResult =
    let stdlib = sharedStdlib
    let preamble = "def add(x: Int64, y: Int64) : Int64 = x + y"
    let tests = [
        makeTest "preamble-build-1" "add(1, 2)" preamble "preamble-build.e2e"
        makeTest "preamble-build-2" "add(3, 4)" preamble "preamble-build.e2e"
    ]

    match buildPreambles stdlib tests with
    | Error err -> Error $"Preamble build failed: {err}"
    | Ok () -> Ok ()

let testPreambleBuiltOnce (sharedStdlib: StdlibResult) : TestResult =
    let stdlib = sharedStdlib
    let preamble = "def add(x: Int64, y: Int64) : Int64 = x + y"
    let tests = [
        makeTest "preamble-once-1" "add(1, 2)" preamble "preamble-once.e2e"
        makeTest "preamble-once-2" "add(3, 4)" preamble "preamble-once.e2e"
    ]

    let originalEnv = Environment.GetEnvironmentVariable "DARK_TRACE_COMPILER"
    Environment.SetEnvironmentVariable("DARK_TRACE_COMPILER", "1")
    let originalError = Console.Error
    use writer = new StringWriter()
    Console.SetError(writer)
    let result =
        try
            match runE2ETestsWithPreambleContexts stdlib (tests |> List.toArray) with
            | Error err -> Error err
            | Ok _ -> Ok ()
        finally
            Console.SetError(originalError)
            Environment.SetEnvironmentVariable("DARK_TRACE_COMPILER", originalEnv)

    match result with
    | Error err -> Error err
    | Ok () ->
        let trace = writer.ToString()
        let count =
            trace.Split('\n', StringSplitOptions.RemoveEmptyEntries)
            |> Array.filter (fun line -> line.StartsWith("TRACE event=preamble.compile.start"))
            |> Array.length
        if count = 1 then
            Ok ()
        else
            Error $"Expected preamble to compile once, but saw {count} compilations"

let tests : (string * (StdlibResult -> TestResult)) list = [
    ("preamble build succeeds", testPreambleBuildSucceeds)
    ("preamble built once", testPreambleBuiltOnce)
]

let testsWithStdlib (sharedStdlib: StdlibResult) : (string * (unit -> TestResult)) list =
    tests |> List.map (fun (name, test) -> (name, fun () -> test sharedStdlib))

/// Run all preamble build tests
let runAllWithStdlib (sharedStdlib: StdlibResult) : TestResult =
    let rec runTests = function
        | [] -> Ok ()
        | (name, test) :: rest ->
            match test () with
            | Ok () -> runTests rest
            | Error msg -> Error $"{name} test failed: {msg}"

    runTests (testsWithStdlib sharedStdlib)

let runAll () : TestResult =
    match StdlibTestHarness.compileStdlib() with
    | Error err -> Error $"Stdlib compile failed: {err}"
    | Ok stdlib -> runAllWithStdlib stdlib
