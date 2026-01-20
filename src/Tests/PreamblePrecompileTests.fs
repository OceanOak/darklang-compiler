// PreamblePrecompileTests.fs - Unit tests for precompiling E2E preambles
//
// Ensures precompilation populates the stdlib preamble cache.

module PreamblePrecompileTests

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

let testPrecompileSucceeds (sharedStdlib: StdlibResult) : TestResult =
    let stdlib = StdlibTestHarness.resetCaches sharedStdlib
    let preamble = "def add(x: Int64, y: Int64) : Int64 = x + y"
    let tests = [
        makeTest "precompile-1" "add(1, 2)" preamble "precompile.e2e"
        makeTest "precompile-2" "add(3, 4)" preamble "precompile.e2e"
    ]

    match precompilePreambles stdlib tests with
    | Error err -> Error $"Precompile failed: {err}"
    | Ok () -> Ok ()

let testPreambleCompiledOnce (sharedStdlib: StdlibResult) : TestResult =
    let stdlib = StdlibTestHarness.resetCaches sharedStdlib
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
            match runE2ETestsWithCompiledPreambles stdlib (tests |> List.toArray) with
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
    ("precompile succeeds", testPrecompileSucceeds)
    ("preamble compiled once", testPreambleCompiledOnce)
]

let testsWithStdlib (sharedStdlib: StdlibResult) : (string * (unit -> TestResult)) list =
    tests |> List.map (fun (name, test) -> (name, fun () -> test sharedStdlib))

/// Run all precompile tests
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
