// CompilerCachingTests.fs - Placeholder for removed caching tests
//
// Caching has been removed from the compiler.
// These tests previously verified caching behavior.

module CompilerCachingTests

open CompilerLibrary

/// Test result type
type TestResult = Result<unit, string>

/// Test that stdlib compilation succeeds (replaces parallel mode test)
let testStdlibCompileSucceeds (sharedStdlib: StdlibResult) : TestResult =
    // Just verify that stdlib compiled successfully
    Ok ()

let tests (verificationEnabled: bool) : (string * (StdlibResult -> TestResult)) list = [
    ("stdlib compile succeeds", testStdlibCompileSucceeds)
]

let testsWithStdlib (sharedStdlib: StdlibResult) : (string * (unit -> TestResult)) list =
    tests false |> List.map (fun (name, test) -> (name, fun () -> test sharedStdlib))

/// Run all compiler caching unit tests
let runAllWithStdlib (sharedStdlib: StdlibResult) : TestResult =
    let rec runTests = function
        | [] -> Ok ()
        | (name, test) :: rest ->
            match test () with
            | Ok () -> runTests rest
            | Error msg -> Error $"{name} test failed: {msg}"

    runTests (testsWithStdlib sharedStdlib)

let runAll () : TestResult =
    match StdlibTestHarness.compileStdlib () with
    | Error err -> Error $"Stdlib compile failed: {err}"
    | Ok stdlib -> runAllWithStdlib stdlib
