// E2EFormatTests.fs - Unit tests for E2E DSL parsing
//
// Verifies parser behavior for multi-line E2E test forms used by upstream .dark files.

module E2EFormatTests

open System
open System.IO
open TestDSL.E2EFormat

type TestResult = Result<unit, string>

let private withTempFile (contents: string) (f: string -> TestResult) : TestResult =
    let tempDir = Path.Combine(Path.GetTempPath(), $"dark-e2eformat-{Guid.NewGuid():N}")
    let tempPath = Path.Combine(tempDir, "test.dark")

    try
        Directory.CreateDirectory(tempDir) |> ignore
        File.WriteAllText(tempPath, contents)
        f tempPath
    finally
        if Directory.Exists(tempDir) then
            Directory.Delete(tempDir, true)

let testParsesMultilineExpectationOnNextLine () : TestResult =
    let testSource =
        "(if true then Builtin.testRuntimeError \"a\" else 0L) =\n"
        + "  Builtin.testDerrorMessage \"Uncaught exception: a\"\n"

    withTempFile testSource (fun path ->
        match parseE2ETestFile path with
        | Error msg ->
            Error $"Expected multiline .dark-style test to parse, but got error: {msg}"
        | Ok tests ->
            match tests with
            | [ test ] ->
                if not (test.Source.Contains("Builtin.testRuntimeError")) then
                    Error $"Expected parsed source to contain runtime error expression, got: {test.Source}"
                elif test.ExpectedStdout <> Some "Builtin.testDerrorMessage \"Uncaught exception: a\"\n" then
                    Error "Expected parser to treat next-line expectation as stdout"
                else
                    Ok ()
            | _ ->
                Error $"Expected exactly 1 parsed test, got {tests.Length}")

let tests = [
    ("parses multiline expectation on next line", testParsesMultilineExpectationOnNextLine)
]
