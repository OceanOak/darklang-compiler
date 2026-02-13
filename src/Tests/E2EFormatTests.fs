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

let testParsesSkipAttribute () : TestResult =
    let testSource = "(if true then 1L else 2L) = skip=\"temporarily unsupported\"\n"

    withTempFile testSource (fun path ->
        match parseE2ETestFile path with
        | Error msg ->
            Error $"Expected skip attribute test to parse, but got error: {msg}"
        | Ok tests ->
            match tests with
            | [ test ] ->
                match test.SkipReason with
                | Some reason when reason = "temporarily unsupported" ->
                    Ok ()
                | Some reason ->
                    Error $"Expected skip reason 'temporarily unsupported', got '{reason}'"
                | None ->
                    Error "Expected skip reason to be parsed"
            | _ ->
                Error $"Expected exactly 1 parsed test, got {tests.Length}"
    )

let testParsesIndentedMultilineDarkTestsInsideModule () : TestResult =
    let testSource =
        "module Int64 =\n"
        + "  (match 6L with\n"
        + "   | 6L -> \"pass\"\n"
        + "   | _ -> \"fail\") = \"pass\"\n"

    withTempFile testSource (fun path ->
        match parseE2ETestFile path with
        | Error msg ->
            Error $"Expected indented .dark multiline test to parse, but got error: {msg}"
        | Ok tests ->
            match tests with
            | [ test ] ->
                if test.Source <> "match 6L with\n   | 6L -> \"pass\"\n   | _ -> \"fail\"" then
                    Error $"Unexpected parsed source: {test.Source}"
                elif test.Preamble.Trim().Length <> 0 then
                    Error $"Expected .dark module declarations to be excluded from preamble, got: {test.Preamble}"
                else
                    Ok ()
            | _ ->
                Error $"Expected exactly 1 parsed test, got {tests.Length}")

let testParsesMultilineWithInnerDoubleEquals () : TestResult =
    let testSource =
        "(match 5L with\n"
        + " | x when (x + 1L) == 6L -> true\n"
        + " | 5L -> false) =\n"
        + "  true\n"

    withTempFile testSource (fun path ->
        match parseE2ETestFile path with
        | Error msg ->
            Error $"Expected multiline test with inner == to parse, but got error: {msg}"
        | Ok tests ->
            if tests.Length <> 1 then
                Error $"Expected exactly 1 parsed test, got {tests.Length}"
            else
                Ok ())

let testParsesMultilineWithInnerLetBinding () : TestResult =
    let testSource =
        "(let x = 4L\n"
        + " match x with\n"
        + " | 1L | 2L | 3L | 4L -> \"pass\"\n"
        + " | _ -> \"fail\") = \"pass\"\n"

    withTempFile testSource (fun path ->
        match parseE2ETestFile path with
        | Error msg ->
            Error $"Expected multiline let-binding test to parse, but got error: {msg}"
        | Ok tests ->
            match tests with
            | [ test ] ->
                if not (test.Source.StartsWith("let x = 4L")) then
                    Error $"Unexpected parsed source: {test.Source}"
                elif test.Preamble.Trim().Length <> 0 then
                    Error $"Expected empty preamble, got: {test.Preamble}"
                else
                    Ok ()
            | _ ->
                Error $"Expected exactly 1 parsed test, got {tests.Length}")

let testParsesExpectationWithKeywordInsideQuotedMessage () : TestResult =
    let testSource =
        "module Errors =\n"
        + "  (match \"nothing matches\" with\n"
        + "   | \"not this\" -> \"fail\") = Builtin.testDerrorMessage \"No matching case found for value \\\"nothing matches\\\" in match expression\"\n"

    withTempFile testSource (fun path ->
        match parseE2ETestFile path with
        | Error msg ->
            Error $"Expected quoted-message expectation to parse, but got error: {msg}"
        | Ok tests ->
            match tests with
            | [ test ] ->
                if test.ExpectedStdout <> Some "Builtin.testDerrorMessage \"No matching case found for value \\\"nothing matches\\\" in match expression\"\n" then
                    Error $"Unexpected expectation parse: {test.ExpectedStdout}"
                else
                    Ok ()
            | _ ->
                Error $"Expected exactly 1 parsed test, got {tests.Length}")

let testParsesMultilineExpectationWithFunctionHeadAndNextLineArg () : TestResult =
    let testSource =
        "(match 1L with\n"
        + " | 1L -> \"wrong number\") = Builtin.testDerrorMessage\n"
        + "  \"No matching case found\"\n"

    withTempFile testSource (fun path ->
        match parseE2ETestFile path with
        | Error msg ->
            Error $"Expected multiline function-head expectation to parse, but got error: {msg}"
        | Ok tests ->
            match tests with
            | [ test ] ->
                let expectationLooksRight =
                    match test.ExpectedStdout with
                    | Some stdout ->
                        stdout.StartsWith("Builtin.testDerrorMessage\n")
                        && stdout.Contains("No matching case found")
                    | None -> false
                if not expectationLooksRight then
                    Error $"Unexpected expectation parse: {test.ExpectedStdout}"
                elif test.Preamble.Trim().Length <> 0 then
                    Error $"Expected empty preamble, got: {test.Preamble}"
                else
                    Ok ()
            | _ ->
                Error $"Expected exactly 1 parsed test, got {tests.Length}")

let tests = [
    ("parses multiline expectation on next line", testParsesMultilineExpectationOnNextLine)
    ("parses skip attribute", testParsesSkipAttribute)
    ("parses indented multiline .dark tests inside module", testParsesIndentedMultilineDarkTestsInsideModule)
    ("parses multiline with inner double equals", testParsesMultilineWithInnerDoubleEquals)
    ("parses multiline with inner let binding", testParsesMultilineWithInnerLetBinding)
    ("parses expectation with keyword inside quoted message", testParsesExpectationWithKeywordInsideQuotedMessage)
    ("parses multiline function-head expectation with next-line arg", testParsesMultilineExpectationWithFunctionHeadAndNextLineArg)
]
