// E2EFormatTests.fs - Unit tests for E2E DSL parsing
//
// Verifies parser behavior for multi-line E2E test forms used by upstream .dark files.

module E2EFormatTests

open System
open System.IO
open TestDSL.E2EFormat

type TestResult = Result<unit, string>

let private withTempFileNamed (fileName: string) (contents: string) (f: string -> TestResult) : TestResult =
    let tempDir = Path.Combine(Path.GetTempPath(), $"dark-e2eformat-{Guid.NewGuid():N}")
    let tempPath = Path.Combine(tempDir, fileName)

    try
        Directory.CreateDirectory(tempDir) |> ignore
        File.WriteAllText(tempPath, contents)
        f tempPath
    finally
        if Directory.Exists(tempDir) then
            Directory.Delete(tempDir, true)

let private withTempFile (contents: string) (f: string -> TestResult) : TestResult =
    withTempFileNamed "test.dark" contents f

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
                elif test.ExpectedValueExpr <> None then
                    Error "Expected Builtin.testDerrorMessage expectation to be parsed as error expectation"
                elif test.ExpectedExitCode <> 1 then
                    Error $"Expected exit code 1, got {test.ExpectedExitCode}"
                elif test.ExpectedStderr <> Some "Uncaught exception: a" then
                    Error $"Expected stderr message parse, got: {test.ExpectedStderr}"
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
                elif test.ExpectedValueExpr <> Some "\"pass\"" then
                    Error $"Expected value-expression RHS '\"pass\"', got: {test.ExpectedValueExpr}"
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
            elif tests.[0].ExpectedValueExpr <> Some "true" then
                Error $"Expected value-expression RHS 'true', got: {tests.[0].ExpectedValueExpr}"
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
                elif test.ExpectedValueExpr <> Some "\"pass\"" then
                    Error $"Expected value-expression RHS '\"pass\"', got: {test.ExpectedValueExpr}"
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
                if test.ExpectedExitCode <> 1 then
                    Error $"Expected exit code 1, got {test.ExpectedExitCode}"
                elif test.ExpectedStderr <> Some "No matching case found for value \"nothing matches\" in match expression" then
                    Error $"Unexpected stderr parse: {test.ExpectedStderr}"
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
                if test.ExpectedExitCode <> 1 then
                    Error $"Expected exit code 1, got {test.ExpectedExitCode}"
                elif test.ExpectedStderr <> Some "No matching case found" then
                    Error $"Unexpected stderr parse: {test.ExpectedStderr}"
                elif test.Preamble.Trim().Length <> 0 then
                    Error $"Expected empty preamble, got: {test.Preamble}"
                else
                    Ok ()
            | _ ->
                Error $"Expected exactly 1 parsed test, got {tests.Length}")

let testParsesBareE2ERightHandSideAsValueExpression () : TestResult =
    let testSource = "2 + 3 = 5\n"

    withTempFileNamed "test.e2e" testSource (fun path ->
        match parseE2ETestFile path with
        | Error msg ->
            Error $"Expected bare .e2e RHS to parse as value expression, but got error: {msg}"
        | Ok tests ->
            match tests with
            | [ test ] ->
                if test.ExpectedValueExpr <> Some "5" then
                    Error $"Expected value-expression RHS '5', got: {test.ExpectedValueExpr}"
                elif test.ExpectedStdout <> None then
                    Error $"Expected no stdout expectation for bare RHS, got: {test.ExpectedStdout}"
                else
                    Ok ()
            | _ ->
                Error $"Expected exactly 1 parsed test, got {tests.Length}")

let testParsesIndentedRhsContinuationWithoutLeakingIntoPreamble () : TestResult =
    let testSource =
        "(Stdlib.Option.Option.None |> Stdlib.Option.Option.Some) = Stdlib.Option.Option.Some\n"
        + "  Stdlib.Option.Option.None\n"
        + "(1L) = 1L\n"

    withTempFile testSource (fun path ->
        match parseE2ETestFile path with
        | Error msg ->
            Error $"Expected RHS continuation to parse, but got error: {msg}"
        | Ok tests ->
            match tests with
            | [ first; second ] ->
                if first.ExpectedValueExpr <> Some "Stdlib.Option.Option.Some\n  Stdlib.Option.Option.None" then
                    Error $"Unexpected first RHS parse: {first.ExpectedValueExpr}"
                elif first.Preamble.Trim().Length <> 0 then
                    Error $"Expected empty preamble for first test, got: {first.Preamble}"
                elif second.Preamble.Trim().Length <> 0 then
                    Error $"Expected empty preamble for second test, got: {second.Preamble}"
                elif second.ExpectedValueExpr <> Some "1L" then
                    Error $"Unexpected second RHS parse: {second.ExpectedValueExpr}"
                else
                    Ok ()
            | _ ->
                Error $"Expected exactly 2 parsed tests, got {tests.Length}")

let testParsesMultilinePipeBeforeSeparator () : TestResult =
    let testSource =
        "(Stdlib.List.map_v0 [ 1L; 2L ] (fun x -> x + 1L))\n"
        + "|> Stdlib.List.length_v0 = 2L\n"

    withTempFile testSource (fun path ->
        match parseE2ETestFile path with
        | Error msg ->
            Error $"Expected multiline pipe test to parse, but got error: {msg}"
        | Ok tests ->
            match tests with
            | [ test ] ->
                if not (test.Source.Contains("|> Stdlib.List.length_v0")) then
                    Error $"Expected source to preserve pipe continuation, got: {test.Source}"
                elif test.ExpectedValueExpr <> Some "2L" then
                    Error $"Unexpected RHS parse: {test.ExpectedValueExpr}"
                elif test.Preamble.Trim().Length <> 0 then
                    Error $"Expected empty preamble, got: {test.Preamble}"
                else
                    Ok ()
            | _ ->
                Error $"Expected exactly 1 parsed test, got {tests.Length}")

let testParsesMultilineWithoutLeadingParen () : TestResult =
    let testSource =
        "Ctor\n"
        + "  { a = 1L\n"
        + "    b = 2L } = Ctor\n"

    withTempFile testSource (fun path ->
        match parseE2ETestFile path with
        | Error msg ->
            Error $"Expected multiline non-paren test to parse, but got error: {msg}"
        | Ok tests ->
            match tests with
            | [ test ] ->
                if not (test.Source.StartsWith("Ctor\n")) then
                    Error $"Unexpected source parse: {test.Source}"
                elif not (test.Source.Contains("b = 2L }")) then
                    Error $"Expected multiline record body in source, got: {test.Source}"
                elif test.ExpectedValueExpr <> Some "Ctor" then
                    Error $"Unexpected RHS parse: {test.ExpectedValueExpr}"
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
    ("parses bare .e2e rhs as value expression", testParsesBareE2ERightHandSideAsValueExpression)
    ("parses indented rhs continuation without preamble leakage", testParsesIndentedRhsContinuationWithoutLeakingIntoPreamble)
    ("parses multiline pipe before separator", testParsesMultilinePipeBeforeSeparator)
    ("parses multiline without leading paren", testParsesMultilineWithoutLeadingParen)
]
