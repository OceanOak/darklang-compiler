// SyntaxRoundtripCorpusTests.fs - Corpus roundtrip tests for parser and pretty-printer.
//
// Uses all .e2e sources as a real-world corpus and enforces:
// parse -> pretty-print -> parse -> pretty-print
// while failing fast on the first mismatch.

module SyntaxRoundtripCorpusTests

open System
open AST
open Parser
open InterpreterParser
open ASTPrettyPrinter
open TestDSL.E2EFormat

type TestResult = Result<unit, string>

type private SyntaxMode = {
    SourceSyntax: CompilerLibrary.SourceSyntax
    PrettySyntax: ASTPrettyPrinter.Syntax
}

type private Snippet = {
    Label: string
    Source: string
}

let private syntaxModeForFile (sourceFile: string) : SyntaxMode =
    let normalized = sourceFile.Replace('\\', '/')
    if normalized.Contains("/interpreter/")
       || normalized.Contains("/e2e/upstream/language/") then
        {
            SourceSyntax = CompilerLibrary.InterpreterSyntax
            PrettySyntax = ASTPrettyPrinter.InterpreterSyntax
        }
    else
        {
            SourceSyntax = CompilerLibrary.CompilerSyntax
            PrettySyntax = ASTPrettyPrinter.CompilerSyntax
        }

let private allowInternalForFile (sourceFile: string) : bool =
    let normalized = sourceFile.Replace('\\', '/')
    normalized.Contains("/stdlib-internal/")
    || normalized.EndsWith("/e2e/stdlib_internals.e2e")

let private parseBySyntax
    (sourceSyntax: CompilerLibrary.SourceSyntax)
    (allowInternal: bool)
    (source: string)
    : Result<AST.Program, string> =
    match sourceSyntax with
    | CompilerLibrary.CompilerSyntax -> Parser.parseString allowInternal source
    | CompilerLibrary.InterpreterSyntax -> InterpreterParser.parseString allowInternal source

let private syntaxName (sourceSyntax: CompilerLibrary.SourceSyntax) : string =
    match sourceSyntax with
    | CompilerLibrary.CompilerSyntax -> "CompilerSyntax"
    | CompilerLibrary.InterpreterSyntax -> "InterpreterSyntax"

let private snippetsForTest (test: E2ETest) : Snippet list =
    let preambleSnippet =
        if String.IsNullOrWhiteSpace test.Preamble then
            []
        else
            [ { Label = "preamble"; Source = test.Preamble } ]

    let sourceSnippet =
        [ { Label = "source"; Source = test.Source } ]

    let expectedSnippet =
        match test.ExpectedValueExpr with
        | Some rhs ->
            [ { Label = "expected-value"; Source = rhs } ]
        | None -> []

    preambleSnippet @ sourceSnippet @ expectedSnippet

let private failureHeader
    (kind: string)
    (sourceFile: string)
    (testName: string)
    (snippetLabel: string)
    (sourceSyntax: CompilerLibrary.SourceSyntax)
    (allowInternal: bool)
    : string =
    [
        $"Roundtrip failure kind: {kind}"
        $"File: {sourceFile}"
        $"Test: {testName}"
        $"Snippet: {snippetLabel}"
        $"Syntax: {syntaxName sourceSyntax}"
        $"Allow internal: {allowInternal}"
    ]
    |> String.concat "\n"

let private roundtripSnippet
    (sourceFile: string)
    (testName: string)
    (mode: SyntaxMode)
    (allowInternal: bool)
    (snippet: Snippet)
    : TestResult =
    match parseBySyntax mode.SourceSyntax allowInternal snippet.Source with
    | Error originalParseError ->
        let header =
            failureHeader
                "ParseOriginalFailed"
                sourceFile
                testName
                snippet.Label
                mode.SourceSyntax
                allowInternal
        Error (
            $"{header}\n"
            + $"\nOriginal parse error: {originalParseError}\n"
            + "\nOriginal source:\n"
            + snippet.Source
        )
    | Ok ast0 ->
        let printed0 = ASTPrettyPrinter.formatProgram mode.PrettySyntax ast0
        match parseBySyntax mode.SourceSyntax allowInternal printed0 with
        | Error prettyParseError ->
            let header =
                failureHeader
                    "ParsePrettyFailed"
                    sourceFile
                    testName
                    snippet.Label
                    mode.SourceSyntax
                    allowInternal
            Error (
                $"{header}\n"
                + $"\nPretty parse error: {prettyParseError}\n"
                + "\nOriginal source:\n"
                + snippet.Source
                + "\n\nPretty-printed source:\n"
                + printed0
            )
        | Ok ast1 ->
            let printed1 = ASTPrettyPrinter.formatProgram mode.PrettySyntax ast1
            if ast0 <> ast1 then
                let ast0Repr = sprintf "%A" ast0
                let ast1Repr = sprintf "%A" ast1
                let header =
                    failureHeader
                        "AstChangedAfterRoundtrip"
                        sourceFile
                        testName
                        snippet.Label
                        mode.SourceSyntax
                        allowInternal
                Error (
                    $"{header}\n"
                    + "\nOriginal source:\n"
                    + snippet.Source
                    + "\n\nPretty-printed source (first pass):\n"
                    + printed0
                    + "\n\nPretty-printed source (second pass):\n"
                    + printed1
                    + "\n\nParsed AST (first parse):\n"
                    + ast0Repr
                    + "\n\nParsed AST (second parse):\n"
                    + ast1Repr
                )
            elif printed0 <> printed1 then
                let header =
                    failureHeader
                        "PrettyNotIdempotent"
                        sourceFile
                        testName
                        snippet.Label
                        mode.SourceSyntax
                        allowInternal
                Error (
                    $"{header}\n"
                    + "\nOriginal source:\n"
                    + snippet.Source
                    + "\n\nPretty-printed source (first pass):\n"
                    + printed0
                    + "\n\nPretty-printed source (second pass):\n"
                    + printed1
                )
            else
                Ok ()

let private runRoundtripForTest
    (sourceFile: string)
    (mode: SyntaxMode)
    (allowInternal: bool)
    (test: E2ETest)
    : TestResult =
    let rec loop (remaining: Snippet list) : TestResult =
        match remaining with
        | [] -> Ok ()
        | snippet :: rest ->
            match roundtripSnippet sourceFile test.Name mode allowInternal snippet with
            | Ok () -> loop rest
            | Error _ as err -> err
    loop (snippetsForTest test)

let private runRoundtripForFile (sourceFile: string) : TestResult =
    match parseE2ETestFile sourceFile with
    | Error msg -> Error $"Failed to parse E2E file before roundtrip: {sourceFile}\n{msg}"
    | Ok tests ->
        let mode = syntaxModeForFile sourceFile
        let allowInternal = allowInternalForFile sourceFile
        let rec loop (remaining: E2ETest list) : TestResult =
            match remaining with
            | [] -> Ok ()
            | test :: rest ->
                match runRoundtripForTest sourceFile mode allowInternal test with
                | Ok () -> loop rest
                | Error _ as err -> err
        loop tests

let private runRoundtripForAllE2EFiles (testFiles: string array) : TestResult =
    let orderedFiles = testFiles |> Array.sort |> Array.toList
    let rec loop (remaining: string list) : TestResult =
        match remaining with
        | [] -> Ok ()
        | testFile :: rest ->
            match runRoundtripForFile testFile with
            | Ok () -> loop rest
            | Error _ as err -> err
    loop orderedFiles

let tests (testFiles: string array) : (string * (unit -> TestResult)) list = [
    ("parser/pretty corpus roundtrip", fun () -> runRoundtripForAllE2EFiles testFiles)
]
