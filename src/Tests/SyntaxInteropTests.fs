// SyntaxInteropTests.fs - Tests for compiler/interpreter syntax interop.
//
// Verifies the dedicated interpreter-syntax parser and AST pretty printers for
// both Darklang syntaxes.

module SyntaxInteropTests

open AST
open Parser
open InterpreterParser
open ASTPrettyPrinter

type TestResult = Result<unit, string>

let testCompilerLibraryParseInterpreterSyntax () : TestResult =
    let source = "let x = 5L in x"
    match CompilerLibrary.parseProgram CompilerLibrary.InterpreterSyntax false source with
    | Error err -> Error $"CompilerLibrary interpreter parse failed: {err}"
    | Ok (Program [Expression _]) -> Ok ()
    | Ok other -> Error $"Expected single expression program, got: {other}"

let testParseInterpreterLambdaApplication () : TestResult =
    let source = "let inc = fun x -> Stdlib.Int64.add x 1L in inc 41L"
    match InterpreterParser.parseString false source with
    | Error err -> Error $"Interpreter parser failed: {err}"
    | Ok (Program [Expression expr]) ->
        match expr with
        | Let ("inc", Lambda ([(paramName, _)], body), Call ("inc", [Int64Literal 41L])) ->
            match body with
            | Call ("Stdlib.Int64.add", [Var "x"; Int64Literal 1L]) when paramName = "x" -> Ok ()
            | _ -> Error $"Unexpected lambda body AST: {body}"
        | _ -> Error $"Unexpected AST shape: {expr}"
    | Ok other ->
        Error $"Expected single expression program, got: {other}"

let testPrettyPrintInterpreterSyntax () : TestResult =
    let source = "let x = 5 in Stdlib.Int64.add(x, 1)"
    match Parser.parseString false source with
    | Error err -> Error $"Compiler parser failed: {err}"
    | Ok ast ->
        let printed = ASTPrettyPrinter.formatProgram InterpreterSyntax ast
        let expected = "let x = 5L in Stdlib.Int64.add x 1L"
        if printed = expected then
            Ok ()
        else
            Error $"Interpreter pretty-printer mismatch. Expected '{expected}', got '{printed}'"

let testPrettyPrintCompilerSyntax () : TestResult =
    let source = "let x = 5L in Stdlib.Int64.add x 1L"
    match InterpreterParser.parseString false source with
    | Error err -> Error $"Interpreter parser failed: {err}"
    | Ok ast ->
        let printed = ASTPrettyPrinter.formatProgram CompilerSyntax ast
        let expected = "let x = 5 in Stdlib.Int64.add(x, 1)"
        if printed = expected then
            Ok ()
        else
            Error $"Compiler pretty-printer mismatch. Expected '{expected}', got '{printed}'"

let testInterpreterParserRejectsBareIntLiteral () : TestResult =
    let source = "let x = 5 in x"
    match InterpreterParser.parseString false source with
    | Ok _ -> Error "Expected interpreter parser to reject bare integer literal without L suffix"
    | Error _ -> Ok ()

let testInterpreterParserRejectsCompilerLambdaSyntax () : TestResult =
    let source = "let inc = (x: Int64) => x + 1 in inc 4L"
    match InterpreterParser.parseString false source with
    | Ok _ -> Error "Expected interpreter parser to reject compiler lambda syntax '(x: T) => ...'"
    | Error _ -> Ok ()

let testInterpreterParserRejectsCommaSeparatedLists () : TestResult =
    let source = "[1L, 2L, 3L]"
    match InterpreterParser.parseString false source with
    | Ok _ -> Error "Expected interpreter parser to reject comma-separated list literal"
    | Error _ -> Ok ()

let tests = [
    ("compiler library interpreter parse", testCompilerLibraryParseInterpreterSyntax)
    ("parse interpreter lambda/application", testParseInterpreterLambdaApplication)
    ("pretty-print interpreter syntax", testPrettyPrintInterpreterSyntax)
    ("pretty-print compiler syntax", testPrettyPrintCompilerSyntax)
    ("reject bare int literal", testInterpreterParserRejectsBareIntLiteral)
    ("reject compiler lambda syntax", testInterpreterParserRejectsCompilerLambdaSyntax)
    ("reject comma-separated lists", testInterpreterParserRejectsCommaSeparatedLists)
]
