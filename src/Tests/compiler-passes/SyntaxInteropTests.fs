// SyntaxInteropTests.fs - Tests for compiler/interpreter syntax interop.
//
// Verifies the dedicated interpreter-syntax parser and AST pretty printers for
// both Darklang syntaxes.

module SyntaxInteropTests

open AST
open Parser
open InterpreterParser
open ASTPrettyPrinter
open TypeChecking

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

let testParseInterpreterTripleQuotedInterpolation () : TestResult =
    let source = "$\"\"\"test {\"1\"}\"\"\""
    match InterpreterParser.parseString false source with
    | Error err -> Error $"Interpreter parser failed on triple-quoted interpolation: {err}"
    | Ok (Program [Expression (InterpolatedString [StringText "test "; StringExpr (StringLiteral "1")])]) ->
        Ok ()
    | Ok (Program [Expression expr]) ->
        Error $"Unexpected AST for triple-quoted interpolation: {expr}"
    | Ok other ->
        Error $"Expected single expression program, got: {other}"

let testParseInterpreterNegativeFloatApplicationArgs () : TestResult =
    let source = "Stdlib.Float.multiply -0.0 -1.0"
    match InterpreterParser.parseString false source with
    | Error err -> Error $"Interpreter parser failed: {err}"
    | Ok (Program [Expression (Call ("Stdlib.Float.multiply", [FloatLiteral left; FloatLiteral right]))]) ->
        if left = -0.0 && right = -1.0 then
            Ok ()
        else
            Error $"Expected negative float args, got left={left}, right={right}"
    | Ok (Program [Expression expr]) ->
        Error $"Unexpected AST for negative float application args: {expr}"
    | Ok other ->
        Error $"Expected single expression program, got: {other}"

let testCompilerParserParsesApostropheTypeArgAtCallSite () : TestResult =
    let source = "Stdlib.Json.parse<'a>(\"5\")"
    match Parser.parseString false source with
    | Error err ->
        Error $"Compiler parser failed on apostrophe type argument call site: {err}"
    | Ok (Program [Expression (TypeApp ("Stdlib.Json.parse", [TVar "a"], [StringLiteral "5"]))]) ->
        Ok ()
    | Ok (Program [Expression expr]) ->
        Error $"Unexpected AST for apostrophe type argument call site: {expr}"
    | Ok other ->
        Error $"Expected single expression program, got: {other}"

let testCompilerParserParsesApostropheTypeArgSpaceCallSite () : TestResult =
    let source = "Stdlib.Json.parse<'a> \"5\""
    match Parser.parseString false source with
    | Error err ->
        Error $"Compiler parser failed on apostrophe type argument space call site: {err}"
    | Ok (Program [Expression (TypeApp ("Stdlib.Json.parse", [TVar "a"], [StringLiteral "5"]))]) ->
        Ok ()
    | Ok (Program [Expression expr]) ->
        Error $"Unexpected AST for apostrophe type argument space call site: {expr}"
    | Ok other ->
        Error $"Expected single expression program, got: {other}"

let testParseInterpreterRecordFunctionFieldType () : TestResult =
    let source =
        "type RecordWithFn = { fn: Int64 -> Int64 }\n"
        + "(let record = RecordWithFn { fn = fun x -> x + 1L } in record.fn 6L)"
    match InterpreterParser.parseString false source with
    | Error err ->
        Error $"Interpreter parser failed on record function field type: {err}"
    | Ok (Program [TypeDef (RecordDef ("RecordWithFn", [], [("fn", TFunction ([AST.TInt64], AST.TInt64))])); Expression _]) ->
        Ok ()
    | Ok other ->
        Error $"Unexpected AST for record function field type: {other}"

let testParseInterpreterNewlineDelimitedLetBody () : TestResult =
    let source =
        "let y = (fun x -> x + 1L)\n"
        + " Stdlib.List.map_v0 [ 1L; 2L; 3L; 4L ] y"
    match InterpreterParser.parseString false source with
    | Error err ->
        Error $"Interpreter parser failed on newline-delimited let body: {err}"
    | Ok (Program [Expression _]) ->
        Ok ()
    | Ok other ->
        Error $"Unexpected AST for newline-delimited let body: {other}"

let testTypeCheckInterpreterRecordFunctionFieldLambda () : TestResult =
    let source =
        "type RecordWithFn = { fn: Int64 -> Int64 }\n"
        + "(let record = RecordWithFn { fn = fun x -> x + 1L } in record.fn 6L)"
    match InterpreterParser.parseString false source with
    | Error err ->
        Error $"Interpreter parser failed before type checking record function field lambda: {err}"
    | Ok ast ->
        match TypeChecking.checkProgram ast with
        | Error err ->
            Error $"Type checking failed for record function field lambda: {typeErrorToString err}"
        | Ok (resultType, _) when resultType = AST.TInt64 ->
            Ok ()
        | Ok (resultType, _) ->
            Error $"Expected TInt64 result for record function field lambda, got {typeToString resultType}"

let testStdlibRegistryExcludesNonIntrinsicFloatMultiply () : TestResult =
    let registry = Stdlib.buildModuleRegistry ()
    match Map.tryFind "Stdlib.Float.multiply" registry with
    | Some _ ->
        Error "Expected Stdlib.Float.multiply to be omitted from module registry (non-intrinsic)"
    | None ->
        Ok ()

let testStdlibRegistryIncludesIntrinsicFloatSqrt () : TestResult =
    let registry = Stdlib.buildModuleRegistry ()
    match Map.tryFind "Stdlib.Float.sqrt" registry with
    | Some _ -> Ok ()
    | None -> Error "Expected Stdlib.Float.sqrt to remain in module registry (intrinsic)"

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

let testPrettyPrintInterpreterSyntaxParenthesizesNestedCallArg () : TestResult =
    let source = "Builtin.unwrap(Stdlib.List.head(xs))"
    match Parser.parseString false source with
    | Error err -> Error $"Compiler parser failed: {err}"
    | Ok ast ->
        let printed = ASTPrettyPrinter.formatProgram InterpreterSyntax ast
        let expected = "Builtin.unwrap (Stdlib.List.head xs)"
        if printed = expected then
            Ok ()
        else
            Error $"Interpreter nested-call argument pretty-print mismatch. Expected '{expected}', got '{printed}'"

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

let testInterpreterParserParsesBareTupleExpression () : TestResult =
    let source = "1L, 2L, 3L"
    match InterpreterParser.parseString false source with
    | Error err -> Error $"Interpreter parser failed on bare tuple expression: {err}"
    | Ok (Program [Expression (TupleLiteral [Int64Literal 1L; Int64Literal 2L; Int64Literal 3L])]) ->
        Ok ()
    | Ok (Program [Expression expr]) ->
        Error $"Unexpected AST for bare tuple expression: {expr}"
    | Ok other ->
        Error $"Expected single expression program, got: {other}"

let tests = [
    ("compiler library interpreter parse", testCompilerLibraryParseInterpreterSyntax)
    ("parse interpreter lambda/application", testParseInterpreterLambdaApplication)
    ("parse interpreter triple-quoted interpolation", testParseInterpreterTripleQuotedInterpolation)
    ("parse interpreter negative float application args", testParseInterpreterNegativeFloatApplicationArgs)
    ("parse compiler apostrophe type argument call site", testCompilerParserParsesApostropheTypeArgAtCallSite)
    ("parse compiler apostrophe type argument space call site", testCompilerParserParsesApostropheTypeArgSpaceCallSite)
    ("parse interpreter record function field type", testParseInterpreterRecordFunctionFieldType)
    ("parse interpreter newline-delimited let body", testParseInterpreterNewlineDelimitedLetBody)
    ("typecheck interpreter record-function-field lambda", testTypeCheckInterpreterRecordFunctionFieldLambda)
    ("stdlib registry excludes non-intrinsic float multiply", testStdlibRegistryExcludesNonIntrinsicFloatMultiply)
    ("stdlib registry includes intrinsic float sqrt", testStdlibRegistryIncludesIntrinsicFloatSqrt)
    ("pretty-print interpreter syntax", testPrettyPrintInterpreterSyntax)
    ("pretty-print interpreter nested call arg", testPrettyPrintInterpreterSyntaxParenthesizesNestedCallArg)
    ("pretty-print compiler syntax", testPrettyPrintCompilerSyntax)
    ("reject bare int literal", testInterpreterParserRejectsBareIntLiteral)
    ("reject compiler lambda syntax", testInterpreterParserRejectsCompilerLambdaSyntax)
    ("reject comma-separated lists", testInterpreterParserRejectsCommaSeparatedLists)
    ("parse bare tuple expression", testInterpreterParserParsesBareTupleExpression)
]
