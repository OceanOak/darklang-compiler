// FormattingRoundtripTests.fs - Focused parser/pretty roundtrip regression tests.
//
// Keeps minimal expressions that have historically failed roundtripping.

module FormattingRoundtripTests

open AST
open Parser
open ASTPrettyPrinter

type TestResult = Result<unit, string>

type private RoundtripCase = {
    Name: string
    Source: string
}

let private roundtripCompilerSyntax (source: string) : TestResult =
    match Parser.parseString false source with
    | Error err ->
        Error $"Initial parse failed.\nSource: {source}\nError: {err}"
    | Ok ast0 ->
        let printed0 = ASTPrettyPrinter.formatProgram CompilerSyntax ast0
        match Parser.parseString false printed0 with
        | Error err ->
            Error $"Re-parse failed.\nSource: {source}\nPretty: {printed0}\nError: {err}"
        | Ok ast1 ->
            let printed1 = ASTPrettyPrinter.formatProgram CompilerSyntax ast1
            if ast0 <> ast1 then
                Error (
                    "AST changed after roundtrip.\n"
                    + $"Source: {source}\n"
                    + $"Pretty (pass 1): {printed0}\n"
                    + $"Pretty (pass 2): {printed1}"
                )
            elif printed0 <> printed1 then
                Error (
                    "Pretty-printer output was not idempotent.\n"
                    + $"Source: {source}\n"
                    + $"Pretty (pass 1): {printed0}\n"
                    + $"Pretty (pass 2): {printed1}"
                )
            else
                Ok ()

let private mkRoundtripTest (caseData: RoundtripCase) : string * (unit -> TestResult) =
    (caseData.Name, fun () -> roundtripCompilerSyntax caseData.Source)

let tests : (string * (unit -> TestResult)) list =
    [
        {
            Name = "float literal zero keeps float syntax"
            Source = "0.0"
        }
        {
            Name = "nested tuple access keeps required parentheses"
            Source = "let outer = ((1, 2), (3, 4)) in (outer.0).0"
        }
    ]
    |> List.map mkRoundtripTest
