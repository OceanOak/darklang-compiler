// LambdaLiftingTests.fs - Unit tests for lambda lifting in AST_to_ANF
//
// Ensures lambda return types are preserved when lifting closures with let-bound bodies.

module LambdaLiftingTests

open AST
open ANF
open AST_to_ANF
open Parser
open TypeChecking

type TestResult = Result<unit, string>

let testLetBoundTupleReturnType () : TestResult =
    let source =
        "def apply(f: (Int64) -> (Int64, Int64), x: Int64) : (Int64, Int64) = f(x)\n" +
        "apply((x: Int64) => let t = (x, x + 1) in t, 1)"
    match parseString source with
    | Error err -> Error $"Parse error: {err}"
    | Ok ast ->
        match checkProgram ast with
        | Error err -> Error $"Type error: {typeErrorToString err}"
        | Ok (_, typedAst) ->
            match convertProgramWithTypes typedAst with
            | Error err -> Error $"ANF conversion error: {err}"
            | Ok convResult ->
                let (Program (functions, _)) = convResult.Program
                let liftedFunc = functions |> List.tryFind (fun func -> func.Name.StartsWith("__closure_"))
                match liftedFunc with
                | None -> Error "Expected a lifted lambda function named __closure_*"
                | Some func ->
                    let expected = AST.TTuple [AST.TInt64; AST.TInt64]
                    if func.ReturnType = expected then
                        Ok ()
                    else
                        Error $"Expected lifted lambda return type {typeToString expected}, got {typeToString func.ReturnType}"

let tests = [
    ("Let-bound tuple return type", testLetBoundTupleReturnType)
]
