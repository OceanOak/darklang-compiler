// ASTToANFTests.fs - Unit tests for AST to ANF conversion error handling
//
// Ensures missing variant payload type info does not silently default to TInt64.

module ASTToANFTests

open AST
open AST_to_ANF
open ANF

type TestResult = Result<unit, string>

let testMissingVariantPayloadTypeErrors () : TestResult =
    let env : VarEnv =
        Map.ofList [("x", (ANF.TempId 0, AST.TSum ("MissingType", [])))]

    let pattern = AST.PConstructor ("MissingCtor", Some (AST.PVar "payload"))

    match AST.NonEmptyList.tryFromList [pattern] with
    | None -> Error "NonEmptyList.tryFromList returned None for a non-empty list"
    | Some patterns ->
        let matchCase : AST.MatchCase = { Patterns = patterns; Guard = None; Body = AST.Var "payload" }
        let expr = AST.Match (AST.Var "x", [matchCase])
        let typeReg : TypeRegistry = Map.empty
        let variantLookup : VariantLookup = Map.empty
        let funcReg : FunctionRegistry = Map.empty
        let moduleRegistry : AST.ModuleRegistry = Map.empty

        match toANF expr ANF.initialVarGen env typeReg variantLookup funcReg moduleRegistry with
        | Ok _ -> Error "Expected error when constructor payload type is missing from variant lookup"
        | Error msg ->
            if msg.Contains "MissingCtor" then Ok ()
            else Error $"Unexpected error message: {msg}"

let tests = [
    ("Missing constructor payload type errors", testMissingVariantPayloadTypeErrors)
]
