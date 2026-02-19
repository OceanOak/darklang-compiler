// ASTToANFTests.fs - Unit tests for AST to ANF conversion error handling
//
// Ensures missing variant payload type info is surfaced as an error.

module ASTToANFTests

open AST_to_ANF

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

let testNeedsLambdaLoweringIgnoresShadowedFunc () : TestResult =
    let knownFuncs = Set.ofList ["f"]
    let expr = AST.Let ("f", AST.Int64Literal 1L, AST.Var "f")
    let program = AST.Program [AST.Expression expr]
    if programNeedsLambdaLowering knownFuncs program then
        Error "Expected shadowed function name to not trigger lambda lowering"
    else
        Ok ()

let testNeedsLambdaLoweringDetectsFuncValue () : TestResult =
    let knownFuncs = Set.ofList ["f"]
    let program = AST.Program [AST.Expression (AST.Var "f")]
    if programNeedsLambdaLowering knownFuncs program then Ok ()
    else Error "Expected function value usage to trigger lambda lowering"

let testNeedsLambdaLoweringDetectsLambda () : TestResult =
    let knownFuncs = Set.empty
    let expr = AST.Lambda ([("x", AST.TInt64)], AST.Var "x")
    let program = AST.Program [AST.Expression expr]
    if programNeedsLambdaLowering knownFuncs program then Ok ()
    else Error "Expected lambda to trigger lambda lowering"

let tests = [
    ("Missing constructor payload type errors", testMissingVariantPayloadTypeErrors)
    ("Lambda lowering ignores shadowed functions", testNeedsLambdaLoweringIgnoresShadowedFunc)
    ("Lambda lowering detects function value", testNeedsLambdaLoweringDetectsFuncValue)
    ("Lambda lowering detects lambda", testNeedsLambdaLoweringDetectsLambda)
]
