// RefCountInsertionTests.fs - Unit tests for RefCountInsertion type inference behavior.
//
// Verifies inferCExprType returns call result types (not function types) so
// downstream RC insertion decisions can use heap/non-heap information correctly.

module RefCountInsertionTests

open ANF
open RefCountInsertion

type TestResult = Result<unit, string>

let testInferCallReturnsFunctionReturnType () : TestResult =
    let ctx : TypeContext = {
        TypeReg = Map.empty
        VariantLookup = Map.empty
        FuncReg =
            Map.ofList [
                ("mkPair", AST.TFunction ([AST.TInt64], AST.TTuple [AST.TInt64; AST.TInt64]))
            ]
        FuncParams = Map.empty
        TempTypes = Map.empty
        ClosureFuncs = Map.empty
    }

    let cexpr = Call ("mkPair", [IntLiteral (Int64 1L)])

    match inferCExprType ctx cexpr with
    | Some (AST.TTuple [AST.TInt64; AST.TInt64]) ->
        Ok ()
    | Some actual ->
        Error $"Expected inferCExprType Call to return tuple return type, got: {actual}"
    | None ->
        Error "Expected inferCExprType Call to return a concrete type, got None"

let rec private hasDecAfterNonSelfTailCall (funcName: string) (expr: AExpr) : bool =
    match expr with
    | Return _ ->
        false
    | Let (_, TailCall (target, _), Let (_, RefCountDec _, _)) when target <> funcName ->
        true
    | Let (_, TailCall (target, _), Let (_, RefCountDecString _, _)) when target <> funcName ->
        true
    | Let (_, _, body) ->
        hasDecAfterNonSelfTailCall funcName body
    | If (_, thenBranch, elseBranch) ->
        hasDecAfterNonSelfTailCall funcName thenBranch
        || hasDecAfterNonSelfTailCall funcName elseBranch

let testNonSelfTailCallDoesNotLeaveDecAfterTailCall () : TestResult =
    let funcReg : AST_to_ANF.FunctionRegistry =
        Map.ofList [
            ("callee", AST.TFunction ([AST.TInt64], AST.TInt64))
            ("caller", AST.TFunction ([AST.TInt64], AST.TInt64))
        ]

    let ctx : TypeContext = {
        TypeReg = Map.empty
        VariantLookup = Map.empty
        FuncReg = funcReg
        FuncParams = Map.empty
        TempTypes = Map.empty
        ClosureFuncs = Map.empty
    }

    let p0 = TempId 0
    let tupleTmp = TempId 1
    let callTmp = TempId 2
    let func : Function = {
        Name = "caller"
        TypedParams = [{ Id = p0; Type = AST.TInt64 }]
        ReturnType = AST.TInt64
        Body =
            Let (
                tupleTmp,
                TupleAlloc [Var p0; IntLiteral (Int64 1L)],
                Let (
                    callTmp,
                    TailCall ("callee", [Var p0]),
                    Return (Var callTmp)
                )
            )
    }

    let (transformed, _, _) = insertRCInFunction ctx func initialVarGen

    if hasDecAfterNonSelfTailCall transformed.Name transformed.Body then
        Error "Found RefCountDec after non-self TailCall; dec should execute before tailcall"
    else
        Ok ()

let tests = [
    ("inferCExprType Call returns function return type", testInferCallReturnsFunctionReturnType)
    ("non-self tailcall does not keep dec after tailcall", testNonSelfTailCallDoesNotLeaveDecAfterTailCall)
]
