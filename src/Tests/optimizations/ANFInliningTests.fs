// ANFInliningTests.fs - Unit tests for ANF inlining behavior
//
// Verifies that the inliner handles literal arguments by inlining the call
// and binding literals to fresh TempIds in the inlined body.

module ANFInliningTests

open ANF

type TestResult = Result<unit, string>

let private intAtom (value: int64) : Atom =
    IntLiteral (Int64 value)

let rec private containsCall (target: string) (expr: AExpr) : bool =
    match expr with
    | Return _ -> false
    | Let (_, Call (name, _), body) ->
        name = target || containsCall target body
    | Let (_, _, body) ->
        containsCall target body
    | If (_, thenBranch, elseBranch) ->
        containsCall target thenBranch || containsCall target elseBranch

let rec private hasLiteralLet (expr: AExpr) : bool =
    match expr with
    | Return _ -> false
    | Let (_, Atom (IntLiteral _), _) -> true
    | Let (_, _, body) -> hasLiteralLet body
    | If (_, thenBranch, elseBranch) ->
        hasLiteralLet thenBranch || hasLiteralLet elseBranch

let testInliningWithLiteralArgumentsRemovesCall () : TestResult =
    let param = { Id = TempId 0; Type = AST.TInt64 }
    let addBody =
        Let (
            TempId 1,
            Prim (Add, Var param.Id, intAtom 1L),
            Return (Var (TempId 1))
        )
    let addOne =
        { Name = "addOne"
          TypedParams = [param]
          ReturnType = AST.TInt64
          ReturnOwnership = OwnedReturn
          Body = addBody }
    let main =
        Let (
            TempId 2,
            Call ("addOne", [intAtom 41L]),
            Return (Var (TempId 2))
        )
    let (Program (_, inlinedMain)) =
        ANF_Inlining.inlineProgramDefault (Program ([addOne], main))
    if containsCall "addOne" inlinedMain then
        Error "Expected literal-argument call to be inlined, but Call remained in main expression"
    else
        Ok ()

let testInliningWithLiteralArgumentsBindsTemp () : TestResult =
    let param = { Id = TempId 0; Type = AST.TInt64 }
    let identity =
        { Name = "id"
          TypedParams = [param]
          ReturnType = AST.TInt64
          ReturnOwnership = OwnedReturn
          Body = Return (Var param.Id) }
    let main =
        Let (
            TempId 1,
            Call ("id", [intAtom 7L]),
            Return (Var (TempId 1))
        )
    let (Program (_, inlinedMain)) =
        ANF_Inlining.inlineProgramDefault (Program ([identity], main))
    if hasLiteralLet inlinedMain then
        Ok ()
    else
        Error "Expected inlined literal argument to be bound to a fresh TempId"

let testInliningUnderscoreFunctionName () : TestResult =
    let param = { Id = TempId 0; Type = AST.TInt64 }
    let addBody =
        Let (
            TempId 1,
            Prim (Add, Var param.Id, intAtom 1L),
            Return (Var (TempId 1))
        )
    let addOne =
        { Name = "_addOne"
          TypedParams = [param]
          ReturnType = AST.TInt64
          ReturnOwnership = OwnedReturn
          Body = addBody }
    let main =
        Let (
            TempId 2,
            Atom (intAtom 41L),
            Let (
                TempId 3,
                Call ("_addOne", [Var (TempId 2)]),
                Return (Var (TempId 3))
            )
        )
    let (Program (_, inlinedMain)) =
        ANF_Inlining.inlineProgramDefault (Program ([addOne], main))
    if containsCall "_addOne" inlinedMain then
        Error "Expected underscore-named function to be inlined, but Call remained in main expression"
    else
        Ok ()

let tests = [
    ("Inlining literal args removes call", testInliningWithLiteralArgumentsRemovesCall)
    ("Inlining literal args binds literal TempId", testInliningWithLiteralArgumentsBindsTemp)
    ("Inlining underscore-named functions", testInliningUnderscoreFunctionName)
]
