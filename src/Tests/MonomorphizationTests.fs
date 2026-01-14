// MonomorphizationTests.fs - Unit tests for AST monomorphization
//
// Ensures monomorphization preserves unresolved type variables in specializations.

module MonomorphizationTests

open AST
open AST_to_ANF

type TestResult = Result<unit, string>

let testPreservesTypeVarsInSpecialization () : TestResult =
    let funcDef : FunctionDef =
        { Name = "id"
          TypeParams = ["t"]
          Params = [("x", TVar "t")]
          ReturnType = TVar "t"
          Body = Var "x" }

    let program =
        Program [
            FunctionDef funcDef
            Expression (TypeApp ("id", [TVar "t"], [IntLiteral 1L]))
        ]

    let (Program topLevels) = monomorphize program
    let funcNames =
        topLevels
        |> List.choose (function
            | FunctionDef f -> Some f.Name
            | _ -> None)

    if List.contains "id_t" funcNames && not (List.contains "id_i64" funcNames) then
        Ok ()
    else
        Error "Expected monomorphized function id_t without defaulting to id_i64"

let tests = [
    ("Preserve TVar in monomorphization", testPreservesTypeVarsInSpecialization)
]
