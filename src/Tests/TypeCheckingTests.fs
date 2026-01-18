// TypeCheckingTests.fs - Unit tests for type checking pass
//
// Tests the type checker for Phase 0 (integers only)
// Will be extended in future phases for booleans, variables, functions, etc.
//
// NOTE: All tests now return Result<> instead of using failwith

module TypeCheckingTests

open AST
open TypeChecking

/// Test result type
type TestResult = Result<unit, string>

/// Helper to check that type checking succeeds with expected type
let expectType (expr: Expr) (expectedType: Type) : TestResult =
    let program = Program [Expression expr]
    match checkProgram program with
    | Ok (actualType, _) ->
        if actualType = expectedType then
            Ok ()
        else
            Error $"Expected {typeToString expectedType}, got {typeToString actualType}"
    | Error err ->
        Error $"Type checking failed: {typeErrorToString err}"

/// Test that integer literals have type TInt64
let testInt64Literal () : TestResult =
    expectType (Int64Literal 42L) TInt64

/// Test that addition of integers has type TInt64
let testAddition () : TestResult =
    expectType (BinOp (Add, Int64Literal 2L, Int64Literal 3L)) TInt64

/// Test that subtraction of integers has type TInt64
let testSubtraction () : TestResult =
    expectType (BinOp (Sub, Int64Literal 10L, Int64Literal 5L)) TInt64

/// Test that multiplication of integers has type TInt64
let testMultiplication () : TestResult =
    expectType (BinOp (Mul, Int64Literal 7L, Int64Literal 6L)) TInt64

/// Test that division of integers has type TInt64
let testDivision () : TestResult =
    expectType (BinOp (Div, Int64Literal 20L, Int64Literal 4L)) TInt64

/// Test that negation of integers has type TInt64
let testNegation () : TestResult =
    expectType (UnaryOp (Neg, Int64Literal 42L)) TInt64

/// Test nested operations
let testNestedOperations () : TestResult =
    // 2 + 3 * 4
    let expr = BinOp (Add, Int64Literal 2L, BinOp (Mul, Int64Literal 3L, Int64Literal 4L))
    expectType expr TInt64

/// Test complex nested expression
let testComplexExpression () : TestResult =
    // (10 + 20) * (30 - 15) / 2
    let expr =
        BinOp (Div,
            BinOp (Mul,
                BinOp (Add, Int64Literal 10L, Int64Literal 20L),
                BinOp (Sub, Int64Literal 30L, Int64Literal 15L)),
            Int64Literal 2L)
    expectType expr TInt64

let tests = [
    ("Integer literal", testInt64Literal)
    ("Addition", testAddition)
    ("Subtraction", testSubtraction)
    ("Multiplication", testMultiplication)
    ("Division", testDivision)
    ("Negation", testNegation)
    ("Nested operations", testNestedOperations)
    ("Complex expression", testComplexExpression)
]

/// Run all type checking unit tests
/// Returns Ok () if all pass, Error with first failure message if any fail
let runAll () : TestResult =
    let rec runTests = function
        | [] -> Ok ()
        | (name, test) :: rest ->
            match test () with
            | Ok () -> runTests rest
            | Error msg -> Error $"{name} test failed: {msg}"

    runTests tests
