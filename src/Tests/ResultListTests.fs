// ResultListTests.fs - Unit tests for sequential list/result helpers
//
// Verifies list/result mapping helpers preserve order and error semantics.

module ResultListTests

open ResultList

type TestResult = Result<unit, string>

let testMapResultsPreservesOrder () : TestResult =
    let input = [ 1; 2; 3; 4 ]
    match mapResults (fun x -> Ok (x * 2)) input with
    | Ok output when output = [ 2; 4; 6; 8 ] -> Ok ()
    | Ok output -> Error $"Expected [2; 4; 6; 8], got {output}"
    | Error err -> Error $"Unexpected error: {err}"

let testMapResultsReturnsFirstError () : TestResult =
    let input = [ 1; 2; 3 ]
    let result =
        mapResults
            (fun x ->
                if x = 2 then
                    Error "bad 2"
                elif x = 3 then
                    Error "bad 3"
                else
                    Ok x)
            input
    match result with
    | Error "bad 2" -> Ok ()
    | Error err -> Error $"Expected first error 'bad 2', got '{err}'"
    | Ok _ -> Error "Expected error but got Ok"

let tests = [
    ("order preserved", testMapResultsPreservesOrder)
    ("first error", testMapResultsReturnsFirstError)
]

let runAll () : TestResult =
    let rec run remaining =
        match remaining with
        | [] -> Ok ()
        | (name, test) :: rest ->
            match test () with
            | Ok () -> run rest
            | Error msg -> Error $"{name} test failed: {msg}"
    run tests
