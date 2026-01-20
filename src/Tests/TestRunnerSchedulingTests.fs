// TestRunnerSchedulingTests.fs - Unit tests for test runner scheduling and CLI helpers
//
// Validates stdlib scheduling decisions and basic CLI arg parsing.

module TestRunnerSchedulingTests

open TestRunnerScheduling

type TestResult = Result<unit, string>

let testSplitUnitTestsByStdlibNeed () : TestResult =
    let suites : UnitTestSuite array = [|
        { Name = "CLI Flags Tests"; Tests = [] }
        { Name = "Stdlib Compile Tests"; Tests = [] }
        { Name = "Parallel Move Tests"; Tests = [] }
    |]
    let (noStdlib, needsStdlib) =
        splitUnitTestsByStdlibNeed [ "Stdlib Compile Tests" ] suites
    let noStdlibNames = noStdlib |> Array.map (fun suite -> suite.Name) |> Array.toList
    let needsStdlibNames = needsStdlib |> Array.map (fun suite -> suite.Name) |> Array.toList
    if noStdlibNames = [ "CLI Flags Tests"; "Parallel Move Tests" ] && needsStdlibNames = [ "Stdlib Compile Tests" ] then
        Ok ()
    else
        Error $"Unexpected unit test split: no-stdlib={noStdlibNames}, stdlib={needsStdlibNames}"

let testShouldStartStdlibCompile () : TestResult =
    let cases = [
        (false, false, false, false, false, false, false)
        (true, false, false, false, false, false, false)
        (true, false, false, true, false, false, true)
        (false, true, false, false, false, false, false)
        (false, true, false, false, true, false, true)
        (false, false, true, false, false, false, true)
        (false, false, false, false, false, true, true)
    ]
    let rec checkCases remaining =
        match remaining with
        | [] -> Ok ()
        | (hasE2E, hasVerification, needsUnitStdlib, hasMatchingE2E, hasMatchingVerification, needsOptimizationStdlib, expected) :: rest ->
            let actual =
                shouldStartStdlibCompile
                    hasE2E
                    hasVerification
                    needsUnitStdlib
                    hasMatchingE2E
                    hasMatchingVerification
                    needsOptimizationStdlib
            if actual = expected then
                checkCases rest
            else
                Error
                    $"Expected shouldStartStdlibCompile({hasE2E}, {hasVerification}, {needsUnitStdlib}, {hasMatchingE2E}, {hasMatchingVerification}, {needsOptimizationStdlib}) to be {expected}, got {actual}"
    checkCases cases

let testStdlibWarmupPlan () : TestResult =
    match decideStdlibWarmupPlan true, decideStdlibWarmupPlan false with
    | CompileStdlibBeforeTests, SkipStdlibWarmup -> Ok ()
    | actual ->
        Error $"Expected CompileStdlibBeforeTests/SkipStdlibWarmup, got {actual}"

let testFormatUnitTestName () : TestResult =
    let actual = formatUnitTestName "Stdlib Compile Tests" "stdlib compile succeeds"
    let expected = "Stdlib Compile Tests: stdlib compile succeeds"
    if actual = expected then
        Ok ()
    else
        Error $"Expected formatted name '{expected}', got '{actual}'"

let testHasVerboseArg () : TestResult =
    let argsWithVerbose = [| "--verbose" |]
    let argsWithoutVerbose = [| "--filter=tuple" |]
    if TestRunnerArgs.hasVerboseArg argsWithVerbose && not (TestRunnerArgs.hasVerboseArg argsWithoutVerbose) then
        Ok ()
    else
        Error "Expected --verbose to be detected only when provided"

let tests = [
    ("Unit test split", testSplitUnitTestsByStdlibNeed)
    ("Stdlib compile decision", testShouldStartStdlibCompile)
    ("Stdlib warmup plan", testStdlibWarmupPlan)
    ("Unit test name format", testFormatUnitTestName)
    ("verbose flag parsing", testHasVerboseArg)
]

let runAll () : TestResult =
    let rec runTests remaining =
        match remaining with
        | [] -> Ok ()
        | (name, test) :: rest ->
            match test () with
            | Ok () -> runTests rest
            | Error msg -> Error $"{name} test failed: {msg}"

    runTests tests
