// TestRunnerScheduling.fs - Helpers for test runner scheduling decisions
//
// Provides deterministic ordering and stdlib-related scheduling helpers.

module TestRunnerScheduling

open TestDSL.E2EFormat

type UnitTestCase = string * (unit -> Result<unit, string>)

type UnitTestSuite = {
    Name: string
    Tests: UnitTestCase list
}

type StdlibWarmupPlan =
    | CompileStdlibBeforeTests
    | SkipStdlibWarmup

let estimateE2ETestCost (test: E2ETest) : int =
    test.Source.Length + test.Preamble.Length

let formatUnitTestName (suiteName: string) (testName: string) : string =
    $"{suiteName}: {testName}"

let orderE2ETestsByEstimatedCost (tests: E2ETest array) : E2ETest array =
    tests
    |> Array.map (fun test -> test, estimateE2ETestCost test)
    |> Array.sortBy (fun (test, cost) -> (-cost, test.SourceFile, test.Name))
    |> Array.map fst

let splitUnitTestsByStdlibNeed
    (needsStdlib: string list)
    (suites: UnitTestSuite array)
    : UnitTestSuite array * UnitTestSuite array =
    let needsStdlibSet = Set.ofList needsStdlib
    let needsStdlibSuites, noStdlibSuites =
        suites
        |> Array.partition (fun suite -> Set.contains suite.Name needsStdlibSet)
    (noStdlibSuites, needsStdlibSuites)

let shouldStartStdlibCompile
    (_hasE2E: bool)
    (_hasVerification: bool)
    (needsUnitStdlib: bool)
    (hasMatchingE2E: bool)
    (hasMatchingVerification: bool)
    (needsOptimizationStdlib: bool)
    : bool =
    needsUnitStdlib || hasMatchingE2E || hasMatchingVerification || needsOptimizationStdlib

let decideStdlibWarmupPlan (shouldCompileStdlib: bool) : StdlibWarmupPlan =
    if shouldCompileStdlib then
        CompileStdlibBeforeTests
    else
        SkipStdlibWarmup
