// TestRunnerScheduling.fs - Helpers for test runner scheduling decisions
//
// Provides stdlib-related scheduling helpers and formatting.

module TestRunnerScheduling

type UnitTestCase = string * (unit -> Result<unit, string>)

type UnitTestSuite = {
    Name: string
    Tests: UnitTestCase list
}

type StdlibWarmupPlan =
    | CompileStdlibBeforeTests
    | SkipStdlibWarmup

let formatUnitTestName (suiteName: string) (testName: string) : string =
    $"{suiteName}: {testName}"

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
