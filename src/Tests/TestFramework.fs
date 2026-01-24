// TestFramework.fs - Generic test runner helpers.
//
// Provides shared types and utilities that are independent of any specific test suite.

module TestFramework

open System
open System.Diagnostics
open Output

type UnitTestCase = string * (unit -> Result<unit, string>)

type UnitTestSuite = {
    Name: string
    Tests: UnitTestCase list
}

// Failed test info for summary at end
type FailedTestInfo = {
    File: string
    Name: string
    Message: string
    Details: string list  // Additional details like expected/actual
}

// Test timing info for slowest tests report
type TestTiming = {
    Name: string
    TotalTime: TimeSpan
    CompileTime: TimeSpan option
    RuntimeTime: TimeSpan option
}

// Summary of per-file test suite results
type FileSuiteSummary = {
    Passed: int
    Failed: int
    FailedTests: FailedTestInfo list
}

type TestRunState = {
    mutable Passed: int
    mutable Failed: int
    FailedTests: ResizeArray<FailedTestInfo>
    Timings: ResizeArray<TestTiming>
    mutable PassTimings: Map<string, TimeSpan>
}

type OutputSymbols = {
    Pass: string
    Fail: string
    SectionPrefix: string
}

let createState () : TestRunState =
    { Passed = 0
      Failed = 0
      FailedTests = ResizeArray()
      Timings = ResizeArray()
      PassTimings = Map.empty }

let recordTiming (state: TestRunState) (timing: TestTiming) : unit =
    state.Timings.Add timing

let recordPassTiming (state: TestRunState) (timing: CompilerLibrary.PassTiming) : unit =
    let existing =
        Map.tryFind timing.Pass state.PassTimings
        |> Option.defaultValue TimeSpan.Zero
    let updated = existing + timing.Elapsed
    state.PassTimings <- Map.add timing.Pass updated state.PassTimings

let recordResults
    (state: TestRunState)
    (passedDelta: int)
    (failedDelta: int)
    (failedTestsDelta: FailedTestInfo list)
    : unit =
    state.Passed <- state.Passed + passedDelta
    state.Failed <- state.Failed + failedDelta
    for test in failedTestsDelta do
        state.FailedTests.Add test

// Format elapsed time
let formatTime (elapsed: TimeSpan) =
    if elapsed.TotalMilliseconds < 1000.0 then
        let ms = elapsed.TotalMilliseconds.ToString("0")
        $"{ms}ms"
    else
        let seconds = elapsed.TotalSeconds.ToString("0.00")
        $"{seconds}s"

let addExpectedActualDetails (expected: string option) (actual: string option) : string list =
    let details = ResizeArray<string>()
    match expected, actual with
    | Some exp, Some act ->
        println "    Expected:"
        for line in exp.Split('\n') do
            println $"      {line}"
            details.Add($"Expected: {line}")
        println "    Actual:"
        for line in act.Split('\n') do
            println $"      {line}"
            details.Add($"Actual: {line}")
    | _ -> ()
    details |> Seq.toList

let runFileSuite
    (state: TestRunState)
    (symbols: OutputSymbols)
    (suiteTitle: string)
    (progressLabel: string)
    (testFiles: string array)
    (getTestName: string -> string)
    (formatTimingName: string -> string)
    (runFile: string -> Result<'result, string>)
    (handleSuccess: ProgressBar.State -> string -> string -> TimeSpan -> 'result -> FileSuiteSummary)
    (handleError: ProgressBar.State -> string -> string -> TimeSpan -> string -> FileSuiteSummary)
    : unit =
    if testFiles.Length > 0 then
        let sectionTimer = Stopwatch.StartNew()
        println $"{Colors.cyan}{suiteTitle}{Colors.reset}"

        let mutable sectionPassed = 0
        let mutable sectionFailed = 0
        let progress = ProgressBar.create progressLabel testFiles.Length
        ProgressBar.update progress

        for testPath in testFiles do
            let testName = getTestName testPath
            let testTimer = Stopwatch.StartNew()
            let summary =
                match runFile testPath with
                | Ok result ->
                    testTimer.Stop()
                    handleSuccess progress testPath testName testTimer.Elapsed result
                | Error msg ->
                    testTimer.Stop()
                    handleError progress testPath testName testTimer.Elapsed msg
            recordTiming state { Name = formatTimingName testName; TotalTime = testTimer.Elapsed; CompileTime = None; RuntimeTime = None }
            sectionPassed <- sectionPassed + summary.Passed
            sectionFailed <- sectionFailed + summary.Failed
            recordResults state summary.Passed summary.Failed summary.FailedTests

        ProgressBar.finish progress
        sectionTimer.Stop()
        if sectionFailed = 0 then
            println $"  {Colors.green}{symbols.Pass} {sectionPassed} passed{Colors.reset}"
        else
            println $"  {Colors.green}{symbols.Pass} {sectionPassed} passed{Colors.reset}, {Colors.red}{symbols.Fail} {sectionFailed} failed{Colors.reset}"
        println $"  {Colors.gray}{symbols.SectionPrefix} Completed in {formatTime sectionTimer.Elapsed}{Colors.reset}"
        println ""

let runUnitTestSuites
    (state: TestRunState)
    (symbols: OutputSymbols)
    (suiteTitle: string)
    (progressLabel: string)
    (suites: UnitTestSuite array)
    : unit =
    let unitSectionTimer = Stopwatch.StartNew()
    println $"{Colors.cyan}{suiteTitle}{Colors.reset}"

    let mutable unitSectionPassed = 0
    let mutable unitSectionFailed = 0
    let unitFailedTests = ResizeArray<FailedTestInfo>()

    let totalUnitTests =
        suites
        |> Array.sumBy (fun suite -> suite.Tests.Length)
    let unitProgress = ProgressBar.create progressLabel totalUnitTests
    ProgressBar.update unitProgress

    for suite in suites do
        for (testName, runTest) in suite.Tests do
            let timer = Stopwatch.StartNew()
            let displayName = $"{suite.Name}: {testName}: "
            match runTest() with
            | Ok () ->
                timer.Stop()
                recordTiming state { Name = $"Unit: {displayName}"; TotalTime = timer.Elapsed; CompileTime = None; RuntimeTime = None }
                unitSectionPassed <- unitSectionPassed + 1
                ProgressBar.increment unitProgress true
            | Error msg ->
                timer.Stop()
                recordTiming state { Name = $"Unit: {suite.Name}: {testName}"; TotalTime = timer.Elapsed; CompileTime = None; RuntimeTime = None }
                recordTiming state { Name = $"Unit: {displayName}"; TotalTime = timer.Elapsed; CompileTime = None; RuntimeTime = None }
                ProgressBar.increment unitProgress false
                ProgressBar.finish unitProgress
                println $"  {displayName}... {Colors.red}{symbols.Fail} FAIL{Colors.reset} {Colors.gray}({formatTime timer.Elapsed}){Colors.reset}"
                println $"    {msg}"
                unitFailedTests.Add({ File = ""; Name = $"Unit: {displayName}"; Message = msg; Details = [] })
                unitSectionFailed <- unitSectionFailed + 1
                ProgressBar.update unitProgress

    ProgressBar.finish unitProgress
    unitSectionTimer.Stop()
    if unitSectionFailed = 0 then
        println $"  {Colors.green}{symbols.Pass} {unitSectionPassed} passed{Colors.reset}"
    else
        println $"  {Colors.green}{symbols.Pass} {unitSectionPassed} passed{Colors.reset}, {Colors.red}{symbols.Fail} {unitSectionFailed} failed{Colors.reset}"
    println $"  {Colors.gray}{symbols.SectionPrefix} Completed in {formatTime unitSectionTimer.Elapsed}{Colors.reset}"
    println ""
    recordResults state unitSectionPassed unitSectionFailed (unitFailedTests |> Seq.toList)
