// TestFrameworkTests.fs - Unit tests for suite timing aggregation and display.
//
// Verifies that known suite-execution timing buckets are shown explicitly.

module TestFrameworkTests

open System
open TestFramework

type TestResult = Result<unit, string>

let private overheadEntries (columns: PassTimingColumns) : PassTimingEntry list =
    columns.Ordered
    |> List.tryFind (fun section -> section.Title = "Overhead")
    |> Option.map (fun section -> section.Entries)
    |> Option.defaultValue []

let private hasEntry (name: string) (entries: PassTimingEntry list) : bool =
    entries |> List.exists (fun entry -> entry.Name = name)

let testSuiteExecutionBucketsAreShownExplicitly () : TestResult =
    let passTimings =
        Map.ofList [
            ("Pass Test Suite Execution", TimeSpan.FromSeconds(1.2))
            ("Unit Test Suite Execution", TimeSpan.FromSeconds(3.4))
        ]
    let columns = buildPassTimingColumns passTimings [] TimeSpan.Zero
    let entries = overheadEntries columns
    if not (hasEntry "Pass Test Suite Execution" entries) then
        Error "Expected Pass Test Suite Execution to appear in the Overhead section"
    elif not (hasEntry "Unit Test Suite Execution" entries) then
        Error "Expected Unit Test Suite Execution to appear in the Overhead section"
    elif hasEntry "Other (known)" entries then
        Error "Expected suite execution timings to avoid Other (known) when explicit buckets exist"
    else
        Ok ()

let tests = [
    ("suite execution buckets shown explicitly", testSuiteExecutionBucketsAreShownExplicitly)
]
