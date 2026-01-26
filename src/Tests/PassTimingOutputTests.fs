// PassTimingOutputTests.fs - Unit tests for pass timing layout ordering.
//
// Ensures pass vs non-pass timing sections are ordered as expected for output.

module PassTimingOutputTests

open System

type TestResult = Result<unit, string>

let testPassTimingColumnsOrdering () : TestResult =
    let timings =
        [
            ("Parse", TimeSpan.FromMilliseconds(120.0))
            ("Type Checking", TimeSpan.FromMilliseconds(200.0))
            ("AST -> ANF", TimeSpan.FromMilliseconds(80.0))
            ("Cache Deserialize", TimeSpan.FromMilliseconds(60.0))
            ("E2E Test Parse", TimeSpan.FromMilliseconds(40.0))
            ("Verification Test Parse", TimeSpan.FromMilliseconds(100.0))
            ("Verification Suite Context Overhead", TimeSpan.FromMilliseconds(130.0))
            ("Compile Overhead", TimeSpan.FromMilliseconds(150.0))
            ("Cache Hash Serialize: typed program", TimeSpan.FromMilliseconds(90.0))
            ("Cache Write: insert", TimeSpan.FromMilliseconds(140.0))
            ("Cache Flush", TimeSpan.FromMilliseconds(160.0))
            ("Custom Timing", TimeSpan.FromMilliseconds(70.0))
            ("Start Function Compilation", TimeSpan.FromMilliseconds(110.0))
        ]
        |> Map.ofList

    let order =
        [
            "Parse"
            "Cache Hash Serialize: typed program"
            "Type Checking"
            "Start Function Compilation"
            "Cache Write: insert"
            "AST -> ANF"
            "Compile Overhead"
        ]

    let unaccounted = TimeSpan.FromMilliseconds(500.0)
    let columns = TestFramework.buildPassTimingColumns timings order unaccounted

    let names (entries: TestFramework.PassTimingEntry list) : string list =
        entries |> List.map (fun entry -> entry.Name)

    let numbers (entries: TestFramework.PassTimingEntry list) : string option list =
        entries |> List.map (fun entry -> entry.Number)

    match columns.Ordered with
    | ordered :: [] ->
        let orderedNames = names ordered.Entries
        let orderedNumbers = numbers ordered.Entries
        let expectedNames =
            [
                "Cache Deserialize"
                "Parser"
                "Type Checking"
                "AST to ANF"
                "Compile Overhead"
                "Cache Hash/Serialize total"
                "Cache Write"
                "Cache Flush"
                "Other (known)"
                "Other (unknown)"
            ]
        let expectedNumbers =
            [
                None
                Some "1"
                Some "1.5"
                Some "2"
                None
                None
                None
                None
                None
                None
            ]
        let orderedNameText = String.concat ", " orderedNames
        let orderedNumberText =
            orderedNumbers
            |> List.map (Option.defaultValue "")
            |> String.concat ", "
        let otherKnownExpected = TimeSpan.FromMilliseconds(450.0)
        let otherUnknownExpected = TimeSpan.FromMilliseconds(500.0)
        let findEntry (target: string) =
            ordered.Entries |> List.tryFind (fun entry -> entry.Name = target)
        let otherKnownEntry = findEntry "Other (known)"
        let otherUnknownEntry = findEntry "Other (unknown)"
        let parseEntry = findEntry "Parser"
        let typeCheckEntry = findEntry "Type Checking"
        let anfEntry = findEntry "AST to ANF"
        let otherKnownOk =
            match otherKnownEntry with
            | Some entry -> entry.Elapsed = otherKnownExpected
            | None -> false
        let otherUnknownOk =
            match otherUnknownEntry with
            | Some entry -> entry.Elapsed = otherUnknownExpected
            | None -> false
        if ordered.Title <> "Order" then
            Error $"Unexpected ordered section title: {ordered.Title}"
        elif orderedNames <> expectedNames then
            Error $"Unexpected ordered names: {orderedNameText}"
        elif orderedNumbers <> expectedNumbers then
            Error $"Unexpected ordered numbers: {orderedNumberText}"
        elif List.contains "E2E Test Parse" orderedNames then
            Error "Expected E2E Test Parse to be excluded below threshold"
        elif List.contains "Verification Test Parse" orderedNames then
            Error "Expected Verification Test Parse to be excluded"
        elif List.contains "Verification Suite Context Overhead" orderedNames then
            Error "Expected Verification Suite Context Overhead to be excluded"
        elif List.contains "Start Function Compilation" orderedNames then
            Error "Expected Start Function Compilation to be excluded"
        elif parseEntry |> Option.exists (fun entry -> entry.Number <> Some "1") then
            Error "Expected Parser to have number 1"
        elif typeCheckEntry |> Option.exists (fun entry -> entry.Number <> Some "1.5") then
            Error "Expected Type Checking to have number 1.5"
        elif anfEntry |> Option.exists (fun entry -> entry.Number <> Some "2") then
            Error "Expected AST to ANF to have number 2"
        elif not otherKnownOk then
            Error "Unexpected Other (known) elapsed"
        elif not otherUnknownOk then
            Error "Unexpected Other (unknown) elapsed"
        else
            match columns.ByTime with
            | byTime :: [] ->
                let byTimeNames = names byTime.Entries
                let byTimeNameText = String.concat ", " byTimeNames
                let times = byTime.Entries |> List.map (fun entry -> entry.Elapsed)
                let rec isDescending (values: TimeSpan list) : bool =
                    match values with
                    | [] -> true
                    | _ :: [] -> true
                    | first :: second :: rest ->
                        first >= second && isDescending (second :: rest)
                if byTime.Title <> "By Time" then
                    Error $"Unexpected by-time section title: {byTime.Title}"
                elif Set.ofList byTimeNames <> Set.ofList orderedNames then
                    Error $"Unexpected by-time names: {byTimeNameText}"
                elif not (isDescending times) then
                    Error "Expected by-time entries to be sorted descending"
                else
                    Ok ()
            | _ ->
                Error "Expected one by-time section"
    | _ ->
        Error "Expected one ordered section"

let testUnaccountedTimeBreakdown () : TestResult =
    let makeTiming (name: string) (totalTime: TimeSpan) (runtimeTime: TimeSpan option) : TestFramework.TestTiming =
        { Name = name
          TotalTime = totalTime
          CompileTime = None
          RuntimeTime = runtimeTime
          CacheHitCount = None
          CacheMissCount = None }

    let passTimings =
        [
            ("Parse", TimeSpan.FromMilliseconds(100.0))
            ("Cache Write: insert", TimeSpan.FromMilliseconds(200.0))
        ]
        |> Map.ofList

    let timings =
        [
            makeTiming "test-a" (TimeSpan.FromMilliseconds(400.0)) (Some (TimeSpan.FromMilliseconds(150.0)))
            makeTiming "test-b" (TimeSpan.FromMilliseconds(600.0)) (Some (TimeSpan.FromMilliseconds(50.0)))
        ]

    let totalTime = TimeSpan.FromMilliseconds(2000.0)
    let breakdown =
        TestFramework.calculateUnaccountedTimeBreakdown totalTime passTimings timings

    let expectedUnaccounted = TimeSpan.FromMilliseconds(1700.0)
    let expectedRuntime = TimeSpan.FromMilliseconds(200.0)
    let expectedOverhead = TimeSpan.FromMilliseconds(1500.0)

    if breakdown.Unaccounted <> expectedUnaccounted then
        Error $"Unexpected unaccounted total: {breakdown.Unaccounted}"
    elif breakdown.Runtime <> expectedRuntime then
        Error $"Unexpected runtime total: {breakdown.Runtime}"
    elif breakdown.Overhead <> expectedOverhead then
        Error $"Unexpected overhead total: {breakdown.Overhead}"
    else
        Ok ()

let testUnaccountedTimeWithAccountedRuntime () : TestResult =
    let makeTiming (name: string) (totalTime: TimeSpan) (runtimeTime: TimeSpan option) : TestFramework.TestTiming =
        { Name = name
          TotalTime = totalTime
          CompileTime = None
          RuntimeTime = runtimeTime
          CacheHitCount = None
          CacheMissCount = None }

    let passTimings =
        [
            ("Parse", TimeSpan.FromMilliseconds(100.0))
            (TestFramework.testRuntimeTimingName, TimeSpan.FromMilliseconds(200.0))
        ]
        |> Map.ofList

    let timings =
        [
            makeTiming "test-a" (TimeSpan.FromMilliseconds(400.0)) (Some (TimeSpan.FromMilliseconds(200.0)))
        ]

    let totalTime = TimeSpan.FromMilliseconds(1000.0)
    let breakdown =
        TestFramework.calculateUnaccountedTimeBreakdown totalTime passTimings timings

    let expectedUnaccounted = TimeSpan.FromMilliseconds(700.0)
    let expectedRuntime = TimeSpan.Zero
    let expectedOverhead = TimeSpan.FromMilliseconds(700.0)

    if breakdown.Unaccounted <> expectedUnaccounted then
        Error $"Unexpected unaccounted total: {breakdown.Unaccounted}"
    elif breakdown.Runtime <> expectedRuntime then
        Error $"Unexpected unaccounted runtime: {breakdown.Runtime}"
    elif breakdown.Overhead <> expectedOverhead then
        Error $"Unexpected overhead total: {breakdown.Overhead}"
    else
        Ok ()

let testUnaccountedTimeWithOverlappingTimings () : TestResult =
    let makeTiming (name: string) (totalTime: TimeSpan) (runtimeTime: TimeSpan option) : TestFramework.TestTiming =
        { Name = name
          TotalTime = totalTime
          CompileTime = None
          RuntimeTime = runtimeTime
          CacheHitCount = None
          CacheMissCount = None }

    let passTimings =
        [
            ("Parse", TimeSpan.FromMilliseconds(10.0))
            ("Start Function Compilation", TimeSpan.FromMilliseconds(15.0))
            ("Cache Hash Serialize: typed program", TimeSpan.FromMilliseconds(5.0))
        ]
        |> Map.ofList

    let timings =
        [
            makeTiming "test-a" (TimeSpan.FromMilliseconds(12.0)) (Some (TimeSpan.FromMilliseconds(4.0)))
        ]

    let totalTime = TimeSpan.FromMilliseconds(20.0)
    let breakdownResult =
        try
            Ok (TestFramework.calculateUnaccountedTimeBreakdown totalTime passTimings timings)
        with ex ->
            Error $"calculateUnaccountedTimeBreakdown threw: {ex.Message}"

    match breakdownResult with
    | Error msg -> Error msg
    | Ok breakdown ->
        let expectedUnaccounted = TimeSpan.FromMilliseconds(5.0)
        let expectedRuntime = TimeSpan.FromMilliseconds(4.0)
        let expectedOverhead = TimeSpan.FromMilliseconds(1.0)
        if breakdown.Unaccounted <> expectedUnaccounted then
            Error $"Unexpected unaccounted total: {breakdown.Unaccounted}"
        elif breakdown.Runtime <> expectedRuntime then
            Error $"Unexpected runtime total: {breakdown.Runtime}"
        elif breakdown.Overhead <> expectedOverhead then
            Error $"Unexpected overhead total: {breakdown.Overhead}"
        else
            Ok ()


let testOrderedStepsIncludeNonPass () : TestResult =
    let timings =
        [
            ("Parse", TimeSpan.FromMilliseconds(120.0))
            ("Compile Overhead", TimeSpan.FromMilliseconds(200.0))
            (TestFramework.testRuntimeTimingName, TimeSpan.FromMilliseconds(180.0))
            ("Cache Hash Serialize: typed program", TimeSpan.FromMilliseconds(140.0))
            ("Cache Write: insert", TimeSpan.FromMilliseconds(160.0))
        ]
        |> Map.ofList

    let order =
        [
            "Parse"
            "Compile Overhead"
            TestFramework.testRuntimeTimingName
            "Cache Hash Serialize: typed program"
            "Cache Write: insert"
        ]

    let columns = TestFramework.buildPassTimingColumns timings order TimeSpan.Zero
    let names (entries: TestFramework.PassTimingEntry list) : string list =
        entries |> List.map (fun entry -> entry.Name)
    let findEntry (target: string) (entries: TestFramework.PassTimingEntry list) =
        entries |> List.tryFind (fun entry -> entry.Name = target)

    match columns.Ordered with
    | ordered :: [] ->
        let orderedNames = names ordered.Entries
        let parserNumber =
            findEntry "Parser" ordered.Entries |> Option.bind (fun entry -> entry.Number)
        let compileOverheadNumber =
            findEntry "Compile Overhead" ordered.Entries |> Option.bind (fun entry -> entry.Number)
        let cacheHashNumber =
            findEntry "Cache Hash/Serialize total" ordered.Entries |> Option.bind (fun entry -> entry.Number)
        let cacheWriteNumber =
            findEntry "Cache Write" ordered.Entries |> Option.bind (fun entry -> entry.Number)
        let runtimeNumber =
            findEntry TestFramework.testRuntimeTimingName ordered.Entries |> Option.bind (fun entry -> entry.Number)
        if not (List.contains "Cache Write" orderedNames) then
            Error "Expected Cache Write in ordered steps"
        elif parserNumber <> Some "1" then
            Error "Expected Parser to have number 1"
        elif compileOverheadNumber.IsSome then
            Error "Expected Compile Overhead to have no numbering"
        elif cacheHashNumber.IsSome then
            Error "Expected Cache Hash/Serialize total to have no numbering"
        elif cacheWriteNumber.IsSome then
            Error "Expected Cache Write to have no numbering"
        elif runtimeNumber.IsSome then
            Error "Expected Test Runtime to have no numbering"
        elif List.contains "Other (known)" orderedNames then
            Error "Did not expect Other (known) when all timings are displayed"
        elif List.contains "Other (unknown)" orderedNames then
            Error "Did not expect Other (unknown) when unaccounted is zero"
        else
            Ok ()
    | _ ->
        Error "Expected one ordered section"

let testOverheadTotalsExcludeNestedTimings () : TestResult =
    let timings =
        [
            ("Parse", TimeSpan.FromMilliseconds(10.0))
            ("Start Function Compilation", TimeSpan.FromMilliseconds(15.0))
            ("Cache Hash Serialize: typed program", TimeSpan.FromMilliseconds(5.0))
        ]
        |> Map.ofList

    let total = TestFramework.calculatePassTimingsTotalForOverhead timings
    let expected = TimeSpan.FromMilliseconds(15.0)
    if total <> expected then
        Error $"Expected overhead total {expected}, got {total}"
    else
        Ok ()

let testOtherTimingsIncludeDefaultsWhenMissing () : TestResult =
    let columns = TestFramework.buildPassTimingColumns Map.empty [] TimeSpan.Zero
    match columns.Ordered, columns.ByTime with
    | ordered :: [], byTime :: [] ->
        if ordered.Title <> "Order" then
            Error $"Unexpected ordered title: {ordered.Title}"
        elif byTime.Title <> "By Time" then
            Error $"Unexpected by-time title: {byTime.Title}"
        elif not (List.isEmpty ordered.Entries) then
            Error "Expected ordered entries to be empty when no timings are present"
        elif not (List.isEmpty byTime.Entries) then
            Error "Expected by-time entries to be empty when no timings are present"
        else
            Ok ()
    | _ ->
        Error "Expected one section per column"

let tests : (string * (unit -> Result<unit, string>)) list =
    [
        ("pass timing columns order", testPassTimingColumnsOrdering)
        ("unaccounted time breakdown", testUnaccountedTimeBreakdown)
        ("unaccounted time with runtime accounted", testUnaccountedTimeWithAccountedRuntime)
        ("unaccounted time with overlapping timings", testUnaccountedTimeWithOverlappingTimings)
        ("ordered steps include non-pass timings", testOrderedStepsIncludeNonPass)
        ("overhead totals exclude nested timings", testOverheadTotalsExcludeNestedTimings)
        ("default other timings always present", testOtherTimingsIncludeDefaultsWhenMissing)
    ]
