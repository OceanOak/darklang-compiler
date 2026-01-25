// PassTimingOutputTests.fs - Unit tests for pass timing layout ordering.
//
// Ensures pass vs non-pass timing sections are ordered as expected for output.

module PassTimingOutputTests

open System

type TestResult = Result<unit, string>

let testPassTimingColumnsOrdering () : TestResult =
    let timings =
        [
            ("Parse", TimeSpan.FromMilliseconds(1.0))
            ("Type Checking", TimeSpan.FromMilliseconds(2.0))
            ("Cache Hash Serialize: typed program", TimeSpan.FromMilliseconds(5.0))
            ("Start Function Compilation", TimeSpan.FromMilliseconds(4.0))
            ("Cache Write: insert", TimeSpan.FromMilliseconds(10.0))
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
        ]

    let columns = TestFramework.buildPassTimingColumns timings order

    let names (entries: TestFramework.PassTimingEntry list) : string list =
        entries |> List.map (fun entry -> entry.Name)

    let numbers (entries: TestFramework.PassTimingEntry list) : string option list =
        entries |> List.map (fun entry -> entry.Number)

    match columns.Ordered with
    | passes :: others :: [] ->
        let passNumbers = numbers passes.Entries
        let passNames = names passes.Entries
        let otherNames = names others.Entries
        let passNumberText = String.concat ", " (passNumbers |> List.map (Option.defaultValue ""))
        let passNameText = String.concat ", " passNames
        let otherNameText = String.concat ", " otherNames
        let expectedNumbers =
            [
                "1"
                "1.5"
                "2"
                "2.3"
                "2.4"
                "2.5"
                "2.6"
                "2.7"
                "3"
                "3.1"
                "3.5"
                "4"
                "4.5"
                "5"
                "5.5"
                "6"
                "7"
            ]
        let expectedNames =
            [
                "Parser"
                "Type Checking"
                "AST to ANF"
                "ANF Optimizations"
                "ANF Inlining"
                "Ref Count Insertion"
                "Print Insertion"
                "Tail Call Optimization"
                "ANF to MIR"
                "SSA Construction"
                "MIR Optimizations"
                "MIR to LIR"
                "LIR Peephole"
                "Register Allocation"
                "Function Tree Shaking"
                "Code Generation"
                "ARM64 Emit"
            ]
        if passNumbers <> (expectedNumbers |> List.map Some) then
            Error $"Unexpected compiler-order pass numbers: {passNumberText}"
        elif passNames <> expectedNames then
            Error $"Unexpected compiler-order pass names: {passNameText}"
        elif otherNames <> [ "Cache Hash/Serialize total"; "Start Function Compilation"; "Cache Write" ] then
            Error $"Unexpected run-order non-pass names: {otherNameText}"
        else
            let astEntry =
                passes.Entries |> List.tryFind (fun entry -> entry.Name = "AST to ANF")
            match astEntry with
            | Some entry when entry.Elapsed = TimeSpan.Zero ->
                match columns.ByTime with
                | timePasses :: timeOthers :: [] ->
                    let timePassNumbers = numbers timePasses.Entries
                    let timePassNames = names timePasses.Entries
                    let timeOtherNames = names timeOthers.Entries
                    let timePassNumberText = String.concat ", " (timePassNumbers |> List.map (Option.defaultValue ""))
                    let timePassNameText = String.concat ", " timePassNames
                    let timeOtherNameText = String.concat ", " timeOtherNames
                    let timeFirstTwoNumbers = timePassNumbers |> List.truncate 2
                    let timeFirstTwoNames = timePassNames |> List.truncate 2
                    if timeFirstTwoNumbers <> [ Some "1.5"; Some "1" ] then
                        Error $"Unexpected time-ordered pass numbers: {timePassNumberText}"
                    elif timeFirstTwoNames <> [ "Type Checking"; "Parser" ] then
                        Error $"Unexpected time-ordered pass names: {timePassNameText}"
                    elif timeOtherNames <> [ "Cache Write"; "Cache Hash/Serialize total"; "Start Function Compilation" ] then
                        Error $"Unexpected time-ordered non-pass names: {timeOtherNameText}"
                    else
                        Ok ()
                | _ ->
                    Error "Expected two time-ordered sections"
            | Some _ ->
                Error "Expected AST to ANF timing to be zero when missing"
            | None ->
                Error "Expected AST to ANF to be present even when missing"
    | _ ->
        Error "Expected two compiler-order sections"

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

let tests : (string * (unit -> Result<unit, string>)) list =
    [
        ("pass timing columns order", testPassTimingColumnsOrdering)
        ("unaccounted time breakdown", testUnaccountedTimeBreakdown)
    ]
