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
            ("AST -> ANF", TimeSpan.FromMilliseconds(3.0))
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

    let labels (entries: TestFramework.PassTimingEntry list) : string list =
        entries |> List.map (fun entry -> entry.Label)

    match columns.Ordered with
    | passes :: others :: [] ->
        let passLabels = labels passes.Entries
        let otherLabels = labels others.Entries
        let passLabelText = String.concat ", " passLabels
        let otherLabelText = String.concat ", " otherLabels
        if passLabels <> [ "1 Parser"; "1.5 Type Checking"; "2 AST to ANF" ] then
            Error $"Unexpected compiler-order pass labels: {passLabelText}"
        elif otherLabels <> [ "Cache Hash/Serialize total"; "Start Function Compilation"; "Cache Write" ] then
            Error $"Unexpected run-order non-pass labels: {otherLabelText}"
        else
            match columns.ByTime with
            | timePasses :: timeOthers :: [] ->
                let timePassLabels = labels timePasses.Entries
                let timeOtherLabels = labels timeOthers.Entries
                let timePassText = String.concat ", " timePassLabels
                let timeOtherText = String.concat ", " timeOtherLabels
                if timePassLabels <> [ "2 AST to ANF"; "1.5 Type Checking"; "1 Parser" ] then
                    Error $"Unexpected time-ordered pass labels: {timePassText}"
                elif timeOtherLabels <> [ "Cache Write"; "Cache Hash/Serialize total"; "Start Function Compilation" ] then
                    Error $"Unexpected time-ordered non-pass labels: {timeOtherText}"
                else
                    Ok ()
            | _ ->
                Error "Expected two time-ordered sections"
    | _ ->
        Error "Expected two compiler-order sections"

let tests : (string * (unit -> Result<unit, string>)) list =
    [
        ("pass timing columns order", testPassTimingColumnsOrdering)
    ]
