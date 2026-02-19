// TestRunnerArgsTests.fs - Unit tests for test runner CLI argument helpers.
//
// Verifies that test-runner-only flags are detected correctly.

module TestRunnerArgsTests

open TestRunnerArgs

type TestResult = Result<unit, string>

let testParserPrettyRoundtripFlagDetected () : TestResult =
    let args = [| "--parser-pretty-roundtrip" |]
    if hasParserPrettyRoundtripArg args then
        Ok ()
    else
        Error "Expected --parser-pretty-roundtrip to be detected"

let testRoundtripAllDarkFlagDetected () : TestResult =
    let args = [| "--roundtrip-all-dark" |]
    if hasRoundtripAllDarkArg args then
        Ok ()
    else
        Error "Expected --roundtrip-all-dark to be detected"

let testAllTestTimingsFlagDetected () : TestResult =
    let args = [| "--all-test-timings" |]
    if hasAllTestTimingsArg args then
        Ok ()
    else
        Error "Expected --all-test-timings to be detected"

let testTimingsJsonArgParsed () : TestResult =
    let args = [| "--timings-json=/tmp/results.json" |]
    match parseTimingsJsonArg args with
    | Some "/tmp/results.json" -> Ok ()
    | Some value -> Error $"Expected timings json path '/tmp/results.json', got '{value}'"
    | None -> Error "Expected --timings-json=... to be parsed"

let tests = [
    ("detect parser/pretty roundtrip flag", testParserPrettyRoundtripFlagDetected)
    ("detect roundtrip all dark flag", testRoundtripAllDarkFlagDetected)
    ("detect all test timings flag", testAllTestTimingsFlagDetected)
    ("parse timings json path arg", testTimingsJsonArgParsed)
]
