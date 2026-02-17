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

let tests = [
    ("detect parser/pretty roundtrip flag", testParserPrettyRoundtripFlagDetected)
    ("detect roundtrip all dark flag", testRoundtripAllDarkFlagDetected)
]
