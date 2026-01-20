// TraceTests.fs - Unit tests for trace formatting helpers
//
// Validates the stable text format used for trace output.

module TraceTests

type TestResult = Result<unit, string>

let testFormatEvent () : TestResult =
    let actual = Trace.formatEvent "stdlib.compile.start" [("source", "tests"); ("count", "3")]
    let expected = "TRACE event=stdlib.compile.start source=tests count=3"
    if actual = expected then
        Ok ()
    else
        Error $"Expected '{expected}', got '{actual}'"

let tests = [
    ("format event", testFormatEvent)
]
