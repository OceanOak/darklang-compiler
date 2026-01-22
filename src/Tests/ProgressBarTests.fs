// ProgressBarTests.fs - Unit tests for the test runner progress bar
//
// Ensures the progress bar handles over-completion without crashing.

module ProgressBarTests

type TestResult = Result<unit, string>

let testProgressBarHandlesOverCompletion () : TestResult =
    let state = ProgressBar.create "Progress" 1
    let result =
        try
            ProgressBar.increment state true
            ProgressBar.increment state true
            Ok ()
        with ex ->
            Error $"ProgressBar threw exception: {ex.Message}"
    result

let tests = [
    ("handles over-completion", testProgressBarHandlesOverCompletion)
]
