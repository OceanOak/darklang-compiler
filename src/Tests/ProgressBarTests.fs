// ProgressBarTests.fs - Unit tests for the test runner progress bar
//
// Ensures the progress bar handles over-completion without crashing.

module ProgressBarTests

open System
open System.IO

type TestResult = Result<unit, string>

type private FlushTrackingWriter() =
    inherit StringWriter()
    let mutable flushed = false
    member _.Flushed = flushed
    override _.Flush() =
        flushed <- true
        base.Flush()

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

let testProgressBarFlushesOutput () : TestResult =
    let writer = new FlushTrackingWriter()
    let original = Console.Error
    Console.SetError writer
    let state = ProgressBar.create "Progress" 1
    ProgressBar.update state
    Console.SetError original
    if writer.Flushed then Ok () else Error "ProgressBar.update did not flush stderr"

let tests = [
    ("handles over-completion", testProgressBarHandlesOverCompletion)
    ("flushes output", testProgressBarFlushesOutput)
]
