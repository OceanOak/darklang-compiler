// StdlibTestHarness.fs - Stdlib compile helpers for test suites
//
// Centralizes stdlib compilation helpers for test scenarios.

module StdlibTestHarness

open CompilerLibrary

/// Compile stdlib for tests that need an explicit compilation step.
let compileStdlib () : Result<StdlibResult, string> =
    CompilerLibrary.buildStdlib()

/// Run a test with stdlib provided by a supplied getter.
let withStdlib
    (getStdlib: unit -> Result<StdlibResult, string>)
    (runner: StdlibResult -> Result<unit, string>)
    : Result<unit, string> =
    match getStdlib () with
    | Error err -> Error err
    | Ok stdlib -> runner stdlib
