// StdlibTestHarness.fs - Stdlib compile helpers for test suites
//
// Centralizes stdlib compilation and cache reset helpers for test scenarios.

module StdlibTestHarness

open CompilerLibrary

/// Compile stdlib for tests that need an explicit compilation step.
let compileStdlib () : Result<StdlibResult, string> =
    CompilerLibrary.compileStdlib()

/// No-op for backward compatibility - caching has been removed.
let resetCaches (stdlib: StdlibResult) : StdlibResult =
    stdlib

/// Run a test with stdlib provided by a supplied getter.
let withStdlib
    (getStdlib: unit -> Result<StdlibResult, string>)
    (runner: StdlibResult -> Result<unit, string>)
    : Result<unit, string> =
    match getStdlib () with
    | Error err -> Error err
    | Ok stdlib -> runner stdlib
