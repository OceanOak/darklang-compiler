// TestRunnerColors.fs - Shared ANSI color codes for the test runner
//
// Centralizes terminal color escape sequences used across the test runner.

module Colors

let reset = "\x1b[0m"
let green = "\x1b[32m"
let red = "\x1b[31m"
let yellow = "\x1b[33m"
let white = "\x1b[97m"
let cyan = "\x1b[36m"
let gray = "\x1b[90m"
let bold = "\x1b[1m"
