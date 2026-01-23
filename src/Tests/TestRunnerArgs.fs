// TestRunnerArgs.fs - Helper functions for test runner CLI arguments
//
// Provides parsing helpers shared by the test runner and its unit tests.

module TestRunnerArgs

// Parse command line for --filter=PATTERN option
let parseFilterArg (args: string array) : string option =
    args
    |> Array.tryFind (fun arg -> arg.StartsWith("--filter="))
    |> Option.map (fun arg -> arg.Substring(9))

// Check if --coverage flag is present (show inline coverage after tests)
let hasCoverageArg (args: string array) : bool =
    args |> Array.exists (fun arg -> arg = "--coverage")

// Check if --verification flag is present (enable verification/stress tests)
let hasVerificationArg (args: string array) : bool =
    args |> Array.exists (fun arg -> arg = "--verification")

// Check if --no-cache flag is present (disable compiler cache)
let hasNoCacheArg (args: string array) : bool =
    args |> Array.exists (fun arg -> arg = "--no-cache")

// Check if --verbose flag is present (print failing tests immediately)
let hasVerboseArg (args: string array) : bool =
    args |> Array.exists (fun arg -> arg = "--verbose" || arg = "-v")

// Check if a test name matches the filter (case-insensitive substring match)
let matchesFilter (filter: string option) (testName: string) : bool =
    match filter with
    | None -> true
    | Some pattern -> testName.ToLowerInvariant().Contains(pattern.ToLowerInvariant())

// Check if --help flag is present
let hasHelpArg (args: string array) : bool =
    args |> Array.exists (fun arg -> arg = "--help" || arg = "-h")
