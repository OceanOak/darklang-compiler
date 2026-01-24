// TestFrameworkTests.fs - Unit tests for cache summary helpers.
//
// Verifies cache hit/miss aggregation used in the test runner summary.

module TestFrameworkTests

open System

type TestResult = Result<unit, string>

let private timing
    (name: string)
    (hits: int option)
    (misses: int option)
    : TestFramework.TestTiming =
    { Name = name
      TotalTime = TimeSpan.Zero
      CompileTime = None
      RuntimeTime = None
      CacheHitCount = hits
      CacheMissCount = misses }

let testCalculateCacheTotals () : TestResult =
    let timings = [
        timing "one" (Some 2) (Some 3)
        timing "two" (Some 5) (Some 1)
        timing "three" None None
    ]
    match TestFramework.calculateCacheTotals timings with
    | Some totals when totals.Hits = 7 && totals.Misses = 4 -> Ok ()
    | Some totals ->
        Error $"Expected hits=7 misses=4, got hits={totals.Hits} misses={totals.Misses}"
    | None ->
        Error "Expected cache totals"

let testCalculateCacheTotalsNone () : TestResult =
    let timings = [ timing "none" None None ]
    match TestFramework.calculateCacheTotals timings with
    | None -> Ok ()
    | Some totals ->
        Error $"Expected no cache totals, got hits={totals.Hits} misses={totals.Misses}"

let tests = [
    ("cache totals sum hits and misses", testCalculateCacheTotals)
    ("cache totals ignore missing data", testCalculateCacheTotalsNone)
]
