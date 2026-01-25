// TestSummaryOutputTests.fs - Unit tests for test summary output formatting.
//
// Ensures cache hit/miss and SQLite IO summary lines are formatted consistently.

module TestSummaryOutputTests

open TestFramework

type TestResult = Result<unit, string>

let testCacheTotalsLineWithData () : TestResult =
    let totals = Some { Hits = 12; Misses = 3 }
    let line = formatCacheTotalsLine totals
    if line <> "Cache hits/misses: 12/3" then
        Error $"Unexpected cache totals line: {line}"
    else
        Ok ()

let testCacheTotalsLineWithoutData () : TestResult =
    let line = formatCacheTotalsLine None
    if line <> "Cache hits/misses: 0/0" then
        Error $"Unexpected cache totals line without data: {line}"
    else
        Ok ()

let testCacheIoTotalsLine () : TestResult =
    let totals =
        { ReadCalls = 4
          ReadQueries = 7
          ReadRows = 20
          WriteCalls = 3
          WriteInserts = 15
          WriteCommits = 2 }
    let line = formatCacheIoTotalsLine totals
    let expected =
        "SQLite IO: reads 4 (queries 7, rows 20), writes 3 (inserts 15, commits 2)"
    if line <> expected then
        Error $"Unexpected cache IO line: {line}"
    else
        Ok ()

let tests : (string * (unit -> Result<unit, string>)) list =
    [
        ("cache totals line with data", testCacheTotalsLineWithData)
        ("cache totals line without data", testCacheTotalsLineWithoutData)
        ("cache io totals line", testCacheIoTotalsLine)
    ]
