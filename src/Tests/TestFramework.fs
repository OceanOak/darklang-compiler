// TestFramework.fs - Generic test runner helpers.
//
// Provides shared types and utilities that are independent of any specific test suite.

module TestFramework

open System
open System.Diagnostics
open Output
open Cache

type UnitTestCase = string * (unit -> Result<unit, string>)

type UnitTestSuite = {
    Name: string
    Tests: UnitTestCase list
}

// Failed test info for summary at end
type FailedTestInfo = {
    File: string
    Name: string
    Message: string
    Details: string list  // Additional details like expected/actual
}

// Test timing info for slowest tests report
type TestTiming = {
    Name: string
    TotalTime: TimeSpan
    CompileTime: TimeSpan option
    RuntimeTime: TimeSpan option
    CacheHitCount: int option
    CacheMissCount: int option
}

type PassTimingEntry = {
    Number: string option
    Name: string
    Elapsed: TimeSpan
}

type PassTimingSection = {
    Title: string
    Entries: PassTimingEntry list
}

type PassTimingColumns = {
    Ordered: PassTimingSection list
    ByTime: PassTimingSection list
}

type UnaccountedTimeBreakdown = {
    Unaccounted: TimeSpan
    Runtime: TimeSpan
    Overhead: TimeSpan
}

let testRuntimeTimingName = "Test Runtime"

type CacheTotals = {
    Hits: int
    Misses: int
}

type CacheIoTotals = {
    ReadCalls: int
    ReadQueries: int
    ReadRows: int
    WriteCalls: int
    WriteInserts: int
    WriteCommits: int
}

type CacheIoInfo =
    | CacheRead of readCalls:int * readQueries:int * readRows:int
    | CacheWrite of writeCalls:int * writeInserts:int * writeCommits:int

// Summary of per-file test suite results
type FileSuiteSummary = {
    Passed: int
    Failed: int
    FailedTests: FailedTestInfo list
}

type TestRunState = {
    mutable Passed: int
    mutable Failed: int
    FailedTests: ResizeArray<FailedTestInfo>
    Timings: ResizeArray<TestTiming>
    mutable PassTimings: Map<string, TimeSpan>
    PassTimingOrder: ResizeArray<string>
    mutable CacheIoTotals: CacheIoTotals
}

type OutputSymbols = {
    Pass: string
    Fail: string
    SectionPrefix: string
}

let createState () : TestRunState =
    { Passed = 0
      Failed = 0
      FailedTests = ResizeArray()
      Timings = ResizeArray()
      PassTimings = Map.empty
      PassTimingOrder = ResizeArray()
      CacheIoTotals = {
          ReadCalls = 0
          ReadQueries = 0
          ReadRows = 0
          WriteCalls = 0
          WriteInserts = 0
          WriteCommits = 0
      } }

let recordTiming (state: TestRunState) (timing: TestTiming) : unit =
    state.Timings.Add timing

let recordPassTiming (state: TestRunState) (timing: CompilerLibrary.PassTiming) : unit =
    if not (Map.containsKey timing.Pass state.PassTimings) then
        state.PassTimingOrder.Add timing.Pass
    let existing =
        Map.tryFind timing.Pass state.PassTimings
        |> Option.defaultValue TimeSpan.Zero
    let updated = existing + timing.Elapsed
    state.PassTimings <- Map.add timing.Pass updated state.PassTimings

let recordCacheIo (state: TestRunState) (info: CacheIoInfo) : unit =
    match info with
    | CacheRead (readCalls, readQueries, readRows) ->
        let current = state.CacheIoTotals
        state.CacheIoTotals <- {
            current with
                ReadCalls = current.ReadCalls + readCalls
                ReadQueries = current.ReadQueries + readQueries
                ReadRows = current.ReadRows + readRows
        }
    | CacheWrite (writeCalls, writeInserts, writeCommits) ->
        let current = state.CacheIoTotals
        state.CacheIoTotals <- {
            current with
                WriteCalls = current.WriteCalls + writeCalls
                WriteInserts = current.WriteInserts + writeInserts
                WriteCommits = current.WriteCommits + writeCommits
        }

type SnapshotCacheState = {
    RawSnapshot: CacheSnapshot
    CachedEntries: Map<FunctionCacheKey, LIR.Function>
    PendingWrites: Map<FunctionCacheKey, LIR.Function>
}

let createSnapshotCacheState (snapshot: CacheSnapshot) : SnapshotCacheState =
    { RawSnapshot = snapshot; CachedEntries = Map.empty; PendingWrites = Map.empty }

let buildSnapshotCacheContext
    (recordCacheIo: CacheIoInfo -> unit)
    (cacheDbPath: string)
    (snapshot: SnapshotCacheState)
    : CompilerLibrary.CacheContext<SnapshotCacheState, LIR.Function> =
    let addEntriesToMap
        (entries: (FunctionCacheKey * LIR.Function) list)
        (initial: Map<FunctionCacheKey, LIR.Function>)
        : Map<FunctionCacheKey, LIR.Function> =
        entries |> List.fold (fun acc (key, value) -> Map.add key value acc) initial

    let read
        (_scope: CompilerLibrary.CacheScope)
        (state: CompilerLibrary.CacheState<SnapshotCacheState, LIR.Function>)
        (keys: FunctionCacheKey list)
        : Result<Map<string, LIR.Function> * CompilerLibrary.CacheState<SnapshotCacheState, LIR.Function>, string> =
        if List.isEmpty keys then
            Ok (Map.empty, state)
        else
            match keys |> List.tryFind (fun key -> key.CompilerKey <> state.Snapshot.RawSnapshot.CompilerKey) with
            | Some key ->
                Crash.crash $"Cache snapshot compiler key mismatch: expected {state.Snapshot.RawSnapshot.CompilerKey}, got {key.CompilerKey}"
            | None ->
                let folder
                    (accResult: Result<Map<string, LIR.Function> * Map<FunctionCacheKey, LIR.Function>, string>)
                    (key: FunctionCacheKey)
                    : Result<Map<string, LIR.Function> * Map<FunctionCacheKey, LIR.Function>, string> =
                    accResult
                    |> Result.bind (fun (acc, cached) ->
                        match Map.tryFind key cached with
                        | Some value -> Ok (Map.add key.FunctionName value acc, cached)
                        | None ->
                            match Map.tryFind key state.Snapshot.RawSnapshot.Entries with
                            | None -> Ok (acc, cached)
                            | Some bytes ->
                                deserialize<LIR.Function> bytes
                                |> Result.map (fun value ->
                                    let updatedCached = Map.add key value cached
                                    (Map.add key.FunctionName value acc, updatedCached)))

                let initial = Ok (Map.empty, state.Snapshot.CachedEntries)
                keys
                |> List.fold folder initial
                |> Result.map (fun (cachedMap, updatedCached) ->
                    let updatedSnapshot = { state.Snapshot with CachedEntries = updatedCached }
                    (cachedMap, { state with Snapshot = updatedSnapshot }))

    let write
        (_scope: CompilerLibrary.CacheScope)
        (state: CompilerLibrary.CacheState<SnapshotCacheState, LIR.Function>)
        (entries: (FunctionCacheKey * LIR.Function) list)
        : Result<CompilerLibrary.CacheState<SnapshotCacheState, LIR.Function>, string> =
        if List.isEmpty entries then
            Ok state
        else
            let updatedCached =
                addEntriesToMap entries state.Snapshot.CachedEntries
            let updatedPending =
                addEntriesToMap entries state.Snapshot.PendingWrites
            let updatedSnapshot =
                { state.Snapshot with CachedEntries = updatedCached; PendingWrites = updatedPending }
            Ok { state with Snapshot = updatedSnapshot }

    let flush
        (_scope: CompilerLibrary.CacheScope)
        (state: CompilerLibrary.CacheState<SnapshotCacheState, LIR.Function>)
        : Result<CompilerLibrary.CacheState<SnapshotCacheState, LIR.Function>, string> =
        if Map.isEmpty state.Snapshot.PendingWrites then
            Ok state
        else
            let pendingWrites = state.Snapshot.PendingWrites |> Map.toList
            let pendingCount = Map.count state.Snapshot.PendingWrites
            setFunctionsWithDbPath pendingWrites cacheDbPath
            |> Result.map (fun () ->
                recordCacheIo (CacheWrite (1, pendingCount, 1))
                let updatedSnapshot = { state.Snapshot with PendingWrites = Map.empty }
                { state with Snapshot = updatedSnapshot })

    CompilerLibrary.CacheContext {
        State = { Snapshot = snapshot; PendingWrites = [] }
        Read = read
        Write = write
        Flush = flush
    }

let recordResults
    (state: TestRunState)
    (passedDelta: int)
    (failedDelta: int)
    (failedTestsDelta: FailedTestInfo list)
    : unit =
    state.Passed <- state.Passed + passedDelta
    state.Failed <- state.Failed + failedDelta
    for test in failedTestsDelta do
        state.FailedTests.Add test

// Format elapsed time
let formatTime (elapsed: TimeSpan) =
    if elapsed.TotalMilliseconds < 1000.0 then
        let ms = elapsed.TotalMilliseconds.ToString("0")
        $"{ms}ms"
    else
        let seconds = elapsed.TotalSeconds.ToString("0.00")
        $"{seconds}s"

let formatCacheMissCount (missCountOpt: int option) : string option =
    match missCountOpt with
    | Some count when count > 0 -> Some $"cache-misses: {count}"
    | _ -> None

let calculateCacheTotals (timings: seq<TestTiming>) : CacheTotals option =
    let folder (hits, misses, sawData) (timing: TestTiming) =
        match timing.CacheHitCount, timing.CacheMissCount with
        | Some hitCount, Some missCount ->
            (hits + hitCount, misses + missCount, true)
        | None, None ->
            (hits, misses, sawData)
        | _ ->
            Crash.crash "calculateCacheTotals: cache hit/miss counts must be recorded together"
    let (hits, misses, sawData) =
        timings |> Seq.fold folder (0, 0, false)
    if sawData then Some { Hits = hits; Misses = misses } else None

let formatCacheTotalsLine (totalsOpt: CacheTotals option) : string =
    let totals =
        match totalsOpt with
        | Some value -> value
        | None -> { Hits = 0; Misses = 0 }
    $"Cache hits/misses: {totals.Hits}/{totals.Misses}"

let formatCacheIoTotalsLine (totals: CacheIoTotals) : string =
    $"SQLite IO: reads {totals.ReadCalls} (queries {totals.ReadQueries}, rows {totals.ReadRows}), writes {totals.WriteCalls} (inserts {totals.WriteInserts}, commits {totals.WriteCommits})"


let consolidateCacheHashSerializeTimings
    (passTimings: Map<string, TimeSpan>)
    : Map<string, TimeSpan> =
    let isCacheHashSerialize (name: string) : bool =
        name.StartsWith("Cache Hash ", StringComparison.Ordinal)
        || name.StartsWith("Cache Serialize:", StringComparison.Ordinal)

    let folder
        (name: string)
        (elapsed: TimeSpan)
        (cacheTotal: TimeSpan, acc: Map<string, TimeSpan>)
        : TimeSpan * Map<string, TimeSpan> =
        if isCacheHashSerialize name then
            (cacheTotal + elapsed, acc)
        else
            (cacheTotal, Map.add name elapsed acc)

    let (cacheTotal, pruned) =
        passTimings |> Map.fold (fun acc name elapsed -> folder name elapsed acc) (TimeSpan.Zero, Map.empty)

    if cacheTotal > TimeSpan.Zero then
        Map.add "Cache Hash/Serialize total" cacheTotal pruned
    else
        pruned

let consolidateCacheWriteTimings
    (passTimings: Map<string, TimeSpan>)
    : Map<string, TimeSpan> =
    let isCacheWrite (name: string) : bool =
        name.StartsWith("Cache Write:", StringComparison.Ordinal)

    let hasParentWrite = Map.containsKey "Cache Write" passTimings

    let folder
        (name: string)
        (elapsed: TimeSpan)
        (cacheTotal: TimeSpan, sawWrite: bool, acc: Map<string, TimeSpan>)
        : TimeSpan * bool * Map<string, TimeSpan> =
        if isCacheWrite name then
            (cacheTotal + elapsed, true, acc)
        else
            (cacheTotal, sawWrite, Map.add name elapsed acc)

    if hasParentWrite then
        passTimings
        |> Map.filter (fun name _ -> not (isCacheWrite name))
    else
        let (cacheTotal, sawWrite, pruned) =
            passTimings |> Map.fold (fun acc name elapsed -> folder name elapsed acc) (TimeSpan.Zero, false, Map.empty)

        if sawWrite then
            Map.add "Cache Write" cacheTotal pruned
        else
            pruned

let calculatePassTimingsTotal (passTimings: Map<string, TimeSpan>) : TimeSpan =
    passTimings
    |> consolidateCacheHashSerializeTimings
    |> consolidateCacheWriteTimings
    |> Map.fold (fun acc _ elapsed -> acc + elapsed) TimeSpan.Zero

let calculatePassTimingsTotalForOverhead (passTimings: Map<string, TimeSpan>) : TimeSpan =
    let overlapTimingNames =
        Set.ofList [
            "Start Function Compilation"
        ]
    passTimings
    |> consolidateCacheHashSerializeTimings
    |> consolidateCacheWriteTimings
    |> Map.filter (fun name _ -> not (Set.contains name overlapTimingNames))
    |> Map.fold (fun acc _ elapsed -> acc + elapsed) TimeSpan.Zero

let calculateUnaccountedTimeBreakdown
    (totalTime: TimeSpan)
    (passTimings: Map<string, TimeSpan>)
    (timings: seq<TestTiming>)
    : UnaccountedTimeBreakdown =
    let passTimingTotal = calculatePassTimingsTotalForOverhead passTimings
    let unaccounted = totalTime - passTimingTotal
    let runtimeTotal =
        timings
        |> Seq.choose (fun timing -> timing.RuntimeTime)
        |> Seq.fold (fun acc runtime -> acc + runtime) TimeSpan.Zero
    let runtimeAccounted =
        Map.tryFind testRuntimeTimingName passTimings
        |> Option.defaultValue TimeSpan.Zero
    let runtimeUnaccounted = runtimeTotal - runtimeAccounted
    let overhead = unaccounted - runtimeUnaccounted
    { Unaccounted = unaccounted; Runtime = runtimeUnaccounted; Overhead = overhead }

let private normalizePassTimingName (name: string) : string =
    if name.StartsWith("Cache Hash ", StringComparison.Ordinal)
       || name.StartsWith("Cache Serialize:", StringComparison.Ordinal) then
        "Cache Hash/Serialize total"
    elif name.StartsWith("Cache Write:", StringComparison.Ordinal) then
        "Cache Write"
    else
        name

let private distinctPreserveOrder (names: string list) : string list =
    let folder (seen: Set<string>, acc: string list) (name: string) =
        if Set.contains name seen then
            (seen, acc)
        else
            (Set.add name seen, acc @ [ name ])
    names |> List.fold folder (Set.empty, []) |> snd

let buildPassTimingColumns
    (passTimings: Map<string, TimeSpan>)
    (passTimingOrder: string list)
    : PassTimingColumns =
    let consolidated =
        passTimings
        |> consolidateCacheHashSerializeTimings
        |> consolidateCacheWriteTimings

    let passDefinitions : (string * string * string) list =
        [
            ("Parse", "1", "Parser")
            ("Type Checking", "1.5", "Type Checking")
            ("AST -> ANF", "2", "AST to ANF")
            ("ANF Optimizations", "2.3", "ANF Optimizations")
            ("ANF Inlining", "2.4", "ANF Inlining")
            ("Reference Count Insertion", "2.5", "Ref Count Insertion")
            ("Print Insertion", "2.6", "Print Insertion")
            ("Tail Call Detection", "2.7", "Tail Call Optimization")
            ("ANF -> MIR", "3", "ANF to MIR")
            ("SSA Construction", "3.1", "SSA Construction")
            ("MIR Optimizations", "3.5", "MIR Optimizations")
            ("MIR -> LIR", "4", "MIR to LIR")
            ("LIR Peephole", "4.5", "LIR Peephole")
            ("Register Allocation", "5", "Register Allocation")
            ("Function Tree Shaking", "5.5", "Function Tree Shaking")
            ("Code Generation", "6", "Code Generation")
            ("ARM64 Emit", "7", "ARM64 Emit")
        ]

    let orderedDefinitions : (string * string option * string * bool) list =
        [
            ("Cache Deserialize", None, "Cache Deserialize", true)
            ("Stdlib Build Overhead", None, "Stdlib Build Overhead", true)
            ("E2E Test Parse", None, "E2E Test Parse", true)
            ("E2E Suite Context Overhead", None, "E2E Suite Context Overhead", true)
            ("Verification Test Parse", None, "Verification Test Parse", true)
            ("Verification Suite Context Overhead", None, "Verification Suite Context Overhead", true)
        ]
        @ (passDefinitions
           |> List.map (fun (timingKey, number, name) ->
               (timingKey, Some number, name, true)))
        @ [
            ("Compile Overhead", None, "Compile Overhead", true)
            ("Cache Hash/Serialize total", None, "Cache Hash/Serialize total", true)
            (testRuntimeTimingName, None, testRuntimeTimingName, true)
            ("Cache Flush", None, "Cache Flush", true)
        ]

    let orderedKeys =
        orderedDefinitions |> List.map (fun (key, _, _, _) -> key) |> Set.ofList

    let orderedEntries =
        orderedDefinitions
        |> List.choose (fun (timingKey, number, name, alwaysInclude) ->
            match Map.tryFind timingKey consolidated with
            | Some elapsed -> Some { Number = number; Name = name; Elapsed = elapsed }
            | None when alwaysInclude -> Some { Number = number; Name = name; Elapsed = TimeSpan.Zero }
            | None -> None)

    let orderedEntriesByTime =
        orderedEntries
        |> List.sortByDescending (fun entry -> entry.Elapsed)

    let nonOrderedMap =
        consolidated |> Map.filter (fun name _ -> not (Set.contains name orderedKeys))

    let normalizedOrder =
        passTimingOrder
        |> List.map normalizePassTimingName
        |> distinctPreserveOrder

    let orderSet = Set.ofList normalizedOrder

    let nonPassDefinitions = [ "Start Function Compilation"; "Cache Write" ]
    let nonPassDefinitionSet = Set.ofList nonPassDefinitions

    let entryForNonPass (name: string) : PassTimingEntry =
        let elapsed = Map.tryFind name nonOrderedMap |> Option.defaultValue TimeSpan.Zero
        { Number = None; Name = name; Elapsed = elapsed }

    let nonPassEntriesInOrder =
        normalizedOrder
        |> List.filter (fun name -> Set.contains name nonPassDefinitionSet)
        |> List.map entryForNonPass

    let nonPassRemainderDefinitions =
        nonPassDefinitions
        |> List.filter (fun name -> not (Set.contains name orderSet))
        |> List.map entryForNonPass

    let nonPassRemainderOther =
        nonOrderedMap
        |> Map.toList
        |> List.choose (fun (name, elapsed) ->
            if Set.contains name orderSet || Set.contains name nonPassDefinitionSet then
                None
            else
                Some { Number = None; Name = name; Elapsed = elapsed })
        |> List.sortBy (fun entry -> entry.Name)

    let nonPassEntriesByTime =
        let baseEntries = nonPassDefinitions |> List.map entryForNonPass
        let extraEntries =
            nonOrderedMap
            |> Map.toList
            |> List.choose (fun (name, elapsed) ->
                if Set.contains name nonPassDefinitionSet then None
                else Some { Number = None; Name = name; Elapsed = elapsed })
        (baseEntries @ extraEntries)
        |> List.sortByDescending (fun entry -> entry.Elapsed)

    {
        Ordered =
            [
                { Title = "Ordered Steps (Suite Order)"; Entries = orderedEntries }
                { Title = "Other Timings (Grouped)"; Entries = nonPassEntriesInOrder @ nonPassRemainderDefinitions @ nonPassRemainderOther }
            ]
        ByTime =
            [
                { Title = "Ordered Steps (By Time)"; Entries = orderedEntriesByTime }
                { Title = "Other Timings (By Time)"; Entries = nonPassEntriesByTime }
            ]
    }

let addExpectedActualDetails (expected: string option) (actual: string option) : string list =
    let details = ResizeArray<string>()
    match expected, actual with
    | Some exp, Some act ->
        println "    Expected:"
        for line in exp.Split('\n') do
            println $"      {line}"
            details.Add($"Expected: {line}")
        println "    Actual:"
        for line in act.Split('\n') do
            println $"      {line}"
            details.Add($"Actual: {line}")
    | _ -> ()
    details |> Seq.toList

let runFileSuite
    (state: TestRunState)
    (symbols: OutputSymbols)
    (suiteTitle: string)
    (progressLabel: string)
    (testFiles: string array)
    (getTestName: string -> string)
    (formatTimingName: string -> string)
    (runFile: string -> Result<'result, string>)
    (handleSuccess: ProgressBar.State -> string -> string -> TimeSpan -> 'result -> FileSuiteSummary)
    (handleError: ProgressBar.State -> string -> string -> TimeSpan -> string -> FileSuiteSummary)
    : unit =
    if testFiles.Length > 0 then
        let sectionTimer = Stopwatch.StartNew()
        println $"{Colors.cyan}{suiteTitle}{Colors.reset}"

        let mutable sectionPassed = 0
        let mutable sectionFailed = 0
        let progress = ProgressBar.create progressLabel testFiles.Length
        ProgressBar.update progress

        for testPath in testFiles do
            let testName = getTestName testPath
            let testTimer = Stopwatch.StartNew()
            let summary =
                match runFile testPath with
                | Ok result ->
                    testTimer.Stop()
                    handleSuccess progress testPath testName testTimer.Elapsed result
                | Error msg ->
                    testTimer.Stop()
                    handleError progress testPath testName testTimer.Elapsed msg
            recordTiming state {
                Name = formatTimingName testName
                TotalTime = testTimer.Elapsed
                CompileTime = None
                RuntimeTime = None
                CacheHitCount = None
                CacheMissCount = None
            }
            sectionPassed <- sectionPassed + summary.Passed
            sectionFailed <- sectionFailed + summary.Failed
            recordResults state summary.Passed summary.Failed summary.FailedTests

        ProgressBar.finish progress
        sectionTimer.Stop()
        if sectionFailed = 0 then
            println $"  {Colors.green}{symbols.Pass} {sectionPassed} passed{Colors.reset}"
        else
            println $"  {Colors.green}{symbols.Pass} {sectionPassed} passed{Colors.reset}, {Colors.red}{symbols.Fail} {sectionFailed} failed{Colors.reset}"
        println $"  {Colors.gray}{symbols.SectionPrefix} Completed in {formatTime sectionTimer.Elapsed}{Colors.reset}"
        println ""

let runUnitTestSuites
    (state: TestRunState)
    (symbols: OutputSymbols)
    (suiteTitle: string)
    (progressLabel: string)
    (suites: UnitTestSuite array)
    : unit =
    let unitSectionTimer = Stopwatch.StartNew()
    println $"{Colors.cyan}{suiteTitle}{Colors.reset}"

    let mutable unitSectionPassed = 0
    let mutable unitSectionFailed = 0
    let unitFailedTests = ResizeArray<FailedTestInfo>()

    let totalUnitTests =
        suites
        |> Array.sumBy (fun suite -> suite.Tests.Length)
    let unitProgress = ProgressBar.create progressLabel totalUnitTests
    ProgressBar.update unitProgress

    for suite in suites do
        for (testName, runTest) in suite.Tests do
            let timer = Stopwatch.StartNew()
            let displayName = $"{suite.Name}: {testName}: "
            match runTest() with
            | Ok () ->
                timer.Stop()
                recordTiming state {
                    Name = $"Unit: {displayName}"
                    TotalTime = timer.Elapsed
                    CompileTime = None
                    RuntimeTime = None
                    CacheHitCount = None
                    CacheMissCount = None
                }
                unitSectionPassed <- unitSectionPassed + 1
                ProgressBar.increment unitProgress true
            | Error msg ->
                timer.Stop()
                recordTiming state {
                    Name = $"Unit: {suite.Name}: {testName}"
                    TotalTime = timer.Elapsed
                    CompileTime = None
                    RuntimeTime = None
                    CacheHitCount = None
                    CacheMissCount = None
                }
                recordTiming state {
                    Name = $"Unit: {displayName}"
                    TotalTime = timer.Elapsed
                    CompileTime = None
                    RuntimeTime = None
                    CacheHitCount = None
                    CacheMissCount = None
                }
                ProgressBar.increment unitProgress false
                ProgressBar.finish unitProgress
                println $"  {displayName}... {Colors.red}{symbols.Fail} FAIL{Colors.reset} {Colors.gray}({formatTime timer.Elapsed}){Colors.reset}"
                println $"    {msg}"
                unitFailedTests.Add({ File = ""; Name = $"Unit: {displayName}"; Message = msg; Details = [] })
                unitSectionFailed <- unitSectionFailed + 1
                ProgressBar.update unitProgress

    ProgressBar.finish unitProgress
    unitSectionTimer.Stop()
    if unitSectionFailed = 0 then
        println $"  {Colors.green}{symbols.Pass} {unitSectionPassed} passed{Colors.reset}"
    else
        println $"  {Colors.green}{symbols.Pass} {unitSectionPassed} passed{Colors.reset}, {Colors.red}{symbols.Fail} {unitSectionFailed} failed{Colors.reset}"
    println $"  {Colors.gray}{symbols.SectionPrefix} Completed in {formatTime unitSectionTimer.Elapsed}{Colors.reset}"
    println ""
    recordResults state unitSectionPassed unitSectionFailed (unitFailedTests |> Seq.toList)
