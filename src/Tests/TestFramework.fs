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

let private updateSnapshot
    (snapshot: CacheSnapshot)
    (entries: (FunctionCacheKey * LIR.Function) list)
    : Result<CacheSnapshot, string> =
    let folder
        (accResult: Result<Map<FunctionCacheKey, byte array>, string>)
        (key, value)
        : Result<Map<FunctionCacheKey, byte array>, string> =
        accResult
        |> Result.bind (fun acc ->
            serialize value
            |> Result.map (fun bytes -> Map.add key bytes acc))
    entries
    |> List.fold folder (Ok snapshot.Entries)
    |> Result.map (fun entriesMap -> { snapshot with Entries = entriesMap })

let buildSnapshotCacheContext
    (recordCacheIo: CacheIoInfo -> unit)
    (cacheDbPath: string)
    (snapshot: CacheSnapshot)
    : CompilerLibrary.CacheContext<CacheSnapshot, LIR.Function> =
    let mergePendingWrites
        (pending: (FunctionCacheKey * LIR.Function) list)
        (entries: (FunctionCacheKey * LIR.Function) list)
        : (FunctionCacheKey * LIR.Function) list =
        let pendingMap =
            pending |> List.fold (fun acc (key, value) -> Map.add key value acc) Map.empty
        let updatedMap =
            entries |> List.fold (fun acc (key, value) -> Map.add key value acc) pendingMap
        updatedMap |> Map.toList

    let read
        (_scope: CompilerLibrary.CacheScope)
        (state: CompilerLibrary.CacheState<CacheSnapshot, LIR.Function>)
        (keys: FunctionCacheKey list)
        : Result<Map<string, LIR.Function> * CompilerLibrary.CacheState<CacheSnapshot, LIR.Function>, string> =
        getFunctionsFromSnapshot<LIR.Function> state.Snapshot keys
        |> Result.map (fun cached -> (cached, state))

    let write
        (_scope: CompilerLibrary.CacheScope)
        (state: CompilerLibrary.CacheState<CacheSnapshot, LIR.Function>)
        (entries: (FunctionCacheKey * LIR.Function) list)
        : Result<CompilerLibrary.CacheState<CacheSnapshot, LIR.Function>, string> =
        if List.isEmpty entries then
            Ok state
        else
            updateSnapshot state.Snapshot entries
            |> Result.map (fun updatedSnapshot ->
                let updatedPending = mergePendingWrites state.PendingWrites entries
                { state with Snapshot = updatedSnapshot; PendingWrites = updatedPending })

    let flush
        (_scope: CompilerLibrary.CacheScope)
        (state: CompilerLibrary.CacheState<CacheSnapshot, LIR.Function>)
        : Result<CompilerLibrary.CacheState<CacheSnapshot, LIR.Function>, string> =
        if List.isEmpty state.PendingWrites then
            Ok state
        else
            let pendingWrites = state.PendingWrites
            setFunctionsWithDbPath pendingWrites cacheDbPath
            |> Result.map (fun () ->
                recordCacheIo (CacheWrite (1, pendingWrites.Length, 1))
                { state with PendingWrites = [] })

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

    let passKeys = passDefinitions |> List.map (fun (key, _, _) -> key) |> Set.ofList

    let passEntriesInOrder =
        passDefinitions
        |> List.map (fun (timingKey, number, name) ->
            let elapsed = Map.tryFind timingKey consolidated |> Option.defaultValue TimeSpan.Zero
            { Number = Some number; Name = name; Elapsed = elapsed })

    let passEntriesByTime =
        passEntriesInOrder
        |> List.sortByDescending (fun entry -> entry.Elapsed)

    let nonPassMap =
        consolidated |> Map.filter (fun name _ -> not (Set.contains name passKeys))

    let normalizedOrder =
        passTimingOrder
        |> List.map normalizePassTimingName
        |> distinctPreserveOrder

    let orderSet = Set.ofList normalizedOrder

    let nonPassEntriesInOrder =
        normalizedOrder
        |> List.choose (fun name ->
            Map.tryFind name nonPassMap
            |> Option.map (fun elapsed -> { Number = None; Name = name; Elapsed = elapsed }))

    let nonPassRemainder =
        nonPassMap
        |> Map.toList
        |> List.choose (fun (name, elapsed) ->
            if Set.contains name orderSet then None else Some { Number = None; Name = name; Elapsed = elapsed })
        |> List.sortBy (fun entry -> entry.Name)

    let nonPassEntriesByTime =
        nonPassMap
        |> Map.toList
        |> List.map (fun (name, elapsed) -> { Number = None; Name = name; Elapsed = elapsed })
        |> List.sortByDescending (fun entry -> entry.Elapsed)

    {
        Ordered =
            [
                { Title = "Passes (Compiler Order)"; Entries = passEntriesInOrder }
                { Title = "Other Timings (Run Order)"; Entries = nonPassEntriesInOrder @ nonPassRemainder }
            ]
        ByTime =
            [
                { Title = "Passes (By Time)"; Entries = passEntriesByTime }
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
