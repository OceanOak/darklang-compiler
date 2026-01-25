// SnapshotCacheTests.fs - Unit tests for snapshot cache context behavior.
//
// Verifies snapshot reads memoize deserialized functions and writes avoid touching raw entries.

module SnapshotCacheTests

open Cache
open CompilerLibrary
open TestFramework
open LIR

type TestResult = Result<unit, string>

let private createLirFunction (name: string) : LIR.Function =
    let entry = LIR.Label "entry"
    let block: LIR.BasicBlock =
        {
            Label = entry
            Instrs = [ LIR.Mov (LIR.Virtual 0, LIR.Imm 1L) ]
            Terminator = LIR.Ret
        }
    let cfg: LIR.CFG =
        {
            Entry = entry
            Blocks = Map.ofList [ (entry, block) ]
        }
    {
        Name = name
        TypedParams = []
        CFG = cfg
        StackSize = 0
        UsedCalleeSaved = []
    }

let private buildKey (compilerKey: string) (functionName: string) : FunctionCacheKey =
    {
        CacheVersion = Cache.cacheVersion
        CompilerKey = compilerKey
        OptionsHash = "opts"
        FunctionName = functionName
        FunctionHash = "hash"
    }

let testSnapshotWriteDoesNotUpdateEntries () : TestResult =
    let compilerKey = "compiler"
    let snapshot = { CompilerKey = compilerKey; Entries = Map.empty }
    let snapshotState = createSnapshotCacheState snapshot
    let cacheContext =
        TestFramework.buildSnapshotCacheContext (fun _ -> ()) "test.db" snapshotState
    match cacheContext with
    | CompilerLibrary.NoCache -> Error "Expected snapshot cache context"
    | CompilerLibrary.CacheContext ctx ->
        let func = createLirFunction "cached_func"
        let key = buildKey compilerKey func.Name
        ctx.Write (CompilerLibrary.CacheScope.TestExpression "snapshot-write") ctx.State [ (key, func) ]
        |> Result.bind (fun state ->
            let rawCount = Map.count state.Snapshot.RawSnapshot.Entries
            let cachedCount = Map.count state.Snapshot.CachedEntries
            let pendingCount = Map.count state.Snapshot.PendingWrites
            if rawCount <> 0 then
                Error $"Expected raw entries to remain empty, got {rawCount}"
            elif cachedCount <> 1 then
                Error $"Expected cached entries to contain 1 value, got {cachedCount}"
            elif pendingCount <> 1 then
                Error $"Expected pending writes to contain 1 value, got {pendingCount}"
            else
                Ok ())

let testSnapshotReadMemoizesEntries () : TestResult =
    let compilerKey = "compiler"
    let func = createLirFunction "read_func"
    let key = buildKey compilerKey func.Name
    Cache.serialize func
    |> Result.bind (fun bytes ->
        let snapshot = { CompilerKey = compilerKey; Entries = Map.ofList [ (key, bytes) ] }
        let snapshotState = createSnapshotCacheState snapshot
        let cacheContext =
            TestFramework.buildSnapshotCacheContext (fun _ -> ()) "test.db" snapshotState
        match cacheContext with
        | CompilerLibrary.NoCache -> Error "Expected snapshot cache context"
        | CompilerLibrary.CacheContext ctx ->
            let scope = CompilerLibrary.CacheScope.TestExpression "snapshot-read"
            ctx.Read scope ctx.State [ key ]
            |> Result.bind (fun (cached, stateAfterFirstRead) ->
                if not (Map.containsKey func.Name cached) then
                    Error "Expected cache to return deserialized function"
                else
                    let cachedCount = Map.count stateAfterFirstRead.Snapshot.CachedEntries
                    if cachedCount <> 1 then
                        Error $"Expected cached entries to contain 1 value, got {cachedCount}"
                    else
                        ctx.Read scope stateAfterFirstRead [ key ]
                        |> Result.bind (fun (_cachedAgain, stateAfterSecondRead) ->
                            let cachedCountAgain = Map.count stateAfterSecondRead.Snapshot.CachedEntries
                            if cachedCountAgain <> 1 then
                                Error $"Expected cached entries to remain at 1 value, got {cachedCountAgain}"
                            else
                                Ok ())))

let tests : (string * (unit -> Result<unit, string>)) list =
    [
        ("snapshot write avoids raw entry update", testSnapshotWriteDoesNotUpdateEntries)
        ("snapshot read memoizes entries", testSnapshotReadMemoizesEntries)
    ]
