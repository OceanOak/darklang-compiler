// CompilerCacheTests.fs - Unit tests for compiler cache invalidation behavior
//
// Verifies that cache keys ignore non-inlined callee bodies when signatures are stable.

module CompilerCacheTests

open System
open CompilerLibrary

/// Test result type
type TestResult = Result<unit, string>

let private compileWithCacheMisses
    (stdlib: StdlibResult)
    (options: CompilerOptions)
    (source: string)
    : Result<int, string> =
    let mutable missCount = 0
    let recordMiss (info: CacheMissInfo) : unit =
        missCount <- missCount + info.Misses

    let request : CompileRequest = {
        Context = StdlibOnly stdlib
        Mode = FullProgram
        Source = source
        SourceFile = "CompilerCacheTests.dark"
        AllowInternal = false
        Verbosity = 0
        Options = options
        PassTimingRecorder = None
        CacheMissRecorder = Some recordMiss
    }

    let report = CompilerLibrary.compile request
    match report.Result with
    | Ok _ -> Ok missCount
    | Error err -> Error err

let testNonInlinedCalleeBodyDoesNotInvalidateCaller () : TestResult =
    let guidFormat = "N"
    let compilerKey = $"test-cache-{Guid.NewGuid().ToString(guidFormat)}"
    let cacheSettings : CacheSettings = { Enabled = true; CompilerKey = compilerKey }
    CompilerLibrary.buildStdlibWithCache cacheSettings None
    |> Result.bind (fun stdlib ->
        let options = { CompilerLibrary.defaultOptions with DisableInlining = true }
        let sourceV1 =
            "def callee(x: Int64) : Int64 = x + 1\n\n" +
            "def caller(y: Int64) : Int64 = callee(y)\n\n" +
            "caller(1)\n"
        let sourceV2 =
            "def callee(x: Int64) : Int64 = x + 2\n\n" +
            "def caller(y: Int64) : Int64 = callee(y)\n\n" +
            "caller(1)\n"
        compileWithCacheMisses stdlib options sourceV1
        |> Result.bind (fun missCount1 ->
            compileWithCacheMisses stdlib options sourceV1
            |> Result.bind (fun missCount2 ->
                compileWithCacheMisses stdlib options sourceV2
                |> Result.bind (fun missCount3 ->
                    if missCount1 <> 2 then
                        Error $"Expected 2 cache misses on first compile, got {missCount1}"
                    else if missCount2 <> 0 then
                        Error $"Expected 0 cache misses on second compile, got {missCount2}"
                    else if missCount3 <> 1 then
                        Error $"Expected 1 cache miss after callee body change, got {missCount3}"
                    else
                        Ok ()
                ))))

let tests : (string * (unit -> Result<unit, string>)) list = [
    ("non-inlined callee body change keeps caller cached", testNonInlinedCalleeBodyDoesNotInvalidateCaller)
]
