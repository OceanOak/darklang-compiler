// CacheTests.fs - Unit tests for compiler cache behavior
//
// Ensures cache special-cases (like skipping _start) remain enforced.

module CacheTests

open System
open System.IO
open Microsoft.Data.Sqlite

type TestResult = Result<unit, string>

let private ensureCacheSchema (conn: SqliteConnection) : Result<unit, string> =
    try
        use cmd = conn.CreateCommand()
        cmd.CommandText <- """
            CREATE TABLE IF NOT EXISTS function_cache (
                cache_version TEXT NOT NULL,
                compiler_key TEXT NOT NULL,
                options_hash TEXT NOT NULL,
                function_name TEXT NOT NULL,
                function_hash TEXT NOT NULL,
                data BLOB NOT NULL,
                created_at INTEGER NOT NULL,
                PRIMARY KEY (cache_version, compiler_key, options_hash, function_name, function_hash)
            );
        """
        cmd.ExecuteNonQuery() |> ignore
        Ok ()
    with ex ->
        Error $"Cache schema error: {ex.Message}"

let private withCacheConnection (f: SqliteConnection -> Result<'T, string>) : Result<'T, string> =
    try
        let dbPath = Cache.getCacheDbPath ()
        let dir = Path.GetDirectoryName(dbPath)
        if not (String.IsNullOrEmpty dir) then
            Directory.CreateDirectory(dir) |> ignore
        use conn = new SqliteConnection($"Data Source={dbPath}")
        conn.Open()
        ensureCacheSchema conn
        |> Result.bind (fun () -> f conn)
    with ex ->
        Error $"Cache connection error: {ex.Message}"

let private deleteStartEntries () : Result<unit, string> =
    withCacheConnection (fun conn ->
        try
            use cmd = conn.CreateCommand()
            cmd.CommandText <- "DELETE FROM function_cache WHERE function_name = '_start'"
            cmd.ExecuteNonQuery() |> ignore
            Ok ()
        with ex ->
            Error $"Cache delete error: {ex.Message}")

let private countStartEntries () : Result<int64, string> =
    withCacheConnection (fun conn ->
        try
            use cmd = conn.CreateCommand()
            cmd.CommandText <- "SELECT COUNT(*) FROM function_cache WHERE function_name = '_start'"
            let result = cmd.ExecuteScalar()
            match result with
            | :? int64 as count -> Ok count
            | :? int as count -> Ok (int64 count)
            | null -> Ok 0L
            | other -> Error $"Cache count error: unexpected result type {other.GetType().FullName}"
        with ex ->
            Error $"Cache count error: {ex.Message}")

let private compileSimpleProgram (stdlib: CompilerLibrary.StdlibResult) : Result<unit, string> =
    let request : CompilerLibrary.CompileRequest = {
        Context = CompilerLibrary.StdlibOnly stdlib
        Mode = CompilerLibrary.TestExpression
        Source = "1"
        SourceFile = "cache-tests"
        AllowInternal = false
        Verbosity = 0
        Options = CompilerLibrary.defaultOptions
    }
    let report = CompilerLibrary.compile request
    match report.Result with
    | Ok _ -> Ok ()
    | Error err -> Error $"Compilation failed: {err}"

let testStartFunctionIsNotCached () : TestResult =
    deleteStartEntries ()
    |> Result.bind (fun () ->
        CompilerLibrary.buildStdlib ()
        |> Result.bind (fun stdlib ->
            compileSimpleProgram stdlib
            |> Result.bind (fun () ->
                countStartEntries ()
                |> Result.bind (fun count ->
                    if count = 0L then Ok ()
                    else Error $"Expected no _start cache entries, found {count}"))))

let tests = [
    ("_start is not cached", testStartFunctionIsNotCached)
]
