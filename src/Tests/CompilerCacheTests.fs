// CompilerCacheTests.fs - Unit tests for compiler cache behavior around _start caching
//
// Ensures the synthesized _start function is persisted in the cache when caching is enabled,
// and that compiler-location-aware eviction clears stale cache entries.

module CompilerCacheTests

open System
open System.IO
open Microsoft.Data.Sqlite
open CompilerLibrary

type TestResult = Result<unit, string>

let private countStartEntries (compilerKey: string) : Result<int, string> =
    let dbPath = Cache.getCacheDbPath ()
    if not (File.Exists dbPath) then
        Ok 0
    else
        try
            use conn = new SqliteConnection($"Data Source={dbPath}")
            conn.Open()
            use cmd = conn.CreateCommand()
            cmd.CommandText <- """
                SELECT COUNT(*)
                FROM function_cache
                WHERE cache_version = $cacheVersion
                  AND compiler_key = $compilerKey
                  AND function_name = $functionName
            """
            cmd.Parameters.AddWithValue("$cacheVersion", Cache.cacheVersion) |> ignore
            cmd.Parameters.AddWithValue("$compilerKey", compilerKey) |> ignore
            cmd.Parameters.AddWithValue("$functionName", "_start") |> ignore
            let result = cmd.ExecuteScalar()
            match result with
            | :? int64 as count -> Ok (int count)
            | :? int as count -> Ok count
            | _ -> Error "Cache query error: unexpected result type"
        with ex ->
            Error $"Cache query error: {ex.Message}"

let private getCompilerPath () : Result<string, string> =
    try
        let assembly = typeof<CompilerLibrary.CompilerOptions>.Assembly
        let location = assembly.Location
        if String.IsNullOrWhiteSpace location then
            Error "Compiler path was empty"
        else
            Ok location
    with ex ->
        Error $"Compiler path error: {ex.Message}"

let private ensureTestSchema (conn: SqliteConnection) : Result<unit, string> =
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

            CREATE TABLE IF NOT EXISTS compiler_meta (
                compiler_path TEXT NOT NULL,
                compiler_key TEXT NOT NULL,
                updated_at INTEGER NOT NULL,
                PRIMARY KEY (compiler_path)
            );
        """
        cmd.ExecuteNonQuery() |> ignore
        Ok ()
    with ex ->
        Error $"Cache schema error: {ex.Message}"

let private withCacheConnection
    (f: SqliteConnection -> Result<'T, string>)
    : Result<'T, string> =
    try
        let dbPath = Cache.getCacheDbPath ()
        let dir = Path.GetDirectoryName(dbPath)
        if not (String.IsNullOrWhiteSpace dir) then
            Directory.CreateDirectory(dir) |> ignore
        use conn = new SqliteConnection($"Data Source={dbPath}")
        conn.Open()
        ensureTestSchema conn
        |> Result.bind (fun () -> f conn)
    with ex ->
        Error $"Cache connection error: {ex.Message}"

let private insertCompilerMeta
    (conn: SqliteConnection)
    (compilerPath: string)
    (compilerKey: string)
    : Result<unit, string> =
    try
        use cmd = conn.CreateCommand()
        cmd.CommandText <- """
            INSERT OR REPLACE INTO compiler_meta
            (compiler_path, compiler_key, updated_at)
            VALUES ($path, $key, $updatedAt)
        """
        cmd.Parameters.AddWithValue("$path", compilerPath) |> ignore
        cmd.Parameters.AddWithValue("$key", compilerKey) |> ignore
        cmd.Parameters.AddWithValue("$updatedAt", DateTimeOffset.UtcNow.ToUnixTimeSeconds()) |> ignore
        cmd.ExecuteNonQuery() |> ignore
        Ok ()
    with ex ->
        Error $"Cache meta insert error: {ex.Message}"

let private insertCacheEntry
    (conn: SqliteConnection)
    (compilerKey: string)
    : Result<unit, string> =
    try
        use cmd = conn.CreateCommand()
        cmd.CommandText <- """
            INSERT OR REPLACE INTO function_cache
            (cache_version, compiler_key, options_hash, function_name, function_hash, data, created_at)
            VALUES ($cacheVersion, $compilerKey, $optionsHash, $functionName, $functionHash, $data, $createdAt)
        """
        cmd.Parameters.AddWithValue("$cacheVersion", Cache.cacheVersion) |> ignore
        cmd.Parameters.AddWithValue("$compilerKey", compilerKey) |> ignore
        cmd.Parameters.AddWithValue("$optionsHash", "test-options") |> ignore
        cmd.Parameters.AddWithValue("$functionName", "test_fn") |> ignore
        cmd.Parameters.AddWithValue("$functionHash", "test-hash") |> ignore
        cmd.Parameters.AddWithValue("$data", [| 0uy |]) |> ignore
        cmd.Parameters.AddWithValue("$createdAt", DateTimeOffset.UtcNow.ToUnixTimeSeconds()) |> ignore
        cmd.ExecuteNonQuery() |> ignore
        Ok ()
    with ex ->
        Error $"Cache entry insert error: {ex.Message}"

let private countEntriesForKey
    (conn: SqliteConnection)
    (compilerKey: string)
    : Result<int, string> =
    try
        use cmd = conn.CreateCommand()
        cmd.CommandText <- """
            SELECT COUNT(*)
            FROM function_cache
            WHERE cache_version = $cacheVersion
              AND compiler_key = $compilerKey
        """
        cmd.Parameters.AddWithValue("$cacheVersion", Cache.cacheVersion) |> ignore
        cmd.Parameters.AddWithValue("$compilerKey", compilerKey) |> ignore
        let result = cmd.ExecuteScalar()
        match result with
        | :? int64 as count -> Ok (int count)
        | :? int as count -> Ok count
        | _ -> Error "Cache query error: unexpected result type"
    with ex ->
        Error $"Cache query error: {ex.Message}"

let private getMetaKey
    (conn: SqliteConnection)
    (compilerPath: string)
    : Result<string option, string> =
    try
        use cmd = conn.CreateCommand()
        cmd.CommandText <- """
            SELECT compiler_key
            FROM compiler_meta
            WHERE compiler_path = $path
        """
        cmd.Parameters.AddWithValue("$path", compilerPath) |> ignore
        let result = cmd.ExecuteScalar()
        if isNull result then
            Ok None
        else
            match result with
            | :? string as key -> Ok (Some key)
            | _ -> Error "Cache meta query error: unexpected result type"
    with ex ->
        Error $"Cache meta query error: {ex.Message}"

let private compileOnce (stdlib: StdlibResult) (source: string) : Result<unit, string> =
    let request : CompileRequest = {
        Context = StdlibOnly stdlib
        Mode = FullProgram
        Source = source
        SourceFile = "CompilerCacheTests.dark"
        AllowInternal = false
        Verbosity = 0
        Options = CompilerLibrary.defaultOptions
        PassTimingRecorder = None
        CacheMissRecorder = None
    }
    let report = CompilerLibrary.compile request
    match report.Result with
    | Ok _ -> Ok ()
    | Error err -> Error err

let testStartFunctionIsCached (stdlib: StdlibResult) : TestResult =
    if not stdlib.CacheSettings.Enabled then
        Ok ()
    else
        let compilerKey = $"test-cache-{Guid.NewGuid():N}"
        let cacheSettings = { stdlib.CacheSettings with CompilerKey = compilerKey; Enabled = true }
        let stdlibWithKey = { stdlib with CacheSettings = cacheSettings }
        let source = "1 + 2"
        countStartEntries compilerKey
        |> Result.bind (fun beforeCount ->
            if beforeCount <> 0 then
                Error $"Expected 0 cached _start entries before compile, found {beforeCount}"
            else
                compileOnce stdlibWithKey source
                |> Result.bind (fun () ->
                    countStartEntries compilerKey
                    |> Result.bind (fun afterCount ->
                        if afterCount >= 1 then Ok ()
                        else Error "Expected cached _start entry after compile, found none")))

let testCompilerLocationEvictsOldCache () : TestResult =
    let oldKey = $"old-cache-{Guid.NewGuid():N}"
    getCompilerPath ()
    |> Result.bind (fun compilerPath ->
        withCacheConnection (fun conn ->
            insertCompilerMeta conn compilerPath oldKey
            |> Result.bind (fun () ->
                insertCacheEntry conn oldKey
                |> Result.bind (fun () ->
                    countEntriesForKey conn oldKey
                    |> Result.bind (fun beforeCount ->
                        if beforeCount <> 1 then
                            Error $"Expected 1 cached entry for old key, found {beforeCount}"
                        else
                            Ok ()))))
        |> Result.bind (fun () ->
            // Compute current key (should evict old cache rows when path matches).
            match Cache.getCompilerKey () with
            | Error msg -> Error msg
            | Ok currentKey ->
                withCacheConnection (fun conn ->
                    countEntriesForKey conn oldKey
                    |> Result.bind (fun afterCount ->
                        if afterCount <> 0 then
                            Error $"Expected old cache entries to be evicted, found {afterCount}"
                        else
                            getMetaKey conn compilerPath
                            |> Result.bind (fun storedKeyOpt ->
                                match storedKeyOpt with
                                | Some storedKey when storedKey = currentKey -> Ok ()
                                | Some storedKey ->
                                    Error $"Expected compiler meta key to update, found {storedKey}"
                                | None ->
                                    Error "Expected compiler meta entry to exist after eviction")))))

let tests (stdlib: StdlibResult) : (string * (unit -> Result<unit, string>)) list = [
    ("cache stores _start", fun () -> testStartFunctionIsCached stdlib)
    ("compiler location evicts old cache", testCompilerLocationEvictsOldCache)
]
