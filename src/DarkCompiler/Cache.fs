// Cache.fs - SQLite-backed compiler cache for function artifacts
//
// Stores only post-register-allocation LIR functions.
// Keys include compiler hash, cache version, and option hashes so cache
// invalidation is explicit and easy to reason about.

module Cache

open System
open System.IO
open System.Diagnostics
open System.Security.Cryptography
open Microsoft.Data.Sqlite
open MessagePack
open MessagePack.FSharp
open MessagePack.Resolvers

let cacheVersion = "v4"

// MessagePack configuration with contractless resolver for F# types
let private resolver =
    CompositeResolver.Create(
        FSharpResolver.Instance,
        ContractlessStandardResolver.Instance)

let private serializerOptions =
    MessagePackSerializerOptions.Standard
        .WithResolver(resolver)
        .WithSecurity(MessagePackSecurity.UntrustedData)

/// Serialize data to MessagePack bytes
let serialize<'T> (data: 'T) : Result<byte array, string> =
    try Ok (MessagePackSerializer.Serialize(data, serializerOptions))
    with ex -> Error $"MessagePack serialize error: {ex}"

/// Deserialize MessagePack bytes to data
let deserialize<'T> (bytes: byte array) : Result<'T, string> =
    try Ok (MessagePackSerializer.Deserialize<'T>(bytes, serializerOptions))
    with ex -> Error $"MessagePack deserialize error: {ex}"

/// Compute SHA256 hash of bytes
let hashBytes (bytes: byte array) : string =
    use sha256 = SHA256.Create()
    let hash = sha256.ComputeHash(bytes)
    BitConverter.ToString(hash).Replace("-", "").ToLowerInvariant()

/// Compute SHA256 hash of a string (UTF-8)
let hashString (s: string) : string =
    let bytes = System.Text.Encoding.UTF8.GetBytes(s)
    hashBytes bytes

/// Hash any serializable data to a string key
let hashData<'T> (data: 'T) : Result<string, string> =
    serialize data |> Result.map hashBytes

type HashTiming = {
    SerializeMs: float
    HashMs: float
}

type CacheReadTiming = {
    ReadMs: float
    DeserializeMs: float
    QueryCount: int
    RowCount: int
}

type CacheWriteTiming = {
    SerializeMs: float
    InsertMs: float
    CommitMs: float
    InsertCount: int
    CommitCount: int
}

/// Hash any serializable data to a string key with timing details
let hashDataWithTiming<'T> (data: 'T) : Result<string * HashTiming, string> =
    let sw = Stopwatch.StartNew()
    serialize data
    |> Result.map (fun bytes ->
        let serializeMs = sw.Elapsed.TotalMilliseconds
        let hash = hashBytes bytes
        let hashMs = sw.Elapsed.TotalMilliseconds - serializeMs
        (hash, { SerializeMs = serializeMs; HashMs = hashMs }))

/// Cache location: ~/.cache/dark-compiler/cache.db
let getCacheDir () : string =
    Path.Combine(
        Environment.GetFolderPath(Environment.SpecialFolder.UserProfile),
        ".cache",
        "dark-compiler")

/// Cache database path
let getCacheDbPath () : string =
    Path.Combine(getCacheDir (), "cache.db")

/// Cache database path for tests
let getCacheDbPathForTests () : string =
    Path.Combine(getCacheDir (), "cache-tests.db")

let private getCompilerPath () : Result<string, string> =
    try
        let assembly = System.Reflection.Assembly.GetExecutingAssembly()
        let location = assembly.Location
        if String.IsNullOrWhiteSpace location then
            Error "Compiler path was empty"
        else
            Ok location
    with ex ->
        Error $"Compiler path error: {ex.Message}"

/// Key for cached function artifacts
type FunctionCacheKey = {
    CacheVersion: string
    CompilerKey: string
    OptionsHash: string
    FunctionName: string
    FunctionHash: string
}

/// In-memory snapshot of cache entries for a compiler key
type CacheSnapshot = {
    CompilerKey: string
    Entries: Map<FunctionCacheKey, byte array>
}

/// Ensure the cache directory exists
let private ensureCacheDir () : Result<unit, string> =
    try
        let dir = getCacheDir ()
        Directory.CreateDirectory(dir) |> ignore
        Ok ()
    with ex ->
        Error $"Cache directory error: {ex.Message}"

let private ensureCacheDirForPath (dbPath: string) : Result<unit, string> =
    try
        let dir = Path.GetDirectoryName(dbPath)
        if String.IsNullOrWhiteSpace dir then
            Ok ()
        else
            Directory.CreateDirectory(dir) |> ignore
            Ok ()
    with ex ->
        Error $"Cache directory error: {ex.Message}"

/// Create database schema
let private createSchema (conn: SqliteConnection) : Result<unit, string> =
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

            -- binary_cache removed; only function_cache is used

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

let private expectedFunctionCacheColumns : Set<string> =
    Set.ofList [
        "cache_version"
        "compiler_key"
        "options_hash"
        "function_name"
        "function_hash"
        "data"
        "created_at"
    ]

let private expectedCompilerMetaColumns : Set<string> =
    Set.ofList [
        "compiler_path"
        "compiler_key"
        "updated_at"
    ]

let private getFunctionCacheColumns (conn: SqliteConnection) : Result<Set<string>, string> =
    try
        use cmd = conn.CreateCommand()
        cmd.CommandText <- "PRAGMA table_info(function_cache)"
        use reader = cmd.ExecuteReader()
        let rec loop (acc: Set<string>) =
            if reader.Read() then
                let name = reader.GetString(1)
                loop (Set.add name acc)
            else
                acc
        Ok (loop Set.empty)
    with ex ->
        Error $"Cache schema error: {ex.Message}"

let private getCompilerMetaColumns (conn: SqliteConnection) : Result<Set<string>, string> =
    try
        use cmd = conn.CreateCommand()
        cmd.CommandText <- "PRAGMA table_info(compiler_meta)"
        use reader = cmd.ExecuteReader()
        let rec loop (acc: Set<string>) =
            if reader.Read() then
                let name = reader.GetString(1)
                loop (Set.add name acc)
            else
                acc
        Ok (loop Set.empty)
    with ex ->
        Error $"Cache schema error: {ex.Message}"

let private ensureCompilerMetaSchema (conn: SqliteConnection) : Result<unit, string> =
    getCompilerMetaColumns conn
    |> Result.bind (fun columns ->
        if Set.isEmpty columns then
            createSchema conn
        elif columns = expectedCompilerMetaColumns then
            Ok ()
        else
            try
                use cmd = conn.CreateCommand()
                cmd.CommandText <- "DROP TABLE IF EXISTS compiler_meta"
                cmd.ExecuteNonQuery() |> ignore
                createSchema conn
            with ex ->
                Error $"Cache schema error: {ex.Message}")

let private ensureSchema (conn: SqliteConnection) : Result<unit, string> =
    getFunctionCacheColumns conn
    |> Result.bind (fun columns ->
        if Set.isEmpty columns then
            createSchema conn
        elif columns = expectedFunctionCacheColumns then
            Ok ()
        else
            try
                use cmd = conn.CreateCommand()
                cmd.CommandText <- "DROP TABLE IF EXISTS function_cache"
                cmd.ExecuteNonQuery() |> ignore
                createSchema conn
            with ex ->
                Error $"Cache schema error: {ex.Message}")
    |> Result.bind (fun () -> ensureCompilerMetaSchema conn)

/// Run a cache operation with a fresh connection
let private withConnectionForPath
    (dbPath: string)
    (f: SqliteConnection -> Result<'T, string>)
    : Result<'T, string> =
    ensureCacheDirForPath dbPath
    |> Result.bind (fun () ->
        try
            use conn = new SqliteConnection($"Data Source={dbPath}")
            conn.Open()
            let configureConnection (conn: SqliteConnection) : Result<unit, string> =
                try
                    use cmd = conn.CreateCommand()
                    cmd.CommandText <- "PRAGMA journal_mode=WAL;"
                    cmd.ExecuteNonQuery() |> ignore
                    cmd.CommandText <- "PRAGMA synchronous=NORMAL;"
                    cmd.ExecuteNonQuery() |> ignore
                    Ok ()
                with ex ->
                    Error $"Cache pragma error: {ex.Message}"
            configureConnection conn
            |> Result.bind (fun () -> ensureSchema conn)
            |> Result.bind (fun () -> f conn)
        with ex ->
            Error $"Cache connection error: {ex.Message}")

/// Run a cache operation with a fresh connection
let private withConnection (f: SqliteConnection -> Result<'T, string>) : Result<'T, string> =
    withConnectionForPath (getCacheDbPath ()) f

/// Inspect cache connection pragmas for diagnostics/testing
let getConnectionPragmas () : Result<string * int, string> =
    let read (conn: SqliteConnection) : Result<string * int, string> =
        try
            use cmd = conn.CreateCommand()
            cmd.CommandText <- "PRAGMA journal_mode;"
            let journalResult = cmd.ExecuteScalar()
            let journalMode =
                match journalResult with
                | :? string as mode -> Ok mode
                | _ -> Error "Cache pragma query error: unexpected journal_mode type"
            journalMode
            |> Result.bind (fun journal ->
                cmd.CommandText <- "PRAGMA synchronous;"
                let syncResult = cmd.ExecuteScalar()
                let syncValue =
                    match syncResult with
                    | :? int as value -> Ok value
                    | :? int64 as value -> Ok (int value)
                    | :? string as value ->
                        match System.Int32.TryParse(value) with
                        | true, parsed -> Ok parsed
                        | false, _ -> Error "Cache pragma query error: synchronous was not numeric"
                    | _ -> Error "Cache pragma query error: unexpected synchronous type"
                syncValue
                |> Result.map (fun sync -> (journal, sync)))
        with ex ->
            Error $"Cache pragma query error: {ex.Message}"
    withConnection read

/// Inspect cache connection pragmas for diagnostics/testing with an explicit path
let getConnectionPragmasWithDbPath (dbPath: string) : Result<string * int, string> =
    let read (conn: SqliteConnection) : Result<string * int, string> =
        try
            use cmd = conn.CreateCommand()
            cmd.CommandText <- "PRAGMA journal_mode;"
            let journalResult = cmd.ExecuteScalar()
            let journalMode =
                match journalResult with
                | :? string as mode -> Ok mode
                | _ -> Error "Cache pragma query error: unexpected journal_mode type"
            journalMode
            |> Result.bind (fun journal ->
                cmd.CommandText <- "PRAGMA synchronous;"
                let syncResult = cmd.ExecuteScalar()
                let syncValue =
                    match syncResult with
                    | :? int as value -> Ok value
                    | :? int64 as value -> Ok (int value)
                    | :? string as value ->
                        match System.Int32.TryParse(value) with
                        | true, parsed -> Ok parsed
                        | false, _ -> Error "Cache pragma query error: synchronous was not numeric"
                    | _ -> Error "Cache pragma query error: unexpected synchronous type"
                syncValue
                |> Result.map (fun sync -> (journal, sync)))
        with ex ->
            Error $"Cache pragma query error: {ex.Message}"
    withConnectionForPath dbPath read

let private computeCompilerKey (compilerPath: string) : Result<string, string> =
    try
        use stream = File.OpenRead(compilerPath)
        use sha256 = SHA256.Create()
        let hash = sha256.ComputeHash(stream)
        Ok (BitConverter.ToString(hash).Replace("-", "").ToLowerInvariant())
    with ex ->
        Error $"Compiler key error: {ex.Message}"

let private getCompilerMetaKey
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

let private deleteFunctionCacheByCompilerKey
    (conn: SqliteConnection)
    (compilerKey: string)
    : Result<unit, string> =
    try
        use cmd = conn.CreateCommand()
        cmd.CommandText <- """
            DELETE FROM function_cache
            WHERE compiler_key = $key
        """
        cmd.Parameters.AddWithValue("$key", compilerKey) |> ignore
        cmd.ExecuteNonQuery() |> ignore
        Ok ()
    with ex ->
        Error $"Cache eviction error: {ex.Message}"

let private upsertCompilerMeta
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
        Error $"Cache meta update error: {ex.Message}"

let private refreshCompilerMeta
    (conn: SqliteConnection)
    (compilerPath: string)
    (compilerKey: string)
    : Result<unit, string> =
    getCompilerMetaKey conn compilerPath
    |> Result.bind (fun existingKeyOpt ->
        match existingKeyOpt with
        | Some existingKey when existingKey <> compilerKey ->
            deleteFunctionCacheByCompilerKey conn existingKey
        | _ -> Ok ())
    |> Result.bind (fun () -> upsertCompilerMeta conn compilerPath compilerKey)

/// Compute the compiler key (SHA256 of compiler binary)
let getCompilerKey () : Result<string, string> =
    getCompilerPath ()
    |> Result.bind (fun compilerPath ->
        computeCompilerKey compilerPath
        |> Result.bind (fun compilerKey ->
            withConnection (fun conn ->
                refreshCompilerMeta conn compilerPath compilerKey
                |> Result.map (fun () -> compilerKey))))

/// Compute the compiler key using a specific cache database path
let getCompilerKeyWithDbPath (dbPath: string) : Result<string, string> =
    getCompilerPath ()
    |> Result.bind (fun compilerPath ->
        computeCompilerKey compilerPath
        |> Result.bind (fun compilerKey ->
            withConnectionForPath dbPath (fun conn ->
                refreshCompilerMeta conn compilerPath compilerKey
                |> Result.map (fun () -> compilerKey))))

/// Get a batch of cached function entries using a single connection
let private getFunctionsInternal<'T>
    (keys: FunctionCacheKey list)
    (recordTiming: bool)
    : Result<Map<string, 'T> * CacheReadTiming, string> =
    if List.isEmpty keys then
        Ok (Map.empty, { ReadMs = 0.0; DeserializeMs = 0.0; QueryCount = 0; RowCount = 0 })
    else
        let desiredByName : Map<string, FunctionCacheKey> =
            keys |> List.fold (fun acc key -> Map.add key.FunctionName key acc) Map.empty

        let groups : (string * string * string) list =
            desiredByName
            |> Map.toList
            |> List.map (fun (_, key) -> (key.CacheVersion, key.CompilerKey, key.OptionsHash))
            |> Set.ofList
            |> Set.toList

        let read (conn: SqliteConnection) : Result<Map<string, 'T> * CacheReadTiming, string> =
            try
                let totalSwOpt =
                    if recordTiming then
                        Some (Stopwatch.StartNew())
                    else
                        None

                use cmd = conn.CreateCommand()
                cmd.CommandText <- """
                    SELECT function_name, function_hash, data
                    FROM function_cache
                    WHERE cache_version = $cacheVersion
                      AND compiler_key = $compilerKey
                      AND options_hash = $optionsHash
                """
                let pCacheVersion = cmd.Parameters.AddWithValue("$cacheVersion", "")
                let pCompilerKey = cmd.Parameters.AddWithValue("$compilerKey", "")
                let pOptionsHash = cmd.Parameters.AddWithValue("$optionsHash", "")

                let rec readRows
                    (cacheVersion: string)
                    (compilerKey: string)
                    (optionsHash: string)
                    (reader: SqliteDataReader)
                    (acc: Map<string, 'T>)
                    (deserializeMs: float)
                    (rowCount: int)
                    : Result<Map<string, 'T> * float * int, string> =
                    if reader.Read() then
                        let functionName = reader.GetString(0)
                        let functionHash = reader.GetString(1)
                        let nextRowCount = rowCount + 1
                        match Map.tryFind functionName desiredByName with
                        | Some key when
                            key.CacheVersion = cacheVersion
                            && key.CompilerKey = compilerKey
                            && key.OptionsHash = optionsHash
                            && key.FunctionHash = functionHash ->
                            let obj = reader.GetValue(2)
                            match obj with
                            | :? (byte array) as bytes ->
                                if recordTiming then
                                    let sw = Stopwatch.StartNew()
                                    deserialize<'T> bytes
                                    |> Result.bind (fun value ->
                                        let elapsed = sw.Elapsed.TotalMilliseconds
                                        readRows cacheVersion compilerKey optionsHash reader (Map.add functionName value acc) (deserializeMs + elapsed) nextRowCount)
                                else
                                    deserialize<'T> bytes
                                    |> Result.bind (fun value ->
                                        readRows cacheVersion compilerKey optionsHash reader (Map.add functionName value acc) deserializeMs nextRowCount)
                            | _ ->
                                Error "Cache read error: unexpected function data type"
                        | _ ->
                            readRows cacheVersion compilerKey optionsHash reader acc deserializeMs nextRowCount
                    else
                        Ok (acc, deserializeMs, rowCount)

                let rec readGroups
                    (remaining: (string * string * string) list)
                    (acc: Map<string, 'T>)
                    (deserializeMs: float)
                    (rowCount: int)
                    : Result<Map<string, 'T> * float * int, string> =
                    match remaining with
                    | [] -> Ok (acc, deserializeMs, rowCount)
                    | (cacheVersion, compilerKey, optionsHash) :: rest ->
                        pCacheVersion.Value <- cacheVersion
                        pCompilerKey.Value <- compilerKey
                        pOptionsHash.Value <- optionsHash
                        use reader = cmd.ExecuteReader()
                        readRows cacheVersion compilerKey optionsHash reader acc deserializeMs rowCount
                        |> Result.bind (fun (updated, updatedDeserialize, updatedRows) ->
                            readGroups rest updated updatedDeserialize updatedRows)

                readGroups groups Map.empty 0.0 0
                |> Result.map (fun (cachedMap, deserializeMs, rowCount) ->
                    let readMs =
                        match totalSwOpt with
                        | Some sw -> sw.Elapsed.TotalMilliseconds
                        | None -> 0.0
                    let queryCount = if recordTiming then List.length groups else 0
                    let rows = if recordTiming then rowCount else 0
                    (cachedMap, {
                        ReadMs = readMs
                        DeserializeMs = if recordTiming then deserializeMs else 0.0
                        QueryCount = queryCount
                        RowCount = rows
                    }))
            with ex ->
                Error $"Cache read error: {ex.Message}"
        withConnection read

/// Load all cached function entries for a compiler key using a single connection
let private loadAllForCompilerKeyInternalWith
    (compilerKey: string)
    (recordTiming: bool)
    (withConn: (SqliteConnection -> Result<CacheSnapshot * CacheReadTiming, string>) -> Result<CacheSnapshot * CacheReadTiming, string>)
    : Result<CacheSnapshot * CacheReadTiming, string> =
    if String.IsNullOrWhiteSpace compilerKey then
        Error "Cache read error: compiler key was empty"
    else
        let read (conn: SqliteConnection) : Result<CacheSnapshot * CacheReadTiming, string> =
            try
                let totalSwOpt =
                    if recordTiming then
                        Some (Stopwatch.StartNew())
                    else
                        None

                use cmd = conn.CreateCommand()
                cmd.CommandText <- """
                    SELECT options_hash, function_name, function_hash, data
                    FROM function_cache
                    WHERE cache_version = $cacheVersion
                      AND compiler_key = $compilerKey
                """
                cmd.Parameters.AddWithValue("$cacheVersion", cacheVersion) |> ignore
                cmd.Parameters.AddWithValue("$compilerKey", compilerKey) |> ignore
                use reader = cmd.ExecuteReader()

                let rec readRows
                    (acc: Map<FunctionCacheKey, byte array>)
                    (rowCount: int)
                    : Result<Map<FunctionCacheKey, byte array> * int, string> =
                    if reader.Read() then
                        let optionsHash = reader.GetString(0)
                        let functionName = reader.GetString(1)
                        let functionHash = reader.GetString(2)
                        let obj = reader.GetValue(3)
                        match obj with
                        | :? (byte array) as bytes ->
                            let key = {
                                CacheVersion = cacheVersion
                                CompilerKey = compilerKey
                                OptionsHash = optionsHash
                                FunctionName = functionName
                                FunctionHash = functionHash
                            }
                            readRows (Map.add key bytes acc) (rowCount + 1)
                        | _ ->
                            Error "Cache read error: unexpected function data type"
                    else
                        Ok (acc, rowCount)

                readRows Map.empty 0
                |> Result.map (fun (entries, rowCount) ->
                    let readMs =
                        match totalSwOpt with
                        | Some sw -> sw.Elapsed.TotalMilliseconds
                        | None -> 0.0
                    let timing = {
                        ReadMs = if recordTiming then readMs else 0.0
                        DeserializeMs = 0.0
                        QueryCount = if recordTiming then 1 else 0
                        RowCount = if recordTiming then rowCount else 0
                    }
                    ({ CompilerKey = compilerKey; Entries = entries }, timing))
            with ex ->
                Error $"Cache read error: {ex.Message}"
        withConn read

let private loadAllForCompilerKeyInternal
    (compilerKey: string)
    (recordTiming: bool)
    : Result<CacheSnapshot * CacheReadTiming, string> =
    loadAllForCompilerKeyInternalWith compilerKey recordTiming withConnection

/// Load all cached function entries for a compiler key with timing details
let loadAllForCompilerKeyWithTiming
    (compilerKey: string)
    : Result<CacheSnapshot * CacheReadTiming, string> =
    loadAllForCompilerKeyInternal compilerKey true

/// Load all cached function entries for a compiler key
let loadAllForCompilerKey (compilerKey: string) : Result<CacheSnapshot, string> =
    loadAllForCompilerKeyInternal compilerKey false
    |> Result.map fst

/// Load all cached function entries for a compiler key with timing details and explicit path
let loadAllForCompilerKeyWithTimingForDbPath
    (compilerKey: string)
    (dbPath: string)
    : Result<CacheSnapshot * CacheReadTiming, string> =
    loadAllForCompilerKeyInternalWith compilerKey true (withConnectionForPath dbPath)

/// Load all cached function entries for a compiler key with explicit path
let loadAllForCompilerKeyForDbPath (compilerKey: string) (dbPath: string) : Result<CacheSnapshot, string> =
    loadAllForCompilerKeyInternalWith compilerKey false (withConnectionForPath dbPath)
    |> Result.map fst

/// Get cached function entries from a preloaded snapshot
let private getFunctionsFromSnapshotInternal<'T>
    (snapshot: CacheSnapshot)
    (keys: FunctionCacheKey list)
    (recordTiming: bool)
    : Result<Map<string, 'T> * CacheReadTiming, string> =
    if List.isEmpty keys then
        Ok (Map.empty, { ReadMs = 0.0; DeserializeMs = 0.0; QueryCount = 0; RowCount = 0 })
    else
        match keys |> List.tryFind (fun key -> key.CompilerKey <> snapshot.CompilerKey) with
        | Some key ->
            Crash.crash $"Cache snapshot compiler key mismatch: expected {snapshot.CompilerKey}, got {key.CompilerKey}"
        | None ->
            let desiredByName : Map<string, FunctionCacheKey> =
                keys |> List.fold (fun acc key -> Map.add key.FunctionName key acc) Map.empty

            let totalSwOpt =
                if recordTiming then
                    Some (Stopwatch.StartNew())
                else
                    None

            let folder
                (accResult: Result<Map<string, 'T> * float, string>)
                (functionName: string, key: FunctionCacheKey)
                : Result<Map<string, 'T> * float, string> =
                accResult
                |> Result.bind (fun (acc, deserializeMs) ->
                    match Map.tryFind key snapshot.Entries with
                    | None -> Ok (acc, deserializeMs)
                    | Some bytes ->
                        if recordTiming then
                            let sw = Stopwatch.StartNew()
                            deserialize<'T> bytes
                            |> Result.map (fun value ->
                                let elapsed = sw.Elapsed.TotalMilliseconds
                                (Map.add functionName value acc, deserializeMs + elapsed))
                        else
                            deserialize<'T> bytes
                            |> Result.map (fun value -> (Map.add functionName value acc, deserializeMs)))

            let initial = Ok (Map.empty, 0.0)
            desiredByName
            |> Map.toList
            |> List.fold folder initial
            |> Result.map (fun (cachedMap, deserializeMs) ->
                let readMs =
                    match totalSwOpt with
                    | Some sw -> sw.Elapsed.TotalMilliseconds
                    | None -> 0.0
                let timing = {
                    ReadMs = if recordTiming then readMs else 0.0
                    DeserializeMs = if recordTiming then deserializeMs else 0.0
                    QueryCount = 0
                    RowCount = 0
                }
                (cachedMap, timing))

/// Get cached function entries from a preloaded snapshot with timing details
let getFunctionsFromSnapshotWithTiming<'T>
    (snapshot: CacheSnapshot)
    (keys: FunctionCacheKey list)
    : Result<Map<string, 'T> * CacheReadTiming, string> =
    getFunctionsFromSnapshotInternal snapshot keys true

/// Get cached function entries from a preloaded snapshot
let getFunctionsFromSnapshot<'T>
    (snapshot: CacheSnapshot)
    (keys: FunctionCacheKey list)
    : Result<Map<string, 'T>, string> =
    getFunctionsFromSnapshotInternal snapshot keys false
    |> Result.map fst

/// Get a batch of cached function entries using a single connection with timing details
let getFunctionsWithTiming<'T>
    (keys: FunctionCacheKey list)
    : Result<Map<string, 'T> * CacheReadTiming, string> =
    getFunctionsInternal keys true

/// Get a batch of cached function entries using a single connection
let getFunctions<'T> (keys: FunctionCacheKey list) : Result<Map<string, 'T>, string> =
    getFunctionsInternal keys false
    |> Result.map fst

/// Store a batch of cached function entries using a single connection
let private setFunctionsInternalWith<'T>
    (entries: (FunctionCacheKey * 'T) list)
    (recordTiming: bool)
    (withConn: (SqliteConnection -> Result<CacheWriteTiming, string>) -> Result<CacheWriteTiming, string>)
    : Result<CacheWriteTiming, string> =
    if List.isEmpty entries then
        Ok { SerializeMs = 0.0; InsertMs = 0.0; CommitMs = 0.0; InsertCount = 0; CommitCount = 0 }
    else
        let write (conn: SqliteConnection) : Result<CacheWriteTiming, string> =
            try
                use transaction = conn.BeginTransaction()
                use cmd = conn.CreateCommand()
                cmd.Transaction <- transaction
                cmd.CommandText <- """
                    INSERT OR REPLACE INTO function_cache
                    (cache_version, compiler_key, options_hash, function_name, function_hash, data, created_at)
                    VALUES ($cacheVersion, $compilerKey, $optionsHash, $functionName, $functionHash, $data, $createdAt)
                """
                let pCacheVersion = cmd.Parameters.AddWithValue("$cacheVersion", "")
                let pCompilerKey = cmd.Parameters.AddWithValue("$compilerKey", "")
                let pOptionsHash = cmd.Parameters.AddWithValue("$optionsHash", "")
                let pFunctionName = cmd.Parameters.AddWithValue("$functionName", "")
                let pFunctionHash = cmd.Parameters.AddWithValue("$functionHash", "")
                let pData = cmd.Parameters.AddWithValue("$data", Array.empty<byte>)
                let pCreatedAt = cmd.Parameters.AddWithValue("$createdAt", 0L)

                let createdAt = DateTimeOffset.UtcNow.ToUnixTimeSeconds()

                let updateParams (key: FunctionCacheKey) (bytes: byte array) : unit =
                    pCacheVersion.Value <- key.CacheVersion
                    pCompilerKey.Value <- key.CompilerKey
                    pOptionsHash.Value <- key.OptionsHash
                    pFunctionName.Value <- key.FunctionName
                    pFunctionHash.Value <- key.FunctionHash
                    pData.Value <- bytes
                    pCreatedAt.Value <- createdAt

                let folder (accResult: Result<CacheWriteTiming, string>) (key, value) =
                    accResult
                    |> Result.bind (fun acc ->
                        let serializeResult =
                            if recordTiming then
                                let sw = Stopwatch.StartNew()
                                serialize value
                                |> Result.map (fun bytes -> (bytes, sw.Elapsed.TotalMilliseconds))
                            else
                                serialize value
                                |> Result.map (fun bytes -> (bytes, 0.0))
                        serializeResult
                        |> Result.bind (fun (bytes, serializeMs) ->
                            let insertMs =
                                if recordTiming then
                                    let sw = Stopwatch.StartNew()
                                    updateParams key bytes
                                    cmd.ExecuteNonQuery() |> ignore
                                    sw.Elapsed.TotalMilliseconds
                                else
                                    updateParams key bytes
                                    cmd.ExecuteNonQuery() |> ignore
                                    0.0
                            Ok {
                                SerializeMs = acc.SerializeMs + serializeMs
                                InsertMs = acc.InsertMs + insertMs
                                CommitMs = acc.CommitMs
                                InsertCount = acc.InsertCount + 1
                                CommitCount = acc.CommitCount
                            }))

                let initial = { SerializeMs = 0.0; InsertMs = 0.0; CommitMs = 0.0; InsertCount = 0; CommitCount = 0 }
                match List.fold folder (Ok initial) entries with
                | Ok timing ->
                    let commitMs =
                        if recordTiming then
                            let sw = Stopwatch.StartNew()
                            transaction.Commit()
                            sw.Elapsed.TotalMilliseconds
                        else
                            transaction.Commit()
                            0.0
                    Ok { timing with CommitMs = commitMs; CommitCount = 1 }
                | Error err -> Error err
            with ex ->
                Error $"Cache write error: {ex.Message}"
        withConn write

let private setFunctionsInternal<'T>
    (entries: (FunctionCacheKey * 'T) list)
    (recordTiming: bool)
    : Result<CacheWriteTiming, string> =
    setFunctionsInternalWith entries recordTiming withConnection

/// Store a batch of cached function entries using a single connection with timing details
let setFunctionsWithTiming<'T>
    (entries: (FunctionCacheKey * 'T) list)
    : Result<CacheWriteTiming, string> =
    setFunctionsInternal entries true

/// Store a batch of cached function entries using a single connection
let setFunctions<'T> (entries: (FunctionCacheKey * 'T) list) : Result<unit, string> =
    setFunctionsInternal entries false
    |> Result.map (fun _ -> ())

/// Store a batch of cached function entries using a specific database path
let setFunctionsWithDbPath<'T>
    (entries: (FunctionCacheKey * 'T) list)
    (dbPath: string)
    : Result<unit, string> =
    setFunctionsInternalWith entries false (withConnectionForPath dbPath)
    |> Result.map (fun _ -> ())

/// Get a cached function entry
let getFunction<'T> (key: FunctionCacheKey) : Result<'T option, string> =
    getFunctions<'T> [key]
    |> Result.map (fun cached -> Map.tryFind key.FunctionName cached)

/// Store a cached function entry
let setFunction<'T> (key: FunctionCacheKey) (value: 'T) : Result<unit, string> =
    setFunctions<'T> [ (key, value) ]
