// Cache.fs - SQLite-backed compiler cache for function artifacts
//
// Stores only post-register-allocation LIR functions.
// Keys include compiler hash, cache version, and option hashes so cache
// invalidation is explicit and easy to reason about.

module Cache

open System
open System.IO
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

/// Cache location: ~/.cache/dark-compiler/cache.db
let getCacheDir () : string =
    Path.Combine(
        Environment.GetFolderPath(Environment.SpecialFolder.UserProfile),
        ".cache",
        "dark-compiler")

/// Cache database path
let getCacheDbPath () : string =
    Path.Combine(getCacheDir (), "cache.db")

/// Compute the compiler key (SHA256 of compiler binary)
let getCompilerKey () : Result<string, string> =
    try
        let assembly = System.Reflection.Assembly.GetExecutingAssembly()
        let location = assembly.Location
        use stream = File.OpenRead(location)
        use sha256 = SHA256.Create()
        let hash = sha256.ComputeHash(stream)
        Ok (BitConverter.ToString(hash).Replace("-", "").ToLowerInvariant())
    with ex ->
        Error $"Compiler key error: {ex.Message}"

/// Key for cached function artifacts
type FunctionCacheKey = {
    CacheVersion: string
    CompilerKey: string
    OptionsHash: string
    FunctionName: string
    FunctionHash: string
}

/// Ensure the cache directory exists
let private ensureCacheDir () : Result<unit, string> =
    try
        let dir = getCacheDir ()
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

/// Run a cache operation with a fresh connection
let private withConnection (f: SqliteConnection -> Result<'T, string>) : Result<'T, string> =
    ensureCacheDir ()
    |> Result.bind (fun () ->
        try
            let dbPath = getCacheDbPath ()
            use conn = new SqliteConnection($"Data Source={dbPath}")
            conn.Open()
            ensureSchema conn
            |> Result.bind (fun () -> f conn)
        with ex ->
            Error $"Cache connection error: {ex.Message}")

/// Get a batch of cached function entries using a single connection
let getFunctions<'T> (keys: FunctionCacheKey list) : Result<Map<string, 'T>, string> =
    if List.isEmpty keys then
        Ok Map.empty
    else
        let read (conn: SqliteConnection) : Result<Map<string, 'T>, string> =
            try
                use cmd = conn.CreateCommand()
                cmd.CommandText <- """
                    SELECT data FROM function_cache
                    WHERE cache_version = $cacheVersion
                      AND compiler_key = $compilerKey
                      AND options_hash = $optionsHash
                      AND function_name = $functionName
                      AND function_hash = $functionHash
                """
                let pCacheVersion = cmd.Parameters.AddWithValue("$cacheVersion", "") 
                let pCompilerKey = cmd.Parameters.AddWithValue("$compilerKey", "")
                let pOptionsHash = cmd.Parameters.AddWithValue("$optionsHash", "")
                let pFunctionName = cmd.Parameters.AddWithValue("$functionName", "")
                let pFunctionHash = cmd.Parameters.AddWithValue("$functionHash", "")

                let updateParams (key: FunctionCacheKey) : unit =
                    pCacheVersion.Value <- key.CacheVersion
                    pCompilerKey.Value <- key.CompilerKey
                    pOptionsHash.Value <- key.OptionsHash
                    pFunctionName.Value <- key.FunctionName
                    pFunctionHash.Value <- key.FunctionHash

                let folder (accResult: Result<Map<string, 'T>, string>) (key: FunctionCacheKey) =
                    accResult
                    |> Result.bind (fun acc ->
                        updateParams key
                        let obj = cmd.ExecuteScalar()
                        if isNull obj then
                            Ok acc
                        else
                            match obj with
                            | :? (byte array) as bytes ->
                                deserialize<'T> bytes
                                |> Result.map (fun value -> Map.add key.FunctionName value acc)
                            | _ ->
                                Error "Cache read error: unexpected function data type")

                List.fold folder (Ok Map.empty) keys
            with ex ->
                Error $"Cache read error: {ex.Message}"
        withConnection read

/// Store a batch of cached function entries using a single connection
let setFunctions<'T> (entries: (FunctionCacheKey * 'T) list) : Result<unit, string> =
    if List.isEmpty entries then
        Ok ()
    else
        let write (conn: SqliteConnection) : Result<unit, string> =
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

                let folder (accResult: Result<unit, string>) (key, value) =
                    accResult
                    |> Result.bind (fun () ->
                        serialize value
                        |> Result.bind (fun bytes ->
                            updateParams key bytes
                            cmd.ExecuteNonQuery() |> ignore
                            Ok ()))

                match List.fold folder (Ok ()) entries with
                | Ok () ->
                    transaction.Commit()
                    Ok ()
                | Error err -> Error err
            with ex ->
                Error $"Cache write error: {ex.Message}"
        withConnection write

/// Get a cached function entry
let getFunction<'T> (key: FunctionCacheKey) : Result<'T option, string> =
    getFunctions<'T> [key]
    |> Result.map (fun cached -> Map.tryFind key.FunctionName cached)

/// Store a cached function entry
let setFunction<'T> (key: FunctionCacheKey) (value: 'T) : Result<unit, string> =
    setFunctions<'T> [ (key, value) ]
