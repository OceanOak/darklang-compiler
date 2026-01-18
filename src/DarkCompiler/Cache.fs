// Cache.fs - SQLite-based caching for compiler intermediates and test results
//
// Uses SQLite for persistent storage and MessagePack for efficient serialization.
// Cache location: ~/.cache/dark-compiler/cache.db
//
// Cache key structure:
//   - Compiler Key = SHA256(compiler_binary) - identifies compiler version
//   - Source Key = SHA256(source_code) - identifies input program
//   - Options Key = SHA256(msgpack(options)) - identifies compilation settings
//   - Platform = "macos" | "linux" - target platform

module Cache

open System
open System.IO
open System.Security.Cryptography
open Microsoft.Data.Sqlite
open MessagePack
open MessagePack.FSharp
open MessagePack.Resolvers

/// Cache location: ~/.cache/dark-compiler/cache.db
let getCacheDir () : string =
    Path.Combine(
        Environment.GetFolderPath(Environment.SpecialFolder.UserProfile),
        ".cache",
        "dark-compiler")

/// Get the cache database path
let getCacheDbPath () : string =
    Path.Combine(getCacheDir (), "cache.db")

/// MessagePack configuration with contractless resolver for F# types
/// Uses ContractlessStandardResolver so no annotations are needed on IR types
let private resolver =
    CompositeResolver.Create(
        FSharpResolver.Instance,
        ContractlessStandardResolver.Instance)

let private options =
    MessagePackSerializerOptions.Standard
        .WithResolver(resolver)
        .WithSecurity(MessagePackSecurity.UntrustedData)

/// Serialize data to MessagePack bytes
let serialize<'T> (data: 'T) : byte[] =
    MessagePackSerializer.Serialize(data, options)

/// Deserialize MessagePack bytes to data
let deserialize<'T> (bytes: byte[]) : 'T =
    MessagePackSerializer.Deserialize<'T>(bytes, options)

/// Compute SHA256 hash of a string
let hashString (s: string) : string =
    use sha256 = SHA256.Create()
    let bytes = System.Text.Encoding.UTF8.GetBytes(s)
    let hash = sha256.ComputeHash(bytes)
    BitConverter.ToString(hash).Replace("-", "").ToLowerInvariant()

/// Compute SHA256 hash of bytes
let hashBytes (bytes: byte[]) : string =
    use sha256 = SHA256.Create()
    let hash = sha256.ComputeHash(bytes)
    BitConverter.ToString(hash).Replace("-", "").ToLowerInvariant()

/// Get the compiler key (SHA256 of compiler binary)
/// This uniquely identifies this build of the compiler
let getCompilerKey () : string =
    let assembly = System.Reflection.Assembly.GetExecutingAssembly()
    let location = assembly.Location
    use stream = File.OpenRead(location)
    use sha256 = SHA256.Create()
    let hash = sha256.ComputeHash(stream)
    BitConverter.ToString(hash).Replace("-", "").ToLowerInvariant()

/// Hash any serializable data to a string key
/// Use this to hash compiler options by serializing them first
let hashData<'T> (data: 'T) : string =
    let bytes = serialize data
    hashBytes bytes

/// Get the current platform as a string
let getPlatform () : string =
    match Platform.detectOS () with
    | Ok Platform.MacOS -> "macos"
    | Ok Platform.Linux -> "linux"
    | Error _ -> "unknown"

/// Cached test result
type CachedTestResult = {
    Success: bool
    Message: string
    Stdout: string option
    Stderr: string option
    ExitCode: int option
}

/// Ensure the cache directory exists
let private ensureCacheDir () : unit =
    let dir = getCacheDir ()
    if not (Directory.Exists dir) then
        Directory.CreateDirectory dir |> ignore

/// Create database schema
let private createSchema (conn: SqliteConnection) : unit =
    use cmd = conn.CreateCommand()
    cmd.CommandText <- """
        CREATE TABLE IF NOT EXISTS compiler_intermediates (
            compiler_key TEXT NOT NULL,
            source_hash TEXT NOT NULL,
            options_hash TEXT NOT NULL,
            stage TEXT NOT NULL,
            data BLOB NOT NULL,
            created_at INTEGER NOT NULL,
            PRIMARY KEY (compiler_key, source_hash, options_hash, stage)
        );

        CREATE TABLE IF NOT EXISTS generated_code (
            compiler_key TEXT NOT NULL,
            source_hash TEXT NOT NULL,
            options_hash TEXT NOT NULL,
            platform TEXT NOT NULL,
            binary_data BLOB NOT NULL,
            created_at INTEGER NOT NULL,
            PRIMARY KEY (compiler_key, source_hash, options_hash, platform)
        );

        CREATE TABLE IF NOT EXISTS test_results (
            compiler_key TEXT NOT NULL,
            test_file_hash TEXT NOT NULL,
            test_name TEXT NOT NULL,
            success INTEGER NOT NULL,
            message TEXT,
            exit_code INTEGER,
            stdout TEXT,
            stderr TEXT,
            created_at INTEGER NOT NULL,
            PRIMARY KEY (compiler_key, test_file_hash, test_name)
        );
    """
    cmd.ExecuteNonQuery() |> ignore

/// Flag to track if schema has been created
let mutable private schemaCreated = false
let private schemaLock = obj()

/// Create a new database connection (thread-safe, one connection per operation)
let private getConnection () : SqliteConnection =
    ensureCacheDir ()
    let dbPath = getCacheDbPath ()
    let connStr = $"Data Source={dbPath}"
    let conn = new SqliteConnection(connStr)
    conn.Open()
    // Create schema only once, protected by lock
    lock schemaLock (fun () ->
        if not schemaCreated then
            createSchema conn
            schemaCreated <- true)
    conn

/// Check if caching is enabled (can be disabled via environment variable)
let isCacheEnabled () : bool =
    let envVar = Environment.GetEnvironmentVariable("DARK_COMPILER_NO_CACHE")
    String.IsNullOrEmpty(envVar) || envVar.ToLowerInvariant() <> "1"

/// Get cached intermediate data for a compilation stage
let getIntermediate
    (compilerKey: string)
    (sourceHash: string)
    (optionsHash: string)
    (stage: string)
    : byte[] option =
    if not (isCacheEnabled ()) then None
    else
        use conn = getConnection ()
        use cmd = conn.CreateCommand()
        cmd.CommandText <- """
            SELECT data FROM compiler_intermediates
            WHERE compiler_key = $compilerKey
              AND source_hash = $sourceHash
              AND options_hash = $optionsHash
              AND stage = $stage
        """
        cmd.Parameters.AddWithValue("$compilerKey", compilerKey) |> ignore
        cmd.Parameters.AddWithValue("$sourceHash", sourceHash) |> ignore
        cmd.Parameters.AddWithValue("$optionsHash", optionsHash) |> ignore
        cmd.Parameters.AddWithValue("$stage", stage) |> ignore
        use reader = cmd.ExecuteReader()
        if reader.Read() then
            Some (reader.GetValue(0) :?> byte[])
        else
            None

/// Set cached intermediate data for a compilation stage
let setIntermediate
    (compilerKey: string)
    (sourceHash: string)
    (optionsHash: string)
    (stage: string)
    (data: byte[])
    : unit =
    if isCacheEnabled () then
        use conn = getConnection ()
        use cmd = conn.CreateCommand()
        cmd.CommandText <- """
            INSERT OR REPLACE INTO compiler_intermediates
            (compiler_key, source_hash, options_hash, stage, data, created_at)
            VALUES ($compilerKey, $sourceHash, $optionsHash, $stage, $data, $createdAt)
        """
        cmd.Parameters.AddWithValue("$compilerKey", compilerKey) |> ignore
        cmd.Parameters.AddWithValue("$sourceHash", sourceHash) |> ignore
        cmd.Parameters.AddWithValue("$optionsHash", optionsHash) |> ignore
        cmd.Parameters.AddWithValue("$stage", stage) |> ignore
        cmd.Parameters.AddWithValue("$data", data) |> ignore
        cmd.Parameters.AddWithValue("$createdAt", DateTimeOffset.UtcNow.ToUnixTimeSeconds()) |> ignore
        cmd.ExecuteNonQuery() |> ignore

/// Get cached generated binary
let getGeneratedCode
    (compilerKey: string)
    (sourceHash: string)
    (optionsHash: string)
    (platform: string)
    : byte[] option =
    if not (isCacheEnabled ()) then None
    else
        use conn = getConnection ()
        use cmd = conn.CreateCommand()
        cmd.CommandText <- """
            SELECT binary_data FROM generated_code
            WHERE compiler_key = $compilerKey
              AND source_hash = $sourceHash
              AND options_hash = $optionsHash
              AND platform = $platform
        """
        cmd.Parameters.AddWithValue("$compilerKey", compilerKey) |> ignore
        cmd.Parameters.AddWithValue("$sourceHash", sourceHash) |> ignore
        cmd.Parameters.AddWithValue("$optionsHash", optionsHash) |> ignore
        cmd.Parameters.AddWithValue("$platform", platform) |> ignore
        use reader = cmd.ExecuteReader()
        if reader.Read() then
            Some (reader.GetValue(0) :?> byte[])
        else
            None

/// Set cached generated binary
let setGeneratedCode
    (compilerKey: string)
    (sourceHash: string)
    (optionsHash: string)
    (platform: string)
    (binary: byte[])
    : unit =
    if isCacheEnabled () then
        use conn = getConnection ()
        use cmd = conn.CreateCommand()
        cmd.CommandText <- """
            INSERT OR REPLACE INTO generated_code
            (compiler_key, source_hash, options_hash, platform, binary_data, created_at)
            VALUES ($compilerKey, $sourceHash, $optionsHash, $platform, $binary, $createdAt)
        """
        cmd.Parameters.AddWithValue("$compilerKey", compilerKey) |> ignore
        cmd.Parameters.AddWithValue("$sourceHash", sourceHash) |> ignore
        cmd.Parameters.AddWithValue("$optionsHash", optionsHash) |> ignore
        cmd.Parameters.AddWithValue("$platform", platform) |> ignore
        cmd.Parameters.AddWithValue("$binary", binary) |> ignore
        cmd.Parameters.AddWithValue("$createdAt", DateTimeOffset.UtcNow.ToUnixTimeSeconds()) |> ignore
        cmd.ExecuteNonQuery() |> ignore

/// Get cached test result
let getTestResult
    (compilerKey: string)
    (testFileHash: string)
    (testName: string)
    : CachedTestResult option =
    if not (isCacheEnabled ()) then None
    else
        use conn = getConnection ()
        use cmd = conn.CreateCommand()
        cmd.CommandText <- """
            SELECT success, message, exit_code, stdout, stderr FROM test_results
            WHERE compiler_key = $compilerKey
              AND test_file_hash = $testFileHash
              AND test_name = $testName
        """
        cmd.Parameters.AddWithValue("$compilerKey", compilerKey) |> ignore
        cmd.Parameters.AddWithValue("$testFileHash", testFileHash) |> ignore
        cmd.Parameters.AddWithValue("$testName", testName) |> ignore
        use reader = cmd.ExecuteReader()
        if reader.Read() then
            let success = reader.GetInt32(0) <> 0
            let message =
                if reader.IsDBNull(1) then "" else reader.GetString(1)
            let exitCode =
                if reader.IsDBNull(2) then None else Some (reader.GetInt32(2))
            let stdout =
                if reader.IsDBNull(3) then None else Some (reader.GetString(3))
            let stderr =
                if reader.IsDBNull(4) then None else Some (reader.GetString(4))
            Some {
                Success = success
                Message = message
                Stdout = stdout
                Stderr = stderr
                ExitCode = exitCode
            }
        else
            None

/// Set cached test result
let setTestResult
    (compilerKey: string)
    (testFileHash: string)
    (testName: string)
    (result: CachedTestResult)
    : unit =
    if isCacheEnabled () then
        use conn = getConnection ()
        use cmd = conn.CreateCommand()
        cmd.CommandText <- """
            INSERT OR REPLACE INTO test_results
            (compiler_key, test_file_hash, test_name, success, message, exit_code, stdout, stderr, created_at)
            VALUES ($compilerKey, $testFileHash, $testName, $success, $message, $exitCode, $stdout, $stderr, $createdAt)
        """
        cmd.Parameters.AddWithValue("$compilerKey", compilerKey) |> ignore
        cmd.Parameters.AddWithValue("$testFileHash", testFileHash) |> ignore
        cmd.Parameters.AddWithValue("$testName", testName) |> ignore
        cmd.Parameters.AddWithValue("$success", if result.Success then 1 else 0) |> ignore
        cmd.Parameters.AddWithValue("$message", result.Message) |> ignore
        match result.ExitCode with
        | Some code -> cmd.Parameters.AddWithValue("$exitCode", code) |> ignore
        | None -> cmd.Parameters.AddWithValue("$exitCode", DBNull.Value) |> ignore
        match result.Stdout with
        | Some s -> cmd.Parameters.AddWithValue("$stdout", s) |> ignore
        | None -> cmd.Parameters.AddWithValue("$stdout", DBNull.Value) |> ignore
        match result.Stderr with
        | Some s -> cmd.Parameters.AddWithValue("$stderr", s) |> ignore
        | None -> cmd.Parameters.AddWithValue("$stderr", DBNull.Value) |> ignore
        cmd.Parameters.AddWithValue("$createdAt", DateTimeOffset.UtcNow.ToUnixTimeSeconds()) |> ignore
        cmd.ExecuteNonQuery() |> ignore

/// Clear all cache data
let clearCache () : unit =
    use conn = getConnection ()
    use cmd = conn.CreateCommand()
    cmd.CommandText <- """
        DELETE FROM compiler_intermediates;
        DELETE FROM generated_code;
        DELETE FROM test_results;
    """
    cmd.ExecuteNonQuery() |> ignore

/// Get cache statistics
type CacheStats = {
    IntermediateCount: int
    GeneratedCodeCount: int
    TestResultCount: int
    TotalSizeBytes: int64
}

/// Get cache statistics
let getCacheStats () : CacheStats =
    use conn = getConnection ()

    let countQuery table =
        use cmd = conn.CreateCommand()
        cmd.CommandText <- $"SELECT COUNT(*) FROM {table}"
        cmd.ExecuteScalar() :?> int64 |> int

    let intermediateCount = countQuery "compiler_intermediates"
    let generatedCodeCount = countQuery "generated_code"
    let testResultCount = countQuery "test_results"

    let dbPath = getCacheDbPath ()
    let totalSize =
        if File.Exists dbPath then
            FileInfo(dbPath).Length
        else
            0L

    {
        IntermediateCount = intermediateCount
        GeneratedCodeCount = generatedCodeCount
        TestResultCount = testResultCount
        TotalSizeBytes = totalSize
    }

/// Close the cache connection (no-op, connections are now per-operation)
let closeConnection () : unit = ()
