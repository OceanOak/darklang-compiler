# Compiler Cache

The compiler uses a small SQLite 3 cache to speed up repeat compilations while
keeping the implementation easy to understand and safe to invalidate.

Cache file location:

```
~/.cache/dark-compiler/cache.db
```

Disable caching with `--no-cache` (compiler CLI) or `./run-tests --no-cache`.

## Goals

- Speed up repeat compiles without changing compiler output.
- Cache only the smallest useful artifacts.
- Keep cache keys explicit and easy to reason about.
- Make invalidation obvious and reliable.

## What We Cache

Only one artifact type:

1. **Function artifacts**: post-register-allocation `LIRSymbolic.Function` values
   (including the synthesized `_start` entrypoint).

This keeps the cache small and avoids storing large IR graphs or binaries.

## Where the Cache Applies

- **Stdlib**: function cache is used when lowering stdlib functions.
- **Preamble**: function cache is used for per-file preamble compilation.
- **User code / test expressions**: function cache is used when lowering user
  functions.

## Design Choices and Assumptions

- **SQLite 3**: simple, local, cross-platform, no extra services.
- **Minimal artifacts**: only store LIRSymbolic functions (no binaries or
  intermediate IR programs).
- **Dependency-based invalidation**: cache keys are derived from the function's
  body, the types it touches, and the signatures of its non-inlined callees.
- **Compiler hash**: SHA256 of the compiler binary invalidates the entire cache
  on compiler changes.
- **Post-inlining inputs**: hashes are computed from ANF after inlining, so
  inlining changes invalidate cached functions.
- **Fail fast on hash errors**: if MessagePack cannot serialize a value used for
  hashing, we crash rather than produce incorrect keys.
- **Determinism assumption**: given the same typed program and options, passes
  are deterministic. Any non-determinism would break caching correctness.
- **Fail-fast I/O**: when caching is enabled, cache read/write errors fail the
  compilation so we never silently ignore I/O problems.

## Cache Keying

All cache entries include:

- Cache version (`v4`)
- Compiler hash (SHA256 of the compiler binary)

### Function Cache Keys

Each function entry is keyed by:

- **Context hash**: a fixed tag for the function cache (kept to preserve the
  simple schema; all meaningful invalidation lives in the function hash).
- **Options hash**: hash of cacheable compiler options (all optimization and
  runtime flags that affect output; debug dump flags are excluded).
- **Function name**: for clarity and collision avoidance.
- **Function hash**: dependency hash derived from the **post-inlining ANF
  function**, the types it uses, and the hashes of its callees.

### Dependency Hash Inputs

Each function's dependency hash is built from:

- **Body hash**: hash of the post-inlining ANF function body plus its typed
  parameters and return type.
- **Type hash**: hash of all types the function touches, including:
  - types in the function signature,
  - explicit types in ANF (TypedAtom/Print/RawGet/RawSet),
  - TempId types from the RC TypeMap,
  - record and sum type definitions referenced by those types.
- **Callee hashes** (post-inlining call graph, so only non-inlined calls remain):
  - **Internal callees**: signature hashes of other functions in the same
    compilation batch (function type plus type dependencies).
  - **External callees**: signature hashes of stdlib/preamble functions
    (function type plus type dependencies).

### Recursion

Recursive functions are hashed independently. Calls to other functions (including
mutual recursion) contribute only signature hashes, so body changes in one
function do not invalidate its callers unless the signature changes.

## Hashing and Serialization

- Hashes are SHA256 of MessagePack-serialized values (`Cache.hashData`).
- MessagePack uses `FSharpResolver` + `ContractlessStandardResolver` with
  `UntrustedData` security.
- Discriminated unions with named fields use `[<MessagePackObject(false)>]`
  for integer-key (positional) serialization to avoid the known named-field
  constructor mismatch bug in MessagePack's F# resolver.
- Simple string content (source text, empty preamble) uses `Cache.hashString`.

## Database Schema

Two tables:

`function_cache`
- `cache_version`, `compiler_key`, `options_hash`, `function_name`,
  `function_hash`, `data`, `created_at`
- Primary key: all key fields above.

`compiler_meta`
- `compiler_path`, `compiler_key`, `updated_at`
- Primary key: `compiler_path`.

`created_at`/`updated_at` are informational only.

## Behavior and Invalidation

- Cache reads/writes are batched per compilation stage to avoid per-function
  SQLite connection overhead.
- Any cache miss results in a normal compile path.
- Cache I/O errors fail compilation unless caching is disabled via `--no-cache`.
- If the compiler binary changes at the same path, cached entries keyed on the
  old compiler hash are evicted.
- Bump `Cache.cacheVersion` if the schema or keying semantics change.
- Delete `~/.cache/dark-compiler/cache.db` to fully clear the cache.

## What Is Not Cached

- Test results.
- Intermediate IRs (AST/ANF/MIR/LIR programs).
- Final binaries (Mach-O/ELF).
- String/float pools (they are per-program; cached functions are stored in
  symbolic LIR before pool resolution).

## Known Limitations

- Eviction only happens when a compiler binary changes at a known path; caches
  otherwise grow until manually cleared.
- No cross-compiler sharing (compiler hash is part of the key).
