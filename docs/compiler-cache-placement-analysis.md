# Compiler Cache Placement Analysis

This note evaluates where to compute function cache keys and capture cached
artifacts in the compiler pipeline. It focuses on correctness, invalidation
precision, and opportunities to move caching earlier without missing
dependencies.

## Current Placement (Post-ANF Optimization + Inlining + RC + TCO)

Today, function dependency hashes are computed from ANF after:

- ANF optimization
- ANF inlining
- reference count insertion
- tail-call detection
- print insertion (user entry function)

The cache key includes:

- compiler hash and cache version
- options hash (all optimizations and codegen toggles)
- per-function dependency hash

The dependency hash includes:

- function body hash (post-inlining ANF body + signature)
- function type hash (types used in body + registry definitions)
- signature hashes of direct callees (post-inlining call graph)

External functions are hashed via function type + associated record/sum type
definitions.

This placement ensures that any inlining changes (caller body changes due to
callee changes) are captured, because the body hash is taken after inlining.
It also ensures that post-optimization ANF is represented, and the key covers
RC-related type dependencies.

## What Drives Correctness

To safely reuse cached LIR for a function, the cache key must account for:

- the function body after inlining (callee changes can change the caller body)
- any type definitions referenced by the function
- calling conventions and codegen options (encoded via options hash)
- any external function dependencies

If any of these change without invalidation, the cached function could be
incorrect or ABI-incompatible.

## Earlier Placement: Options and Risks

### 1) Pre-Inlining (Still Post-ANF Optimization)

If we hash pre-inlining ANF bodies, we must ensure that inlining decisions are
accounted for. Inlining depends on callee bodies (size, recursion, closure
presence), so either:

- we still run inlining to compute a post-inline body hash, which removes any
  savings, or
- we include direct callee signatures in the dependency hash, which still does
  not capture inlining decisions without running the inliner.

### 2) Pre-ANF Optimization (Post-ANF Conversion)

If we hash pre-optimization ANF and rely on deterministic optimization, then
changes in the pre-opt IR plus the options hash uniquely determine the
optimized result. This can be correct, but:

- it increases invalidations because pre-opt bodies include dead/duplicate
  code that the optimizer would remove, and
- it still does not resolve inlining dependencies without either running the
  inliner or conservatively hashing callees.

### 3) Pre-RC Insertion

The current type hash includes types used in the function body and type
definitions from registries. Today, part of that type usage is inferred from
the RC insertion type map. Moving earlier would require a new, deterministic
type-use analysis that does not depend on RC insertion.

Without a replacement type map, pre-RC hashing risks missing type
dependencies introduced by temp values or inferred return types, which is
unsafe.

### 4) Pre-ANF (Typed AST Level)

At the typed AST level, we can compute function signatures and type
dependencies, but we do not have stable ANF bodies. Any cache keyed at this
level would need a different cached artifact (e.g., AST/ANF), not the current
LIR. This is effectively a separate cache design.

## Summary of Tradeoffs

Post-inlining hashing maximizes correctness and minimizes invalidation, at the
cost of computing ANF inlining for all functions (which is already required to
produce cached LIR for misses).

Earlier hashing is possible but either:

- requires conservative dependency inclusion (more invalidations), or
- requires a new type map analysis to avoid missing type dependencies, or
- needs a different cached artifact (pre-ANF or pre-opt IR).

Given the current cache artifact (symbolic LIR post-regalloc), the safest and
most precise placement is post-inlining and post-ANF optimization.
