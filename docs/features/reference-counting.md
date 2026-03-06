# Reference Counting: Coverage, Gaps, and Expansion Plan

This document is a codebase-wide RC audit. It describes:

- where RC logic exists across compiler passes and runtime codegen
- where RC is not currently applied
- edge cases and mismatches in current behavior
- a concrete plan to expand RC across the type system and runtime representation

Status in this document reflects the codebase as of 2026-03-06.

## What Exists Today

The compiler currently uses a mixed memory model:

- generic RC insertion for selected heap-like ANF types
- special list RC helpers for tagged FingerTree nodes
- string-specific RC intrinsics in IR/codegen
- raw-memory paths for HAMT and other internals that bypass generic RC

This is not yet a single unified ownership model.

## RC Through the Pipeline

## 1) AST / Type checking

Type checking tracks source types (`AST.Type`) but does not encode ownership kinds.
There is no explicit "RC-managed runtime representation" type in the source type system.

## 2) ANF lowering (`2_AST_to_ANF.fs`)

RC-adjacent behavior already exists before pass 2.5:

- list literal lowering builds FingerTree nodes with `RawAlloc` / `RawSet`
- it sets node refcounts manually
- it can insert `RefCountInc` for heap elements stored in leaf nodes (`addLeafInc`)
- raw-memory intrinsics carry optional payload type metadata (`RawGet` / `RawSet` value type)

This means ownership-affecting behavior is currently split across pass 2 and pass 2.5.

## 3) RC insertion (`2.5_RefCountInsertion.fs`)

Pass 2.5 inserts `RefCountInc` / `RefCountDec` over ANF based on inferred types and borrowing rules.

Core model:

- borrowed calling convention
- dec on scope exit for owned RC values
- no dec for borrowed aliases
- no dec for returned values
- param return materialization for borrowed convention

Additional responsibilities:

- type inference/cache for `TempId -> Type` used by downstream passes
- cleanup reordering before non-self tailcalls

## 4) Tailcalls and RC cleanup (`2.7_TailCallDetection.fs`, `3_ANF_to_MIR.fs`)

Tailcall detection explicitly treats refcount decs as cleanup that may sit between call and return.
ANF->MIR has explicit logic to preserve self-tailcall cleanup and avoid overlap bugs with tail args.

RC and TCO are therefore tightly coupled in lowering correctness.

## 5) Optimization passes and side effects

ANF and MIR optimizers classify RC operations as side-effecting, so they are not eliminated as dead pure code.
This is essential for correctness.

## 6) MIR/LIR lowering

`RefCountInc/Dec` and string RC ops are preserved through MIR and LIR without semantic reinterpretation.

## 7) ARM64 codegen (`6_CodeGen.fs`)

Generic RC:

- heap blocks are treated as `[payload][refcount:8]`
- `RefCountDec` decrements and pushes to free list on zero

Specialized RC:

- list helper routines `__dark_list_refcount_inc_helper` / `__dark_list_refcount_dec_helper`
- dynamic string refcount intrinsics (`RefCountIncString` / `RefCountDecString`)

Allocator state:

- `X27`: free-list head table base (256 bytes)
- `X28`: bump allocation pointer

## Type Coverage Matrix (Current)

| Source type | Runtime shape (today) | Auto RC via pass 2.5 | Special RC path | Notes |
|---|---|---|---|---|
| `TTuple` | heap block + trailing refcount | Yes | No | payload-size based generic RC |
| `TRecord` | heap block + trailing refcount | Yes | No | same as tuple layout |
| `TSum` | mixed (immediate for pure enums, heap for payload variants) | Partially | No | modeled conservatively as heap-like in some paths |
| `TList` | tagged FingerTree pointer | Yes | Yes | codegen uses list helpers for payload size 24 |
| `TDict` | tagged root + raw HAMT nodes | No (excluded) | Partial (`RawSet` edge inc) | root excluded from generic RC |
| `TString` | mixed (pooled const / heap string) | No | Intrinsics exist | no broad insertion of `RefCountDecString` |
| `TBytes` | heap-like layout in stdlib (`len,data,refcount`) | No | No | no automatic lifetime decref |
| `TFunction` | closure pointer at runtime when captured | No | No | closure representation mismatch with source type |
| `TRawPtr` | unmanaged pointer | No | No | `RawFree` currently no-op |
| Primitive scalars | immediate | No | No | expected |

## Where RC Does Not Cover the Codebase

The following are currently outside complete RC coverage:

- strings as ordinary values (no broad automatic decref insertion)
- bytes values (`TBytes`) lifetime management
- closure values typed as `TFunction` at use sites
- dict/HAMT node lifecycle (raw memory domain)
- raw pointers in general (`RawFree` no-op)
- large-object free-list class handling consistency
- explicit representation-level ownership kinds in the type system

## Edge Cases and Gaps

## A) Payload-size-24 collision with list helper dispatch (verified)

Codegen currently routes generic RC to list helpers when `payloadSize = 24`.
That collides with non-list payload-24 objects (for example tuple3/record3).

Observed repros:

```bash
./dark --leak-check /tmp/rc_tuple3.dark && ./dark.out   # 6leaks: 1
./dark --leak-check /tmp/rc_record3.dark && ./dark.out  # 6leaks: 1
```

## B) Closure representation mismatch (`ClosureAlloc` vs `TFunction`) (verified)

Closures are heap-backed at runtime, but many call-site temps are typed `TFunction`.
`TFunction` is not RC-managed by generic pass logic, so cleanup can be skipped.

Observed repro:

```bash
./dark --leak-check /tmp/rc_closure1cap.dark && ./dark.out  # 4leaks: 1
```

## C) `TDict` consistency mismatch across passes (high confidence from code)

`ANF.isHeapType` includes `TDict`, but `RefCountInsertion.isRcManagedHeapType` excludes `TDict`.
Lowering code that uses `ANF.isHeapType` (for example list-literal leaf ownership inc) can emit dict RC ops even though dict roots are excluded from generic RC policy.

This is a model inconsistency and should be resolved in one place.

## D) Strings are not lifetime-managed end-to-end (verified)

`RefCountIncString/DecString` exists in IR/codegen, but pass 2.5 does not generally insert them for normal string scopes.
`RefCountDecString` also does not currently reclaim string memory.

Observed repros:

```bash
./dark --leak-check /tmp/rc_string_literal.dark && ./dark.out  # 3leaks: 1
./dark --leak-check /tmp/rc_string_concat.dark && ./dark.out   # 2leaks: 1
```

## E) String/Bytes layout mismatch for dynamic refcount offset (high confidence from code)

String/bytes constructors in stdlib Dark code often write refcount at `8 + len`.
String RC intrinsics in codegen read/write at `8 + aligned(len)`.

Before expanding automatic string/bytes RC, layout rules must be unified.

## F) `TBytes` not managed by pass 2.5 (verified)

Bytes values can be heap-backed but are not included in generic RC-managed type checks.

Observed repro:

```bash
./dark --leak-check /tmp/rc_bytes.dark && ./dark.out  # 4leaks: 1
```

## G) Dict/HAMT raw-memory lifetime is mostly unmanaged (verified)

HAMT nodes use raw alloc/set/get operations; `RawFree` is a no-op.
Dict roots are excluded from generic RC.

Observed repro:

```bash
./dark --leak-check /tmp/rc_dict.dark && ./dark.out  # 1leaks: 1
```

## H) Free-list class bounds are inconsistent between alloc paths (high confidence from code)

`RawAlloc` has explicit bounds checks for free-list classes (`<= 248` payload class).
Generic `HeapAlloc`/`RefCountDec` paths use `[X27 + payloadSize]` directly without equivalent class-bounds gating.

Large payload objects can therefore index outside the intended 256-byte head table.

## I) Borrowing rules are partly name-based (high confidence from code)

`isBorrowingExpr` has hardcoded function-name checks for FingerTree accessors.
This is brittle and easy to miss when adding new borrowed-return APIs.

## J) Type inference fallback can suppress RC opportunities (high confidence from code)

When types are unresolved (`TVar`) around `RawGet`/alias chains/sum payload recovery, pass 2.5 errs toward unknown instead of unsafe defaults.
That avoids wrong frees but can miss required RC updates and leak.

## K) Sum payload typing for multi-arg sums remains fragile (high confidence from code)

`TupleGet` payload inference for `TSum` is straightforward for single type arg and less precise for multi-parameter sums.
The pass has alias-based recovery logic, but this remains a fragile area for ownership inference.

## L) Standard RC constraints still apply

- no cycle collection
- non-atomic refcounts (single-threaded assumptions)

## Expansion Plan: RC Across the Full Type System

## Goal

Move from "mixed ad hoc RC policy" to "single representation-aware ownership model" where each runtime value shape has explicit, consistent retain/release behavior.

## Phase 1: Stabilize existing generic RC behavior

1. Remove payload-size dispatch ambiguity.
2. Introduce explicit list RC ops or list-specific IR op variants (do not infer list from `payloadSize = 24`).
3. Add bounds-safe size-class mapping for all free-list users.
4. Unify `TDict` handling rule between ANF helpers and pass 2.5.

## Phase 2: Introduce representation-aware ownership typing

1. Add a runtime representation kind in ANF typing (distinct from source `AST.Type`).
2. Model closure values explicitly as a runtime heap representation (not plain `TFunction` for ownership decisions).
3. Separate immediate sums vs boxed sums in ownership inference.

Example direction:

```fsharp
type RcShape =
    | FixedOffset of payloadSize:int
    | TaggedListNode
    | DynamicString
    | DynamicBytes
    | DictRoot
```

One central `tryRcShape` should drive all retain/release decisions.

## Phase 3: Extend RC to `String` and `Bytes`

1. Canonicalize runtime layout and offset rules (`aligned` vs non-aligned trailing fields).
2. Insert lifetime decrefs for string/bytes in pass 2.5 (or a dedicated ownership pass).
3. Implement actual reclaim path for zero-refcount dynamic strings/bytes.
4. Update stdlib constructors to match canonical layout.

## Phase 4: Integrate Dict/HAMT lifecycle

Choose one explicit strategy:

1. Make HAMT nodes RC-managed with retain/release semantics per edge.
2. Or formalize dict/HAMT as a separate region/arena discipline with explicit reclamation.

Do not keep current hybrid "raw alloc + no-op free + partial edge increments" model.

## Phase 5: Make borrow analysis semantic, not name-based

1. Annotate APIs/IR ops with borrow/own return semantics.
2. Drive RC insertion from annotations and representation metadata.
3. Remove hardcoded function-name borrowing checks.

## Phase 6: RC optimization after correctness

After semantics are unified:

1. RC elision for provably unique values
2. loop-aware/batched RC motion
3. specialized fast paths for immutable single-owner temporaries

## Test Expansion Needed

Current tests focus heavily on tuple/list basics and TCO interactions.
Add systematic leak-check matrix coverage for:

- tuple/record payload boundaries (16, 24, 32, >248)
- closures returned/stored/passed in containers
- list of dict/list of closure/list of bytes/list of string
- bytes lifetimes and transformations
- string constructors using stdlib raw builders
- dict-heavy mutation sequences with delete/rebuild
- mixed tailcall + RC cleanup with alias overlap

Use:

```bash
./dark --dump-anf prog.dark
./dark --dump-mir prog.dark
./dark --dump-lir prog.dark
./dark --leak-check prog.dark && ./dark.out
```

## Key Files

- `src/DarkCompiler/ANF.fs`
- `src/DarkCompiler/passes/2_AST_to_ANF.fs`
- `src/DarkCompiler/passes/2.5_RefCountInsertion.fs`
- `src/DarkCompiler/passes/2.7_TailCallDetection.fs`
- `src/DarkCompiler/passes/3_ANF_to_MIR.fs`
- `src/DarkCompiler/passes/4_MIR_to_LIR.fs`
- `src/DarkCompiler/passes/6_CodeGen.fs`
- `src/DarkCompiler/stdlib/String.dark`
- `src/DarkCompiler/stdlib/Bytes.dark`
- `src/DarkCompiler/stdlib/__FingerTree.dark`
- `src/DarkCompiler/stdlib/__HAMT.dark`
