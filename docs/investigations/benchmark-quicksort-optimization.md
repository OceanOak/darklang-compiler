# Benchmark Investigation: Quicksort

## Executive Summary

**Benchmark:** quicksort (functional quicksort with three-way partition)
**Dark Performance:** Full benchmark exits with allocator OOM (`Out of heap memory`, exit 1); reduced quick check runs
**Rust Performance:** 6,506,788 instructions (baseline)
**OCaml Performance:** 47,643,021 instructions (7.32x slower than Rust)

Dark cannot run `benchmarks/problems/quicksort/dark/main.dark` at benchmark size and is skipped by `run_benchmarks.sh`. The allocator now traps cleanly instead of segfaulting, but quicksort still exceeds available heap due high allocation pressure from list-heavy partitioning.

## Benchmark Implementation Comparison

### Dark Implementation
```dark
def quicksort(arr: List<Int64>) : List<Int64> =
    let len = Stdlib.List.length<Int64>(arr) in
    if len <= 1 then arr
    else
        let pivot = getAtOrDefault(arr, len / 2, 0) in
        let left = Stdlib.List.filter<Int64>(arr, (x: Int64) => x < pivot) in
        let middle = Stdlib.List.filter<Int64>(arr, (x: Int64) => x == pivot) in
        let right = Stdlib.List.filter<Int64>(arr, (x: Int64) => x > pivot) in
        Stdlib.List.append<Int64>(
            Stdlib.List.append<Int64>(quicksort(left), middle),
            quicksort(right))
```

### Rust Implementation
```rust
fn quicksort(arr: Vec<i64>) -> Vec<i64> {
    if arr.len() <= 1 { return arr; }
    let pivot = arr[arr.len() / 2];
    let left: Vec<i64> = arr.iter().filter(|&&x| x < pivot).copied().collect();
    let middle: Vec<i64> = arr.iter().filter(|&&x| x == pivot).copied().collect();
    let right: Vec<i64> = arr.iter().filter(|&&x| x > pivot).copied().collect();
    let mut result = quicksort(left);
    result.extend(middle);
    result.extend(quicksort(right));
    result
}
```

### OCaml Implementation
```ocaml
let rec quicksort arr =
  match arr with
  | [] -> []
  | [x] -> [x]
  | _ ->
    let len = List.length arr in
    let pivot = List.nth arr (len / 2) in
    let left = List.filter (fun x -> x < pivot) arr in
    let middle = List.filter (fun x -> x = pivot) arr in
    let right = List.filter (fun x -> x > pivot) arr in
    quicksort left @ middle @ quicksort right
```

**Key Difference:** All three implementations are functionally similar, but Dark uses a complex FingerTree-based list implementation while OCaml uses simple cons-lists and Rust uses Vecs.

---

## Optimization 1: Replace FingerTree with Simple Cons-List for Sequential Access

### Impact Estimate: ~70-80% reduction in overhead for filter/map operations

### Root Cause
Dark uses a sophisticated FingerTree data structure for lists, which provides O(1) amortized append and O(log n) random access. However, for the quicksort benchmark, the operations are:
- `filter`: Sequential traversal (O(n))
- `append`: Concatenating results
- `head`/`tail`: List decomposition

The FingerTree adds massive overhead for these sequential operations:

**Dark filterHelper ANF:**
```
Function Stdlib.List.__filterHelper_i64:
let TempId 1181 = Stdlib.FingerTree.isEmpty_i64(t1178)
if t1181 then
    return t1180
else
    let TempId 1182 = Stdlib.FingerTree.head_i64(t1178)    ; COMPLEX: tag checks, node traversal
    let TempId 1183 = t1182.0                              ; option check
    let TempId 1184 = t1183 == 1                           ; is None?
    if t1184 then
        return t1180
    else
        let TempId 1185 = t1182.1                          ; extract value
        let TempId 1187 = ClosureCall(t1179, [t1185])      ; call predicate
        let TempId 1188 = Stdlib.FingerTree.pushBack_i64(t1180, t1185)  ; COMPLEX
        let TempId 1189 = if t1187 then t1188 else t1180
        let TempId 1191 = Stdlib.FingerTree.tail_i64(t1178)  ; VERY COMPLEX
        let TempId 1192 = Stdlib.List.__filterHelper_i64(t1191, t1179, t1189)
        return t1192
```

The `tail_i64` function is extremely complex (spans ~400 lines of ANF) because it must handle:
- Single nodes
- Deep nodes with prefix/suffix/middle components
- Rebalancing when prefix is exhausted
- Node explosion and reconstruction

**Dark FingerTree.tail_i64 (simplified):**
```
Function Stdlib.FingerTree.tail_i64:
let TempId 512 = Stdlib.FingerTree.__isNull_i64(t511)
if t512 then return empty
else
    let TempId 514 = Stdlib.FingerTree.__getTag_i64(t511)
    ... 30+ branches for different node configurations ...
    ... recursive calls to __rebuildFrom_i64 ...
    ... calls to __explodeNodeToFront_i64 ...
```

**OCaml filter (simpler cons-list):**
```ocaml
(* Pattern match directly on list structure *)
| [] -> acc
| x :: xs -> if pred x then filter_aux (x :: acc) xs else filter_aux acc xs
```

**Rust filter (iterator):**
```asm
; Tight loop with direct memory access
96e0:   f84086f3    ldr x19, [x23], #8    ; load element
96ec:   eb15027f    cmp x19, x21          ; compare to pivot
96f0:   54ffff8a    b.ge 96e4             ; skip if >= pivot
; ... push to result vector ...
```

### Evidence from Instruction Counts

The IR shows Dark generates:
- 77 functions for FingerTree operations (from `__allocNode2_i64` to `concat_i64`)
- Each `tail` operation involves 10-20 function calls
- Each `pushBack` operation involves 5-10 function calls

For a list of 5000 elements with 3 filter operations per quicksort call:
- Rust: ~3 tight loops, ~15,000 simple iterations
- OCaml: ~3 traversals with cons-cell manipulation
- Dark: ~15,000 complex FingerTree operations, each with multiple function calls

### Implementation Approach

**Option A: Specialize filter for sequential iteration**
- Detect when `filter` is the only consumer of a list
- Use a simple iterator that walks the tree without reconstructing
- Build result directly as contiguous array

**Option B: Add simple cons-list type for functional operations**
- `SimpleList<T>` with head/tail representation
- O(1) cons, head, tail operations
- Convert from FingerTree only when random access needed

**Option C: Inline FingerTree operations for common cases**
- Specialize `tail` for single-element prefix (most common)
- Eliminate tag checks when type is statically known

### Files to Modify
- `src/stdlib/list.dark` - Add optimized filter implementation
- `src/anf/optimizations/` - Add list operation specialization
- Consider: `src/stdlib/simple_list.dark` - Lightweight list type

---

## Optimization 2: Closure Allocation Hoisting

### Impact Estimate: ~15-20% improvement for filter-heavy code

### Root Cause
Dark allocates closures inside the quicksort loop for each filter call:

**Dark quicksort ANF:**
```
Function quicksort:
let TempId 1280 = Stdlib.List.length_i64(t1279)
let TempId 1282 = t1280 <= 1
if t1282 then return t1279
else
    let TempId 1283 = t1280 >> 1
    let TempId 1284 = getAtOrDefault(t1279, t1283, 0)      ; pivot
    let TempId 1286 = ClosureAlloc(__closure_0, [t1284])   ; ALLOC for x < pivot
    let TempId 1287 = Stdlib.List.filter_i64(t1279, t1286)
    let TempId 1289 = ClosureAlloc(__closure_1, [t1284])   ; ALLOC for x == pivot
    let TempId 1290 = Stdlib.List.filter_i64(t1279, t1289)
    let TempId 1292 = ClosureAlloc(__closure_2, [t1284])   ; ALLOC for x > pivot
    let TempId 1293 = Stdlib.List.filter_i64(t1279, t1292)
```

Each `ClosureAlloc` creates a heap object with the captured `pivot` value. For quicksort with average log(n) recursion depth and 5000 elements, this creates ~39,000 closure allocations.

**Closure structures (from ANF):**
```
Function __closure_0:
let TempId 2 = t0.1            ; load pivot from closure environment
let TempId 4 = t1 < t2         ; compare: element < pivot
return t4
```

**Rust approach (inline comparison):**
```asm
96ec:   eb15027f    cmp x19, x21    ; direct register comparison
96f0:   54ffff8a    b.ge 96e4       ; branch
```

Rust keeps the pivot in a register (x21) throughout the filter loop - no closure allocation needed.

### Evidence

The three closure types have trivial bodies:
- `__closure_0`: `x < pivot`
- `__closure_1`: `x == pivot`
- `__closure_2`: `x > pivot`

These could be specialized into inline comparisons rather than indirect calls through closures.

### Implementation Approach

1. **Closure Specialization Pass**
   - Detect closures with single primitive operations
   - Inline the comparison at call sites
   - Eliminate closure allocation entirely

2. **Filter Fusion**
   - Combine multiple filters into single traversal
   - `filter(arr, x < p) + filter(arr, x == p) + filter(arr, x > p)` → `partition3(arr, p)`

### Files to Modify
- `src/anf/optimizations/closure_inline.rs` - Inline trivial closures
- `src/anf/optimizations/filter_fusion.rs` - Combine consecutive filters

---

## Optimization 3: Eliminate Redundant isEmpty/head Checks

### Impact Estimate: ~10-15% improvement

### Root Cause
The filterHelper performs redundant checks:

```
Function Stdlib.List.__filterHelper_i64:
let TempId 1181 = Stdlib.FingerTree.isEmpty_i64(t1178)    ; Check 1: isEmpty
if t1181 then return t1180
else
    let TempId 1182 = Stdlib.FingerTree.head_i64(t1178)   ; Check 2: head returns Option
    let TempId 1183 = t1182.0
    let TempId 1184 = t1183 == 1                          ; Check 3: is None?
```

After confirming `!isEmpty`, the `head` call still returns an Option and checks for None - this is redundant.

**OCaml/Rust approach:**
```ocaml
| [] -> acc                    (* Single pattern match handles both *)
| x :: xs -> ...
```

```rust
for x in arr.iter() { ... }    // Iterator guarantees valid element
```

### Evidence from ANF

The pattern `isEmpty + head + None-check` appears in:
- `filterHelper`
- `reverseHelper`
- `checksumHelper`
- Many other list operations

Each redundant check adds:
- 1 function call to `isEmpty`
- Tag extraction and comparison
- Branch instruction

### Implementation Approach

1. **Introduce `headTail` operation**
   - Returns `Option<(head, tail)>` in single operation
   - Eliminates separate isEmpty check

2. **Pattern match optimization**
   - Recognize `if isEmpty then ... else head/tail` pattern
   - Convert to single destructuring operation

### Files to Modify
- `src/stdlib/finger_tree.dark` - Add headTail operation
- `src/anf/optimizations/pattern_match.rs` - Optimize isEmpty+head pattern

---

## Optimization 4: Fix Allocator Capacity/OOM (Critical Bug)

### Impact Estimate: Required to run benchmark

### Root Cause
Full benchmark quicksort is allocation-heavy enough to exceed the current heap budget. The crash path has been replaced by explicit allocator checks, but the underlying pressure remains.

Current allocator behavior:
- `generateHeapInit` now maps a larger heap (512MB)
- `LIR.HeapAlloc` and `LIR.RawAlloc` now perform bounds checks before bump allocation
- Overflow now terminates with `Out of heap memory` and exit code `1` instead of invalid memory writes

### Evidence
- `benchmarks/problems/quicksort/dark/main.dark` now exits with `Out of heap memory` (exit `1`)
- `benchmarks/problems/quicksort/dark/quick.dark` still succeeds
- Guarded allocator path is active (regression test in `rawptr.e2e` verifies overflow trap)
- Quicksort still does heavy allocation (`filter` x3 + `append` x2 per recursive partition)

### Implementation Approach

1. **Completed: add allocator bounds checks**
   - Before each bump allocation, compute/check `next_ptr <= heap_end`
   - On overflow, terminate with explicit runtime error (`Out of heap memory`)

2. **Completed: increase heap size**
   - mmap heap increased from 128MB to 512MB

3. **Next fix: reduce quicksort allocation pressure**
   - Replace 3-pass partition (`filter`/`filter`/`filter`) with a single-pass partition
   - Prefer array-backed implementation for this benchmark when array primitive exists
   - Consider benchmark-specific input sizing if full-size parity is not required yet

### Files to Modify
- `src/DarkCompiler/passes/6_CodeGen.fs`
  - `generateHeapInit` (heap size / heap-end tracking)
  - `LIR.HeapAlloc` lowering (bounds check)
  - `LIR.RawAlloc` lowering (bounds check)
- `benchmarks/problems/quicksort/dark/main.dark` (single-pass partition rewrite, recommended next step)

---

## Summary of Optimization Opportunities

| Optimization | Est. Impact | Complexity | Priority |
|-------------|-------------|------------|----------|
| 4. Reduce Quicksort Allocation Pressure (after allocator guards) | Required | Medium | Critical |
| 1. Simplify List for Sequential Access | 70-80% | High | High |
| 2. Closure Hoisting/Inlining | 15-20% | Medium | Medium |
| 3. Eliminate Redundant Checks | 10-15% | Low | Medium |

**Combined potential improvement:** Would need fix + optimizations to match OCaml's ~7x Rust performance, then additional work to approach Rust performance.

## Hot Loop Comparison

### Filter Operation (per element)

**Dark (estimated from ANF):**
```
1. FingerTree.isEmpty check (~5 ops)
2. FingerTree.head - tag check, node navigation (~15 ops)
3. Option unpacking (~3 ops)
4. Closure call setup (~5 ops)
5. Comparison (~2 ops)
6. FingerTree.pushBack - alloc, tag, link (~20 ops)
7. FingerTree.tail - rebalancing (~30+ ops)
8. Recursive call setup (~5 ops)
Total: ~85 ops per element
```

**OCaml (from disassembly):**
```asm
54e64:  f940002a    ldr x10, [x1]         ; load cons cell
54e68:  f94003e0    ldr x0, [sp]          ; load arg
54e6c:  d63f0140    blr x10               ; call predicate
; cons cell operations inline, ~15 ops per element
```

**Rust (from disassembly):**
```asm
96e0:   f84086f3    ldr x19, [x23], #8    ; load element (post-increment)
96ec:   eb15027f    cmp x19, x21          ; compare to pivot
96f0:   54ffff8a    b.ge 96e4             ; skip if >= pivot
; ~5-6 ops per element in tight loop
```

### Instruction Ratio Estimate
- Rust: ~6 ops × 5000 elements × 3 filters = ~90,000 ops per partition
- OCaml: ~15 ops × 5000 elements × 3 filters = ~225,000 ops per partition
- Dark: ~85 ops × 5000 elements × 3 filters = ~1,275,000 ops per partition (if working)

Expected Dark/Rust ratio: ~14x (worse than OCaml's 7.32x)

---

## Appendix: IR Dumps

### Dark ANF Key Functions

See `/tmp/dark_ir_dump.txt` for complete IR dumps.

Key function sizes (line counts in ANF):
- `Stdlib.FingerTree.tail_i64`: ~400 lines
- `Stdlib.FingerTree.__pushBackNode_i64`: ~130 lines
- `Stdlib.List.__filterHelper_i64`: ~20 lines
- `quicksort`: ~18 lines

### Assembly Comparison Files

- Rust: `/tmp/rust_disasm.txt` (59,947 lines)
- OCaml: `/tmp/ocaml_disasm.txt` (90,062 lines)
- Dark: Compiles, full benchmark exits with allocator OOM
