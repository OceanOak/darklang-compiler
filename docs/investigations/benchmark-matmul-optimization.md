# Benchmark Investigation: matmul (Matrix Multiplication)

**Date:** 2026-01-14
**Benchmark:** matmul
**Randomly selected:** Yes

## Executive Summary

The Dark implementation of matrix multiplication performs significantly worse than Rust and OCaml due to:
1. **Algorithmic overhead**: Dark uses FingerTree-based lists with O(log n) access vs O(1) array access
2. **Missing constant folding**: Tag comparison functions return constants but are called as functions
3. **Excessive heap allocation**: Option type wrappers allocated on every `getAt` call
4. **No loop unrolling or vectorization**: Rust uses `madd` (multiply-add) instructions

## Benchmark Comparison

### Source Code Differences

| Language | Matrix Size | Data Structure | Access Complexity |
|----------|-------------|----------------|-------------------|
| Rust     | 100x100     | Vec<Vec<i64>>  | O(1)              |
| OCaml    | 100x100     | Array          | O(1)              |
| Dark     | 3x3         | List (FingerTree) | O(log n)       |

**Critical Finding:** The Dark benchmark uses hardcoded 3x3 matrices instead of 100x100, with comment noting "nested list bugs". Even comparing the same algorithm, Dark's O(log n) access is fundamentally slower.

### Inner Loop Analysis

**Rust Inner Loop (10 instructions/iteration):**
```asm
990c: ldr  x1, [x4]           ; bounds check
9928: ldr  x1, [x15, #8]      ; load row pointer
9934: ldr  x1, [x1, x3, lsl #3] ; a[i][k] - direct indexed access
9938: ldr  x5, [x5, x13, lsl #3]; b[k][j] - direct indexed access
9940: add  x3, x3, #1         ; k++
9944: cmp  x3, #0x64          ; k < 100
9948: madd x17, x5, x1, x17   ; s += a[i][k] * b[k][j] (fused multiply-add)
994c: b.ne 990c               ; loop back
```

**Dark `matGet` function (per element access):**
```
matGet_body:
  Call Stdlib.FingerTree.getAt_list_i64  ; O(log n) tree traversal
  HeapLoad v11320, 8                     ; extract option payload
  Call Stdlib.FingerTree.getAt_i64       ; another O(log n) traversal
  HeapLoad v11323, 0                     ; check option discriminant
  Cmp v11325, Imm 0                      ; is Some?
  Branch...
```

Each element access in Dark involves:
- 2 function calls to `FingerTree.getAt` (O(log n) each)
- Multiple heap allocations for Option wrappers
- Tag checks and branches

## Optimization Opportunities

### 1. Inline Constant Tag Functions (High Impact)

**Problem:** Functions like `__TAG_SINGLE`, `__TAG_DEEP`, `__TAG_LEAF` return constant values but are called via full function call overhead.

**Evidence from LIR:**
```
Stdlib.FingerTree.__TAG_SINGLE:
  Label "Stdlib.FingerTree.__TAG_SINGLE_body":
    X0 <- Mov(Imm 1)
    Ret

Stdlib.FingerTree.__TAG_DEEP:
  Label "Stdlib.FingerTree.__TAG_DEEP_body":
    X0 <- Mov(Imm 2)
    Ret
```

These functions are called repeatedly:
```
v11330 <- Call(Stdlib.FingerTree.__TAG_SINGLE, [])
Cmp(v11328, Reg v11330)
```

**Solution:**
- Mark these functions as `@inline` or `@const`
- Constant propagation should replace `Call(__TAG_SINGLE)` with `Imm 1`
- Estimate: 5-10% speedup on list-heavy code

### 2. Eliminate Option Heap Allocation in getAt (High Impact)

**Problem:** Every `getAt` call allocates a 16-byte Option wrapper on the heap:

```
v11356 <- HeapAlloc(16)
HeapStore(v11356, 0, Imm 0)      ; discriminant = Some (0)
HeapStore(v11356, 8, Reg v11355) ; payload
```

**Evidence:** The `getAt_i64` function has 6 `HeapAlloc(16)` calls for Option construction.

**Solution:**
- Use tagged pointers for Option<Int64> (null = None, non-null = Some)
- Or use register pairs (discriminant, value) for small return types
- Or implement unboxed option optimization
- Estimate: 10-20% speedup on code using Option types

### 3. Add Array Type with O(1) Access (Critical Impact)

**Problem:** Dark lacks native array support, forcing use of FingerTree-based Lists.

**Evidence from ANF:**
```
Function Stdlib.FingerTree.getAt_i64:
  ; 17 labels, multiple nested function calls
  ; O(log n) tree traversal for each access
```

**Rust equivalent:**
```asm
ldr x1, [x1, x3, lsl #3]  ; Single instruction, O(1)
```

**Solution:**
- Add `Array<T>` type with contiguous memory layout
- Implement `Array.get(arr, idx)` as single indexed load
- Estimate: 10-100x speedup for array-heavy benchmarks

### 4. Specialize Small List Access Patterns (Medium Impact)

**Problem:** `getAt` for small lists (size < 4) goes through full FingerTree dispatch.

**Evidence:**
```
Stdlib.FingerTree.getAt_i64:
  Cmp(v551, Imm 0)           ; negative index check
  Branch...
  And_imm(v550, #7)          ; extract tag
  Call(__TAG_SINGLE)         ; compare to SINGLE
  Call(__TAG_DEEP)           ; compare to DEEP
  ; many more branches
```

For a 3-element list, this is massive overhead.

**Solution:**
- Specialize getAt for lists known to be small at compile time
- Pattern-match on list literals to generate direct accesses
- Estimate: 2-5x speedup for small list access

### 5. Implement Loop-Invariant Code Motion (Medium Impact)

**Problem:** In the `dot3` function, same matrix references passed 6 times to `matGet`:

```
v10610 <- Call(matGet, [Reg v596, Reg v598, Imm 0])  ; a, i, 0
v10611 <- Call(matGet, [Reg v597, Imm 0, Reg v599])  ; b, 0, j
v10613 <- Call(matGet, [Reg v596, Reg v598, Imm 1])  ; a, i, 1 - same row!
```

**Solution:**
- Hoist row lookup outside of column iteration
- Cache intermediate FingerTree nodes
- Estimate: 1.5-2x speedup in matrix-like access patterns

## IR Analysis Details

### ANF Stage (after optimization)

Functions show reasonable structure but no inlining occurred:
- `matGet` calls `Stdlib.List.getAt_list_i64` then `Stdlib.List.getAt_i64`
- No constant propagation for known-small lists
- Option type fully constructed even when immediately destructured

### MIR Stage

Control flow graphs generated but:
- No loop detection (no loops in 3x3 unrolled version)
- No vectorization opportunities identified

### LIR Stage (after register allocation)

Heavy stack spilling visible in complex functions:
```
Store(Stack -416, X11)
Store(Stack -424, X11)
Store(Stack -360, X11)
...
```

Register pressure high due to many live values through call sites.

## Quantified Performance Impact

| Optimization | Estimated Speedup | Implementation Effort |
|--------------|------------------|----------------------|
| Array type   | 10-100x          | High                 |
| Inline constants | 5-10%        | Low                  |
| Unboxed Option | 10-20%         | Medium               |
| Small list specialization | 2-5x | Medium             |
| LICM for matrix access | 1.5-2x | Low                |

## Files to Modify

1. **Constant inlining:**
   - `src/anf_optimization.rs` - Add constant function detection
   - `src/stdlib/finger_tree.rs` - Mark tag functions as const

2. **Unboxed Option:**
   - `src/types.rs` - Add unboxed representation
   - `src/lir_codegen.rs` - Generate inline Option handling

3. **Array type:**
   - `src/parser.rs` - New Array syntax
   - `src/types.rs` - Array type definition
   - `src/stdlib/array.rs` - Array operations
   - `src/lir_codegen.rs` - O(1) array access codegen

4. **Small list specialization:**
   - `src/anf_optimization.rs` - List size tracking
   - `src/stdlib/list.rs` - Specialized small list ops

## Conclusion

The Dark compiler's performance gap on matmul stems from fundamental data structure choices (FingerTree vs Array) combined with missing micro-optimizations (constant inlining, unboxed options). Adding an Array type would provide the largest improvement, while the other optimizations would compound to improve performance across all list-using code.
