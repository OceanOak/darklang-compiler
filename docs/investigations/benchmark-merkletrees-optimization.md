# Merkletrees Benchmark Optimization Investigation

## Executive Summary

The Dark compiler generates **1,114,096,152 instructions** for the merkletrees benchmark, which is **9.83x slower** than Rust (113,304,119 instructions) and comparable to OCaml (1,004,581,199 instructions at 8.87x).

This investigation identified **5 optimization opportunities** that could significantly improve performance. The most impactful is a **duplicate function call bug** that causes Dark to call `buildTree` twice per iteration instead of once, effectively doubling the benchmark workload.

## Benchmark Overview

The merkletrees benchmark:
- Builds complete binary Merkle trees of depth 15 (32,768 leaves)
- Runs 50 iterations
- Each iteration builds a tree, verifies it by rebuilding, and accumulates a checksum
- Core hot path: `hashLoop` (8-iteration FNV-1a hash) called from `buildTree`

## Performance Data

| Language | Instructions | vs Rust |
|----------|-------------|---------|
| Rust     | 113,304,119 | 1.00x   |
| OCaml    | 1,004,581,199 | 8.87x |
| **Dark** | **1,114,096,152** | **9.83x** |

## Optimization Opportunities

### 1. CRITICAL: Duplicate buildTree Call in Benchmark Function

**Impact: ~50% reduction expected (eliminates half the work)**

#### Root Cause

After inlining `verifyTree`, the Dark compiler generates **two separate calls** to `buildTree` with identical arguments instead of reusing the first call's result.

#### Evidence from ANF (after RC insertion, lines 262-264):

```
let TempId 42 = buildTree(t37, t39)
let TempId 62 = buildTree(t37, t39)   // <-- DUPLICATE CALL!
let TempId 63 = t62 == t42
```

#### Evidence from MIR (benchmark_L1, lines 516-518):

```
v42 <- Call(buildTree, [v37, v39])
v62 <- Call(buildTree, [v37, v39])   // <-- DUPLICATE CALL!
v63 <- v62 == v42 : TFunction ([TInt64; TInt64], TInt64)
```

#### Evidence from LIR post-regalloc (lines 882-890):

```
ArgMoves(X0 <- Reg X26, X1 <- Reg X24)
X20 <- Call(buildTree, [Reg X26, Reg X24])
...
ArgMoves(X0 <- Reg X26, X1 <- Reg X24)
X19 <- Call(buildTree, [Reg X26, Reg X24])   // <-- DUPLICATE CALL!
```

#### Analysis

The original Dark code:
```dark
let root = buildTree(depth, i) in
let verified = verifyTree(depth, i, root) in
```

Where `verifyTree` is:
```dark
def verifyTree(depth, leafStart, expectedRoot) =
    buildTree(depth, leafStart) == expectedRoot
```

After inlining `verifyTree`, this should become:
```
let root = buildTree(depth, i)
let verified = (buildTree(depth, i) == root)  // Should reuse root!
```

The compiler should recognize that `buildTree(depth, i)` is a pure function and the second call is redundant with `root`. This is a **missing Common Subexpression Elimination (CSE)** optimization.

#### Implementation Approach

1. Add CSE pass in ANF optimization phase
2. For pure function calls, hash the (function, args) tuple and check for existing bindings
3. Replace duplicate calls with reference to existing result

#### Files to Modify
- `src/DarkCompiler/ANFOptimization.fs` - Add CSE pass
- `src/DarkCompiler/ANF.fs` - May need purity annotations

---

### 2. Missing Loop Unrolling for hashLoop

**Impact: ~20-30% reduction in hashLoop execution time**

#### Root Cause

The `hashLoop` function iterates 8 times (fixed count) but Dark compiles it as a recursive tail-call loop, while Rust completely unrolls it into 8 inline XOR/MUL sequences.

#### Evidence from Dark LIR (hashLoop_L1, lines 761-772):

```asm
hashLoop_L1:
  X1 <- And_imm(X5, #255)
  X1 <- Eor(X3, X1)
  X2 <- Mov(Imm 1099511628211)
  X3 <- Mul(X1, Reg X2)
  X2 <- Add(X4, Imm 1)
  X1 <- Mov(Reg X3)
  X1 <- Mov(Reg X5)
  X1 <- Mov(Reg X2)
  X3 <- Mov(Reg X3)
  X5 <- Mov(Reg X5)
  X4 <- Mov(Reg X2)
  Jump(Label "hashLoop_body")
hashLoop_body:
  Cmp(X4, Imm 8)
  CondBranch(GE, Label "hashLoop_L0", Label "hashLoop_L1")
```

Each iteration: ~12 instructions + compare + branch = ~14 instructions
Total for 8 iterations: ~112 instructions per hash

#### Evidence from Rust disassembly (lines 5327-5349):

```asm
; Completely unrolled - no loop, no branches
ca090109   eor x9, x8, x9
9b0a7d29   mul x9, x9, x10
ca080129   eor x9, x9, x8
9b0a7d29   mul x9, x9, x10
ca080129   eor x9, x9, x8
9b0a7d29   mul x9, x9, x10
... (repeated 8 times total)
```

Total: 16 instructions (2 per iteration, no loop overhead)

#### Implementation Approach

1. Add loop unrolling pass that recognizes fixed-count loops
2. Unroll when iteration count is small and known at compile time
3. Alternative: Inline `hashLoop` entirely into callers

#### Files to Modify
- `src/DarkCompiler/ANFOptimization.fs` - Add unrolling heuristics
- `src/DarkCompiler/ANFInlining.fs` - Consider aggressive inlining for small recursive functions

---

### 3. Redundant Register Moves in hashLoop

**Impact: ~10% reduction in hashLoop execution time**

#### Root Cause

The register allocator produces redundant MOV instructions where the same register is written multiple times without reading the intermediate value.

#### Evidence from LIR post-regalloc (hashLoop_L1, lines 766-771):

```asm
X1 <- Mov(Reg X3)   // Dead write - X1 overwritten below
X1 <- Mov(Reg X5)   // Dead write - X1 overwritten below
X1 <- Mov(Reg X2)   // Dead write - X1 never read before next write
X3 <- Mov(Reg X3)   // Useless self-move
X5 <- Mov(Reg X5)   // Useless self-move
X4 <- Mov(Reg X2)
```

#### Evidence from generated assembly (offset 0x1f0-0x200):

```asm
1f0: aa0303e1   mov x1, x3   // Dead
1f4: aa0503e1   mov x1, x5   // Dead
1f8: aa0203e1   mov x1, x2   // Dead
1fc: aa0203e4   mov x4, x2   // This one is used
```

#### Implementation Approach

1. Add dead store elimination after register allocation
2. Remove self-moves (X <- X)
3. Remove writes to registers that are overwritten before being read

#### Files to Modify
- `src/DarkCompiler/LIROptimization.fs` - Add post-regalloc dead store elimination
- `src/DarkCompiler/RegisterAllocation.fs` - Consider coalescing improvements

---

### 4. Missing CSE for (depth - 1) in buildTree

**Impact: ~5% reduction in buildTree execution time**

#### Root Cause

The expression `depth - 1` is computed 3 times in `buildTree` instead of once.

#### Evidence from ANF (buildTree, lines 168-172):

```
let TempId 21 = t17 - 1   // First computation
let TempId 22 = 1 << t21
let TempId 24 = t17 - 1   // Duplicate!
let TempId 25 = buildTree(t24, t18)
let TempId 27 = t17 - 1   // Duplicate!
```

#### Implementation Approach

Same as Optimization #1 - CSE pass will catch this.

#### Files to Modify
- `src/DarkCompiler/ANFOptimization.fs`

---

### 5. Function Call Overhead for hashLoop

**Impact: ~15-20% reduction (combined with unrolling)**

#### Root Cause

Dark makes actual function calls to `hashLoop` from `buildTree`, incurring:
- Stack frame setup/teardown
- Register save/restore
- Branch prediction overhead

Rust inlines `hash()` into `build_tree()` completely.

#### Evidence from Dark LIR (buildTree_L0, lines 619-626):

```
SaveRegs([], [])
ArgMoves(X0 <- Imm -3750763034362895579, X1 <- Reg v18, X2 <- Imm 0)
v10077 <- Call(hashLoop, [Imm -3750763034362895579, Reg v18, Imm 0])
RestoreRegs([], [])
```

#### Evidence from Rust disassembly (build_tree):

The hash code is completely inlined - no `call` instructions to a separate hash function.

#### Implementation Approach

1. Mark `hashLoop` as an inlining candidate
2. Inline small recursive functions with known bounds
3. After inlining, loop unrolling can eliminate the recursion entirely

#### Files to Modify
- `src/DarkCompiler/ANFInlining.fs` - Increase inlining threshold for recursive functions with small bodies

---

## Comparison with Rust

### Rust build_tree (lines 5306-5350):

```asm
; Function is ~100 instructions
; Hash loop is completely unrolled inline
; No function calls except recursive build_tree calls
; Zero-cost abstraction - hash_pair and hash are both inlined
```

### Dark buildTree:

```asm
; Function makes 3 function calls per invocation:
;   - 2x buildTree (recursive)
;   - 1x hashLoop (should be inlined)
; Plus duplicate buildTree call in benchmark
```

## Recommended Priority

1. **Fix duplicate buildTree call (CSE)** - ~50% improvement, clear bug
2. **Inline hashLoop into buildTree** - ~20% improvement, straightforward
3. **Loop unrolling for hashLoop** - ~20% improvement when combined with inlining
4. **Dead store elimination** - ~10% improvement
5. **Local CSE for depth-1** - ~5% improvement (subsumed by #1)

## Estimated Combined Impact

With all optimizations implemented, Dark could potentially achieve:
- Current: 1,114,096,152 instructions (9.83x vs Rust)
- Target: ~300-400M instructions (~3-4x vs Rust)

This would make Dark competitive with OCaml and closer to Rust performance on this benchmark.
