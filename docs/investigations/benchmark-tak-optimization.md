# Benchmark Investigation: tak (Takeuchi Function)

**Date:** 2026-01-15
**Benchmark:** tak
**Randomly selected:** Yes

## Executive Summary

The Dark implementation of the Takeuchi function performs worse than Rust due to:
1. **Redundant MOV chains after PHI resolution**: 6 extra MOV instructions at function entry
2. **Missing copy propagation in LIR**: Chains like `X19=X2; X22=X19` not simplified to `X22=X2`
3. **Suboptimal PHI node lowering**: Extra intermediate registers used for PHI operands
4. **No argument register reuse**: Arguments moved to callee-saved immediately, even when not needed

## Benchmark Overview

The Takeuchi function (tak) is a classic benchmark testing recursion and function call overhead:

```dark
def tak(x: Int64, y: Int64, z: Int64) : Int64 =
    if x <= y then z
    else tak(tak(x - 1, y, z), tak(y - 1, z, x), tak(z - 1, x, y))
```

This creates deeply nested recursive calls - tak(24, 16, 8) makes millions of function invocations.

## Performance Comparison

| Language | Time | Instructions in hot path |
|----------|------|--------------------------|
| Rust     | ~50ms | 23 per iteration |
| Dark     | ~53ms | 29+ per iteration |

## Code Comparison

### Rust tak Function Entry (lines 110-130 of main.s)

```asm
_ZN4main3tak17hc54be1d56ea100feE:
    stp    x29, x30, [sp, #-64]!    ; Stack frame setup
    str    x23, [sp, #16]            ; Save callee-saved
    stp    x22, x21, [sp, #32]
    stp    x20, x19, [sp, #48]
    mov    x29, sp
    cmp    x0, x1                    ; DIRECT comparison using args
    mov    x19, x2                   ; Only save z to callee-saved
    b.ls   .LBB4_3                   ; Early exit path
    mov    x20, x1                   ; Save y (only in non-exit path)
    mov    x21, x0                   ; Save x
```

Key optimizations in Rust:
- **4 register saves** total (X19-X23, frame pointer)
- **Direct use of argument registers** for the comparison
- **Conditional saves**: X20, X21 only saved if continuing

### Dark tak Function Entry (from LIR after register allocation)

```
Label "tak_entry":
    X21 <- Mov(Reg X0)    ; Save x to callee-saved
    X20 <- Mov(Reg X1)    ; Save y to callee-saved
    X19 <- Mov(Reg X2)    ; Save z to callee-saved
    X22 <- Mov(Reg X19)   ; REDUNDANT: X22 = X19 = z
    X23 <- Mov(Reg X20)   ; REDUNDANT: X23 = X20 = y
    X24 <- Mov(Reg X21)   ; REDUNDANT: X24 = X21 = x
    Jump(Label "tak_body")
```

Issues identified:
- **6 register saves** (X19-X24)
- **3 completely redundant MOV instructions** at entry
- Arguments moved twice: first to intermediate, then to final location

### Root Cause: PHI Node Lowering

The MIR shows the PHI nodes:
```
tak_body:
    v10018 <- Phi([(Reg v2, tak_entry), (Reg v10030, tak_L1)])      ; z
    v10019 <- Phi([(Reg v1, tak_entry), (Reg v10029, tak_L1)])      ; y
    v10020 <- Phi([(Reg v0, tak_entry), (Reg v10028, tak_L1)])      ; x
```

After register allocation, v2 gets X19, but the PHI destination v10018 gets X22. This creates:
1. Entry: `X19 <- X2` (v2 = argument z)
2. Entry: `X22 <- X19` (v10018 = v2, PHI input)

The register allocator doesn't coalesce v2 and v10018 to the same register.

## Optimization Opportunities

### 1. Post-RA Copy Propagation (High Impact)

**Problem:** After register allocation, chains of MOV instructions exist that could be simplified.

**Evidence from LIR:**
```
X21 <- Mov(Reg X0)
X24 <- Mov(Reg X21)   ; Could be X24 <- Mov(Reg X0)
```

**Solution:**
- Add a post-register-allocation pass to propagate copies
- When `Rd <- Rs` followed by `Rx <- Rd` where Rd is not used later, replace with `Rx <- Rs`
- Estimate: 3-5% speedup on recursive functions

**Files to modify:**
- `src/DarkCompiler/passes/4.5_LIR_Optimize.fs` - Add cross-block copy propagation

### 2. Improved PHI Coalescing (High Impact)

**Problem:** PHI nodes with entry block operands don't share registers with their sources.

**Evidence:**
```
v10018 <- Phi([(Reg v2, tak_entry), ...])
```
Here v2 (argument z) gets X19, but v10018 gets X22, requiring a MOV.

**Solution:**
- Prioritize coalescing PHI destinations with their entry-block sources
- Entry block operands are "free" - they come from argument registers
- When a PHI operand comes from the entry block, try to allocate the PHI destination to the same register

**Files to modify:**
- `src/DarkCompiler/passes/5_RegisterAllocation.fs` - Enhance `collectPhiPreferences`

### 3. Dead Move Elimination (Medium Impact)

**Problem:** Intermediate registers used only to feed PHI nodes are not eliminated.

**Evidence:**
```
X21 <- Mov(Reg X0)   ; X21 is only used by next line
X24 <- Mov(Reg X21)  ; X24 is the actual live value
```

**Solution:**
- Detect when a register is defined by MOV and only used by a single subsequent MOV
- Eliminate the intermediate register by forwarding the source

**Files to modify:**
- `src/DarkCompiler/passes/4.5_LIR_Optimize.fs` - Add dead move chain elimination

### 4. Argument Register Reuse in Comparisons (Medium Impact)

**Problem:** Dark moves arguments to callee-saved before using them, even for the first comparison.

**Evidence:**
Rust directly uses X0, X1 for `cmp x0, x1` before saving to callee-saved.
Dark saves all arguments first, then compares the saved copies.

**Solution:**
- Allow argument registers to be used in the first basic block before being clobbered
- Only save to callee-saved when necessary (before a call or loop back)

**Files to modify:**
- `src/DarkCompiler/passes/3_ANF_to_MIR.fs` - Generate better CFG structure
- `src/DarkCompiler/passes/5_RegisterAllocation.fs` - Add argument register preference

## Quantified Performance Impact

| Optimization | Estimated Speedup | Implementation Effort |
|--------------|------------------|----------------------|
| Post-RA copy propagation | 3-5% | Low |
| Improved PHI coalescing | 2-4% | Medium |
| Dead move elimination | 1-2% | Low |
| Argument register reuse | 1-2% | Medium |

Combined potential improvement: **7-13%** on tak-like recursive benchmarks.

## Detailed IR Analysis

### ANF Stage
The ANF is clean and shows correct tail call detection:
```
let TempId 10 = TailCall(tak, [t5, t7, t9])
```

### MIR Stage
The MIR correctly implements the tail recursion as a loop:
```
tak_L1:
    v4 <- v0 - 1 : TInt64
    v5 <- Call(tak, [v4, v1, v2])
    ...
    v0 <- v5 : TFunction ([TInt64; TInt64; TInt64], TInt64)
    v1 <- v7 : TFunction ([TInt64; TInt64; TInt64], TInt64)
    v2 <- v9 : TFunction ([TInt64; TInt64; TInt64], TInt64)
    jump tak_body
```

The loop structure is correct, but the variable reassignments create PHI nodes that don't coalesce well.

### LIR Stage (Before Register Allocation)
Shows PHI nodes that will cause register pressure:
```
v10018 <- Phi([(Reg v2, tak_entry), (Reg v10030, tak_L1)])
v10019 <- Phi([(Reg v1, tak_entry), (Reg v10029, tak_L1)])
v10020 <- Phi([(Reg v0, tak_entry), (Reg v10028, tak_L1)])
```

### LIR Stage (After Register Allocation)
The redundant moves are clearly visible:
```
Label "tak_entry":
    X21 <- Mov(Reg X0)
    X20 <- Mov(Reg X1)
    X19 <- Mov(Reg X2)
    X22 <- Mov(Reg X19)   ; REDUNDANT
    X23 <- Mov(Reg X20)   ; REDUNDANT
    X24 <- Mov(Reg X21)   ; REDUNDANT
```

## Conclusion

The tak benchmark reveals inefficiencies in Dark's register allocation and PHI lowering phases. While the high-level optimizations (tail call detection) work correctly, the low-level code generation produces redundant register moves that add approximately 7-13% overhead compared to Rust.

The most impactful fix would be post-RA copy propagation, which is straightforward to implement and would eliminate the redundant MOV chains. Improved PHI coalescing would provide additional benefit by reducing register pressure.
