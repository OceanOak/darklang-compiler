# Mandelbrot Benchmark Optimization Investigation

## Executive Summary

The Dark compiler produces code that is **1.54x slower than Rust** and **1.10x slower than OCaml** for the mandelbrot benchmark. The primary causes are:

1. **Redundant register moves in the iterate loop** (estimated 20-30% improvement potential)
2. **Repeated float constant loads** (estimated 5-10% improvement potential)
3. **Missing fused multiply-add (FMA) instructions** (estimated 10-15% improvement potential)
4. **Non-optimal phi node resolution** (estimated 5-10% improvement potential)

## Benchmark Results

| Language | Mean Time | vs Rust |
|----------|-----------|---------|
| Rust     | 1.4 ms    | 1.00x   |
| OCaml    | 1.9 ms    | 1.37x   |
| Dark     | 2.1 ms    | 1.54x   |

## Hot Loop Analysis

### Dark Compiler - `iterate` Function

The `iterate` function is the innermost loop, called ~2 million times (200 * 200 * ~50 iterations average).

**LIR after Register Allocation (iterate_L4 - the hot continue path):**
```
Label "iterate_L4":
    D0 <- FSub(D2, D1)
    D1 <- FAdd(D0, D5)
    fv1000 <- FLoad(float[2])        ; PROBLEM: Constant reload
    D0 <- FMul(fv1000, D4)
    D0 <- FMul(D0, D7)
    D4 <- FAdd(D0, D6)
    X2 <- Add(X1, Imm 1)
    D0 <- FMov(D5)                   ; PROBLEM: Dead store
    D0 <- FMov(D6)                   ; PROBLEM: Dead store
    D0 <- FMov(D1)                   ; PROBLEM: Dead store
    D0 <- FMov(D4)                   ; PROBLEM: Dead store
    X1 <- Mov(Reg X2)
    X1 <- Mov(Reg X3)                ; PROBLEM: Overwrites previous store
    D3 <- FMov(D5)
    D2 <- FMov(D6)
    D1 <- FMov(D1)                   ; PROBLEM: Self-move (no-op)
    D0 <- FMov(D4)
    X1 <- Mov(Reg X2)                ; PROBLEM: Redundant (same as earlier)
    X3 <- Mov(Reg X3)                ; PROBLEM: Self-move (no-op)
    D7 <- FMov(D0)
    D4 <- FMov(D1)
    D6 <- FMov(D2)
    D5 <- FMov(D3)
    Jump(Label "iterate_body")
```

**Instruction count in iterate hot path:** ~25 instructions
**Redundant instructions:** ~10 (self-moves, dead stores, constant reloads)
**Effective useful instructions:** ~15

### Rust Compiler - Inlined `mandelbrot` Function

Rust aggressively inlines and optimizes:
```asm
8464:   1e6708f0    fmul  d16, d7, d7           ; zr * zr
8468:   1e6608d1    fmul  d17, d6, d6           ; zi * zi
846c:   1e712a12    fadd  d18, d16, d17         ; zr2 + zi2
8470:   1e622240    fcmp  d18, d2               ; compare with 4.0 (constant in register!)
8474:   54fffe2c    b.gt  8438                  ; escape check
8478:   1e6728e7    fadd  d7, d7, d7            ; 2.0 * zr (strength reduction!)
847c:   1e713a10    fsub  d16, d16, d17         ; zr2 - zi2
8480:   7100056b    subs  w11, w11, #0x1        ; counter decrement
8484:   1e6708c6    fmul  d6, d6, d7            ; zi * (2*zr)
8488:   1e7028a7    fadd  d7, d5, d16           ; new_zr = cr + (zr2 - zi2)
848c:   1e662886    fadd  d6, d4, d6            ; new_zi = ci + zi*(2*zr)
8490:   54fffea1    b.ne  8464                  ; loop
```

**Rust hot loop instruction count:** ~11 instructions
**Key optimizations:**
- `2.0 * zr` optimized to `zr + zr` (strength reduction)
- Float constant 4.0 kept in register `d2`
- No redundant moves
- Counter-based loop (not tail recursion)

### OCaml Compiler - `camlMain__mandelbrot_267`

```asm
4eae8:  1e610825    fmul  d5, d1, d1            ; zr * zr
4eaec:  1f421446    fmadd d6, d2, d2, d5        ; FMADD! zi*zi + zr2
4eaf0:  1e6420c0    fcmp  d6, d4                ; compare
4eaf4:  5400006d    b.le  4eb00                 ; continue if <= 4.0
...
4eb04:  1f429449    fmsub d9, d2, d2, d5        ; FMSUB! zr2 - zi*zi
4eb08:  1e67292a    fadd  d10, d9, d7           ; + cr
4eb0c:  1e60100b    fmov  d11, #2.0             ; constant (could be hoisted)
4eb10:  1e61096c    fmul  d12, d11, d1          ; 2.0 * zr
4eb18:  1f423582    fmadd d2, d12, d2, d13      ; FMADD! new_zi
```

**OCaml key advantage:** Uses FMADD/FMSUB instructions for fused multiply-add

## Optimization Opportunities

### 1. Dead Store Elimination in Phi Resolution (High Impact: ~20-30%)

**Problem:** The register allocator generates redundant moves when resolving phi nodes.

**Evidence (LIR after RegAlloc, iterate_L4):**
```
D0 <- FMov(D5)     ; Dead - immediately overwritten
D0 <- FMov(D6)     ; Dead - immediately overwritten
D0 <- FMov(D1)     ; Dead - immediately overwritten
D0 <- FMov(D4)     ; Dead - immediately overwritten
X1 <- Mov(Reg X2)
X1 <- Mov(Reg X3)  ; Overwrites previous value
```

**Root Cause:** Phi resolution in `iterate_L4` generates moves for variables that will flow to `iterate_body`, but the register allocator doesn't eliminate self-moves or coalesce moves effectively.

**Implementation Approach:**
1. Add a post-regalloc pass to eliminate:
   - Self-moves: `Xn <- Mov(Reg Xn)`
   - Consecutive overwrites to same register
2. Improve phi node parallel copy sequencing

**Files to modify:**
- `src/DarkCompiler/RegAlloc.fs` - Add move coalescing
- `src/DarkCompiler/LIR.fs` - Add dead store elimination pass

### 2. Float Constant Hoisting (Medium Impact: ~5-10%)

**Problem:** Float constants are loaded from memory inside the loop.

**Evidence (LIR iterate_L1 and iterate_L4):**
```
fv1001 <- FLoad(float[4])    ; Loaded every iteration for escape check
...
fv1000 <- FLoad(float[2])    ; Loaded every iteration for 2.0 * zr
```

Rust keeps 4.0 in `d2` register throughout the entire function.

**Implementation Approach:**
1. Identify float constants used in loops
2. Hoist loads to function entry or loop preheader
3. Allocate a dedicated register for frequently-used constants

**Files to modify:**
- `src/DarkCompiler/LIROptimizations.fs` - Add constant hoisting pass
- `src/DarkCompiler/MIRToLIR.fs` - Improve constant handling

### 3. Fused Multiply-Add Instructions (Medium Impact: ~10-15%)

**Problem:** Dark generates separate FMUL+FADD sequences where FMADD would be faster and more accurate.

**Evidence - Dark LIR (countRow_L1):**
```
fv10083 <- FMul(fv1000, fv10081)   ; Separate multiply
fv10084 <- FDiv(fv10083, fv10080)  ; Divide
```

**Evidence - OCaml uses FMADD:**
```asm
4eaec:  1f421446    fmadd d6, d2, d2, d5   ; d6 = d2*d2 + d5 in one instruction
4eb04:  1f429449    fmsub d9, d2, d2, d5   ; d9 = d2*d2 - d5 in one instruction
```

**Pattern to detect:**
```
t1 = a * b
t2 = t1 + c   (or t1 - c)
; Can become: FMADD t2, a, b, c (or FMSUB)
```

**Implementation Approach:**
1. Add pattern matching in LIR optimization pass to detect FMul followed by FAdd/FSub
2. Replace with FMADD/FMSUB when the multiply result is only used by the add
3. Add new LIR instructions: `FMAdd`, `FMSub`

**Files to modify:**
- `src/DarkCompiler/LIR.fs` - Add FMAdd/FMSub instructions
- `src/DarkCompiler/LIROptimizations.fs` - Add fusion pattern matching
- `src/DarkCompiler/CodeGen.fs` - Emit FMADD/FMSUB encodings
- `src/DarkCompiler/ARM64Encoding.fs` - Encode FMA instructions

### 4. Strength Reduction: 2.0 * x → x + x (Low Impact: ~3-5%)

**Problem:** Multiplication by 2.0 uses FMUL when FADD would be faster.

**Evidence - Dark ANF:**
```
let TempId 16 = 2 * t2    ; 2.0 * zr uses FMul
```

**Evidence - Rust uses FADD:**
```asm
8478:   1e6728e7    fadd  d7, d7, d7    ; 2.0 * zr = zr + zr
```

**Implementation Approach:**
1. Pattern match `2.0 * x` or `x * 2.0` in ANF optimization
2. Replace with `x + x`

**Files to modify:**
- `src/DarkCompiler/ANFOptimizations.fs` - Add strength reduction pattern

## Summary of Optimization Impact

| Optimization | Estimated Impact | Complexity |
|-------------|------------------|------------|
| Dead store elimination | 20-30% | Medium |
| Constant hoisting | 5-10% | Low |
| FMA instructions | 10-15% | Medium |
| Strength reduction | 3-5% | Low |
| **Total potential** | **~40-50%** | |

If all optimizations are implemented, Dark could approach or match Rust performance on this benchmark.

## Appendix: Full IR Dumps

### Dark ANF (after optimization)

```
Function iterate:
let TempId 6 = t4 >= t5
if t6 then
  return 1
else
  let TempId 7 = t2 * t2
  let TempId 9 = t3 * t3
  let TempId 11 = t7 + t9
  let TempId 12 = t11 > 4
  if t12 then
    return 0
  else
    let TempId 13 = t7 - t9
    let TempId 14 = t13 + t0
    let TempId 16 = 2 * t2
    let TempId 17 = t16 * t3
    let TempId 18 = t17 + t1
    let TempId 20 = t4 + 1
    TailCall(iterate, [t0, t1, t14, t18, t20, t5])
```

### Dark MIR (iterate function)

```
Function iterate:
  iterate_entry:
    jump iterate_body
  iterate_L0:
    v22 <- 1 : TInt64
    jump iterate_L2
  iterate_L1:
    v7 <- v2 * v2 : TFloat64
    v9 <- v3 * v3 : TFloat64
    v11 <- v7 + v9 : TFloat64
    v12 <- v11 > float[4] : TFloat64
    branch v12 ? iterate_L3 : iterate_L4
  iterate_L2:
    ret v22
  iterate_L3:
    v23 <- 0 : TInt64
    jump iterate_L5
  iterate_L4:
    v13 <- v7 - v9 : TFloat64
    v14 <- v13 + v0 : TFloat64
    v16 <- float[2] * v2 : TFloat64
    v17 <- v16 * v3 : TFloat64
    v18 <- v17 + v1 : TFloat64
    v20 <- v4 + 1 : TInt64
    [phi resolution moves...]
    jump iterate_body
  iterate_L5:
    v22 <- v23 : TInt64
    jump iterate_L2
  iterate_body:
    v6 <- v4 >= v5 : TInt64
    branch v6 ? iterate_L0 : iterate_L1
```
