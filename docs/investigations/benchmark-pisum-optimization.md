# Pisum Benchmark Optimization Investigation

## Summary

The pisum benchmark computes the partial sum of the series 1/k² from k=1 to n, which converges to π²/6. It runs this sum 500 times with n=10000.

**Performance Results:**
- Rust: ~3ms per run
- OCaml: ~4ms per run
- Dark: ~9ms per run
- **Dark is ~3x slower than Rust, ~2.25x slower than OCaml**

## Benchmark Source Code

### Dark (`benchmarks/problems/pisum/dark/main.dark`)
```dark
def innerSum(k: Int64, n: Int64, acc: Float) : Float =
    if k > n then
        acc
    else
        let kf = Stdlib.Int64.toFloat(k) in
        innerSum(k + 1, n, acc + 1.0 / (kf * kf))

def pisum(rounds: Int64, n: Int64, lastResult: Float) : Float =
    if rounds <= 0 then
        lastResult
    else
        pisum(rounds - 1, n, innerSum(1, n, 0.0))

Stdlib.Float.toInt(pisum(500, 10000, 0.0) * 1000000000000.0)
```

### Rust (`benchmarks/problems/pisum/rust/main.rs`)
```rust
fn pisum(n: i64) -> f64 {
    let mut s: f64 = 0.0;
    for _ in 0..500 {
        s = 0.0;
        for k in 1..=n {
            s += 1.0 / ((k * k) as f64);
        }
    }
    s
}

fn main() {
    let result = pisum(10000);
    println!("{}", (result * 1000000000000.0) as i64);
}
```

### OCaml (`benchmarks/problems/pisum/ocaml/main.ml`)
```ocaml
let pisum n =
  let s = ref 0.0 in
  for _ = 0 to 499 do
    s := 0.0;
    for k = 1 to n do
      s := !s +. 1.0 /. float_of_int (k * k)
    done
  done;
  !s
```

## Analysis

### Rust Hot Loop (7-8 instructions)

LLVM generates a highly optimized inner loop:

```asm
; Inner loop at 0x8420-0x8444
8420:   movi    d0, #0x0            ; s = 0.0 (at start of outer loop)
8424:   mov     w10, #0x1           ; k = 1
8424:   mul     x11, x10, x10       ; k * k (INTEGER multiply - fast!)
8428:   cmp     x10, x9             ; compare k with n
842c:   cinc    x10, x10, ne        ; k++ (conditional increment)
8430:   scvtf   d2, x11             ; convert k*k to float
8434:   fdiv    d2, d1, d2          ; 1.0 / (k*k) - d1 pre-loaded with 1.0!
8438:   fadd    d0, d0, d2          ; s += result
843c:   b.eq    8410                ; if k == n, done
8440:   cmp     x10, x9
8444:   b.le    8424                ; continue loop
```

**Key observations:**
- 1.0 constant is pre-loaded into D1 before the loop (`fmov d1, #1.0` at 0x8404)
- Integer multiply `mul x11, x10, x10` for k*k (1 cycle on ARM64)
- Single conversion `scvtf` after integer multiply
- Tight loop with minimal register pressure

### OCaml Hot Loop (~12 instructions)

OCaml's native code is also well-optimized:

```asm
; Inner loop at 0x50278-0x502a8
50278:  orr     x6, xzr, #0x1       ; constant 1
5027c:  asr     x7, x3, #1          ; k >> 1 (tagged integer adjustment)
50280:  sub     x8, x3, #0x1        ; k - 1
50284:  madd    x9, x8, x7, x6      ; k * k (integer, using tagged int math)
50288:  asr     x10, x9, #1         ; adjust tagged result
5028c:  scvtf   d3, x10             ; convert to float
50290:  fmov    d4, #1.0            ; load 1.0 (unfortunately inside loop)
50294:  fdiv    d5, d4, d3          ; 1.0 / (k*k)
50298:  fadd    d0, d0, d5          ; accumulate
5029c:  mov     x11, x3             ; save k
502a0:  add     x3, x3, #0x2        ; k++ (tagged: +2)
502a4:  cmp     x11, x0             ; compare k with n
502a8:  b.eq    502bc               ; done?
```

**Key observations:**
- Uses tagged integer representation (adds overhead)
- Integer multiply for k*k via `madd` instruction
- GC polling adds some overhead (`ldr x16, [x28]`)
- 1.0 loaded inside loop (suboptimal, same issue as Dark)

### Dark Hot Loop (After Register Allocation)

```
innerSum:
  Label "innerSum_L1":
    D0 <- IntToFloat(X1)         ; Convert k to float FIRST
    X2 <- Add(X1, Imm 1)         ; k + 1
    D0 <- FMul(D0, D0)           ; kf * kf (FLOAT multiply!)
    fv1000 <- FLoad(float[1])    ; Load 1.0 EVERY ITERATION!
    D0 <- FDiv(fv1000, D0)       ; 1.0 / (kf * kf)
    D1 <- FAdd(D1, D0)           ; acc + result
    X1 <- Mov(Reg X2)            ; Redundant move
    X1 <- Mov(Reg X3)            ; Immediately overwritten!
    D0 <- FMov(D1)               ; Redundant FMov
    X1 <- Mov(Reg X2)            ; Duplicate of earlier move
    X3 <- Mov(Reg X3)            ; Self-move! No-op!
    D0 <- FMov(D1)               ; Duplicate FMov
    D1 <- FMov(D0)               ; Ping-pong! D0->D1 after D1->D0
    Jump(Label "innerSum_body")

  Label "innerSum_body":
    Cmp(X1, Reg X3)
    CondBranch(GT, Label "innerSum_L0", Label "innerSum_L1")
```

**Critical issues identified:**

1. **Float multiply vs Integer** (lines 359-361): Dark converts k to float first, then multiplies as floats. Rust does integer multiply then converts. Float multiply has higher latency.

2. **FLoad inside loop** (line 362): The constant 1.0 is loaded from memory EVERY iteration. Rust pre-loads this into D1 before the loop.

3. **Redundant register moves** (lines 365-372): Multiple redundant moves including self-moves and ping-pong patterns.

4. **Function call overhead**: Dark keeps `innerSum` as a separate function while Rust/OCaml inline everything.

## Identified Optimization Opportunities

### 1. Hoist Constant Loads Out of Loops (Loop-Invariant Code Motion)

**Impact: ~15-20% performance improvement (estimated)**

**Root Cause:**
The `FLoad(float[1])` instruction loads the constant 1.0 from memory on every loop iteration. This is a memory access in the hottest part of the code.

**Evidence from Dark LIR:**
```
Label "innerSum_L1":
    ...
    fv1000 <- FLoad(float[1])    ; INSIDE LOOP - loads every iteration!
    D0 <- FDiv(fv1000, D0)
```

**Evidence from Rust (constant hoisted):**
```asm
8404:   fmov    d1, #1.000000000000000000e+00  ; BEFORE loop
; ... inner loop ...
8434:   fdiv    d2, d1, d2                     ; d1 already loaded
```

**Implementation Approach:**
1. Implement Loop-Invariant Code Motion (LICM) pass at MIR or LIR level
2. Identify instructions whose operands don't change within a loop
3. Move such instructions to the loop preheader
4. For this case: detect `FLoad` of constants and move to function entry

**Files to Modify:**
- Add new pass `src/DarkCompiler/passes/3.6_LICM.fs` (Loop-Invariant Code Motion)
- Or modify `src/DarkCompiler/passes/4_MIR_to_LIR.fs` to pre-load constants

---

### 2. Integer Multiply Before Float Conversion

**Impact: ~10-15% performance improvement (estimated)**

**Root Cause:**
Dark converts the loop variable k to float first, then performs float multiplication:
```
kf = Int64.toFloat(k)
kf * kf   ; Float multiply
```

Rust computes the product as integers first, then converts once:
```
k * k     ; Integer multiply (faster!)
float(k * k)
```

Integer multiply is typically 1-3 cycles on ARM64, while float multiply is 3-5 cycles.

**Evidence from Dark ANF:**
```
let TempId 4 = IntToFloat(t0)    ; Convert k to float
let TempId 7 = t4 * t4           ; Float multiply kf * kf
```

**Evidence from Rust:**
```asm
8424:   mul     x11, x10, x10    ; Integer multiply k * k
8430:   scvtf   d2, x11          ; Convert result to float (once!)
```

**Implementation Approach:**
This is a classic strength reduction / algebraic optimization:
1. At ANF level, detect pattern: `IntToFloat(x) * IntToFloat(x)` or `IntToFloat(x) * IntToFloat(y)`
2. Transform to: `IntToFloat(x * y)` (single conversion instead of two)
3. For squaring: detect `let y = IntToFloat(x) in y * y` and transform to `IntToFloat(x * x)`

**Files to Modify:**
- `src/DarkCompiler/passes/2.3_ANFOptimization.fs` - Add algebraic optimization rule
- Or add specialized pattern matching in `src/DarkCompiler/passes/3_ANF_to_MIR.fs`

---

### 3. Eliminate Redundant Register Moves (Post-RA Cleanup)

**Impact: ~10-15% performance improvement (estimated)**

**Root Cause:**
The register allocator generates many redundant moves that waste cycles:

```
X1 <- Mov(Reg X2)            ; Set X1
X1 <- Mov(Reg X3)            ; Immediately overwritten! First move was useless
D0 <- FMov(D1)
X1 <- Mov(Reg X2)            ; Duplicate
X3 <- Mov(Reg X3)            ; SELF-MOVE - complete no-op!
D0 <- FMov(D1)               ; Duplicate
D1 <- FMov(D0)               ; Ping-pong back!
```

**Evidence from Dark LIR (After Register Allocation):**
Lines 365-372 show:
- 6 redundant moves in the inner loop
- Including self-moves (`X3 <- Mov(Reg X3)`)
- And ping-pong patterns (`D0 <- FMov(D1)` followed by `D1 <- FMov(D0)`)

**Implementation Approach:**
1. Add post-register-allocation cleanup pass
2. Eliminate self-moves: `X <- Mov(Reg X)` → delete
3. Eliminate redundant moves: `X <- Mov(A); X <- Mov(B)` → keep only second
4. Detect and simplify ping-pong: `D0 <- FMov(D1); D1 <- FMov(D0)` → `D0 <- FMov(D1)` if D1 not needed later

**Files to Modify:**
- `src/DarkCompiler/passes/5_RegisterAllocation.fs` - Add post-RA cleanup
- Or `src/DarkCompiler/passes/6_CodeGen.fs` - Filter during emission

---

### 4. Inline Inner Recursive Functions

**Impact: ~15-20% performance improvement (estimated)**

**Root Cause:**
Dark compiles `innerSum` as a separate function that `pisum` calls. Rust and OCaml inline the inner loop entirely, eliminating call overhead.

**Evidence from Dark:**
```
Function pisum:
  pisum_L1:
    v16 <- Call(innerSum, [1, v12, float[0]])  ; FUNCTION CALL
```

**Evidence from Rust:**
The entire computation is in one function - no calls in the hot path:
```asm
; Everything inlined into single function at 0x83ec
; Inner loop is just a branch, not a call
```

**Implementation Approach:**
1. Enhance inlining heuristics to inline tail-recursive helper functions
2. Detect pattern: function A calls function B in tail position, B is small and recursive
3. Inline B into A, converting B's tail recursion to a loop
4. Current inlining pass may have too conservative size limits

**Files to Modify:**
- `src/DarkCompiler/passes/2.4_Inlining.fs` - Adjust inlining thresholds for tail-recursive helpers
- Consider special case: always inline tail-recursive functions called from non-recursive context

---

## Summary of Expected Improvements

| Optimization | Estimated Impact | Complexity |
|--------------|-----------------|------------|
| Hoist Constant Loads (LICM) | 15-20% | Medium |
| Integer Multiply Before Convert | 10-15% | Low-Medium |
| Eliminate Redundant Moves | 10-15% | Low |
| Inline Inner Functions | 15-20% | Medium |

**Total estimated improvement: ~2-3x faster** (bringing Dark to parity with Rust/OCaml)

## Recommended Implementation Order

1. **Eliminate Redundant Moves** - Quick win, low complexity, benefits all benchmarks
2. **Integer Multiply Before Convert** - Targeted optimization for this pattern, moderate impact
3. **Hoist Constant Loads (LICM)** - Important general optimization, benefits many programs
4. **Inline Inner Functions** - Larger scope change, but significant impact

## Instruction Count Comparison (Inner Loop)

| Compiler | Instructions in Hot Loop | Notes |
|----------|-------------------------|-------|
| Rust | 7-8 | Tight, pre-loaded constants |
| OCaml | ~12 | Tagged integers add overhead |
| Dark | ~15+ | Redundant moves, FLoad in loop |

Dark's inner loop has nearly 2x the instructions of Rust's, directly explaining the 3x slowdown (combined with worse instruction choices like float multiply instead of integer).

## Appendix: Full IR Dumps

### Dark ANF (after optimization)
```
Function innerSum:
let TempId 3 = t0 > t1
if t3 then
return t2
else
let TempId 4 = IntToFloat(t0)
let TempId 6 = t0 + 1
let TempId 7 = t4 * t4
let TempId 8 = 1 / t7
let TempId 9 = t2 + t8
let TempId 10 = innerSum(t6, t1, t9)
return t10

Function pisum:
let TempId 14 = t11 <= 0
if t14 then
return t13
else
let TempId 15 = t11 - 1
let TempId 16 = innerSum(1, t12, 0)
let TempId 17 = pisum(t15, t12, t16)
return t17
```

### Dark MIR (Control Flow Graph)
```
Function innerSum:
  innerSum_entry:
    jump innerSum_body
  innerSum_L0:
    v11 <- v2 : TFloat64
    jump innerSum_L2
  innerSum_L1:
    v4 <- IntToFloat(v0)
    v6 <- v0 + 1 : TInt64
    v7 <- v4 * v4 : TFloat64
    v8 <- float[1] / v7 : TFloat64
    v9 <- v2 + v8 : TFloat64
    v12 <- v6 : TInt64
    v13 <- v1 : TInt64
    v14 <- v9 : TFloat64
    v0 <- v12 : TInt64
    v1 <- v13 : TInt64
    v2 <- v14 : TFloat64
    jump innerSum_body
  innerSum_L2:
    ret v11
  innerSum_body:
    v3 <- v0 > v1 : TInt64
    branch v3 ? innerSum_L0 : innerSum_L1
```

### Dark LIR (After Register Allocation)
```
innerSum:
  Label "innerSum_L0":
    Jump(Label "innerSum_L2")
  Label "innerSum_L1":
    D0 <- IntToFloat(X1)
    X2 <- Add(X1, Imm 1)
    D0 <- FMul(D0, D0)
    fv1000 <- FLoad(float[1])
    D0 <- FDiv(fv1000, D0)
    D1 <- FAdd(D1, D0)
    X1 <- Mov(Reg X2)
    X1 <- Mov(Reg X3)
    D0 <- FMov(D1)
    X1 <- Mov(Reg X2)
    X3 <- Mov(Reg X3)
    D0 <- FMov(D1)
    D1 <- FMov(D0)
    Jump(Label "innerSum_body")
  Label "innerSum_L2":
    D0 <- FMov(D1)
    Ret
  Label "innerSum_body":
    Cmp(X1, Reg X3)
    CondBranch(GT, Label "innerSum_L0", Label "innerSum_L1")
  Label "innerSum_entry":
    X2 <- Mov(Reg X0)
    X3 <- Mov(Reg X1)
    X1 <- Mov(Reg X2)
    D1 <- FMov(D0)
    Jump(Label "innerSum_body")
```

### Rust Disassembly (Hot Loop)
```asm
00000000000083ec <_ZN5pisum4main17h2289c8ddc22b027cE>:
    83ec:   sub     sp, sp, #0xe0
    83f0:   stp     x29, x30, [sp, #176]
    ...
    8404:   fmov    d1, #1.000000000000000000e+00  ; Pre-load 1.0
    8408:   mov     w9, #0x2710                    ; n = 10000
    840c:   b       841c
    ; Outer loop
    8410:   add     w8, w8, #0x1                   ; rounds++
    8414:   cmp     w8, #0x1f4                     ; rounds < 500?
    8418:   b.eq    844c
    841c:   movi    d0, #0x0                       ; s = 0.0
    8420:   mov     w10, #0x1                      ; k = 1
    ; Inner loop
    8424:   mul     x11, x10, x10                  ; k * k (INTEGER!)
    8428:   cmp     x10, x9                        ; k vs n
    842c:   cinc    x10, x10, ne                   ; k++
    8430:   scvtf   d2, x11                        ; convert to float
    8434:   fdiv    d2, d1, d2                     ; 1.0 / (k*k)
    8438:   fadd    d0, d0, d2                     ; s += ...
    843c:   b.eq    8410
    8440:   cmp     x10, x9
    8444:   b.le    8424
    8448:   b       8410
```
