# Compiler Passes

The Dark compiler transforms source code through a series of passes, each with a specific responsibility. This document explains each pass in detail.

## Pipeline Overview

```
Source Code
    │
    ▼
┌─────────────────────────────────────┐
│ Pass 1: Parser (1_Parser.fs)        │
│ Source → AST                        │
└─────────────────────────────────────┘
    │
    ▼
┌─────────────────────────────────────┐
│ Pass 1.5: Type Checking             │
│ (1.5_TypeChecking.fs)               │
│ AST → Typed AST                     │
└─────────────────────────────────────┘
    │
    ▼
┌─────────────────────────────────────┐
│ Pass 2: AST to ANF (2_AST_to_ANF.fs)│
│ AST → A-Normal Form                 │
└─────────────────────────────────────┘
    │
    ▼
┌─────────────────────────────────────┐
│ Pass 2.3: ANF Optimizations         │
│ (2.3_ANF_Optimize.fs)               │
└─────────────────────────────────────┘
    │
    ▼
┌─────────────────────────────────────┐
│ Pass 2.4: ANF Inlining              │
│ (2.4_ANF_Inlining.fs)               │
└─────────────────────────────────────┘
    │
    ▼
┌─────────────────────────────────────┐
│ Pass 2.5: Ref Count Insertion       │
│ (2.5_RefCountInsertion.fs)          │
│ ANF → ANF with memory ops           │
└─────────────────────────────────────┘
    │
    ▼
┌─────────────────────────────────────┐
│ Pass 2.6: Print Insertion           │
│ (2.6_PrintInsertion.fs)             │
└─────────────────────────────────────┘
    │
    ▼
┌─────────────────────────────────────┐
│ Pass 2.7: Tail Call Optimization    │
│ (2.7_TailCallDetection.fs)          │
└─────────────────────────────────────┘
    │
    ▼
┌─────────────────────────────────────┐
│ Pass 3: ANF to MIR (3_ANF_to_MIR.fs)│
│ ANF → Control Flow Graph            │
└─────────────────────────────────────┘
    │
    ▼
┌─────────────────────────────────────┐
│ Pass 3.1: SSA Construction          │
│ (3.1_SSA_Construction.fs)           │
└─────────────────────────────────────┘
    │
    ▼
┌─────────────────────────────────────┐
│ Pass 3.5: MIR Optimizations         │
│ (3.5_MIR_Optimize.fs)               │
└─────────────────────────────────────┘
    │
    ▼
┌─────────────────────────────────────┐
│ Pass 4: MIR to LIR (4_MIR_to_LIR.fs)│
│ MIR → Low-level IR (virtual regs)   │
└─────────────────────────────────────┘
    │
    ▼
┌─────────────────────────────────────┐
│ Pass 4.5: LIR Peephole              │
│ (4.5_LIR_Peephole.fs)               │
└─────────────────────────────────────┘
    │
    ▼
┌─────────────────────────────────────┐
│ Pass 5: Register Allocation         │
│ (5_RegisterAllocation.fs)           │
│ LIR (virtual) → LIR (physical)      │
└─────────────────────────────────────┘
    │
    ▼
┌─────────────────────────────────────┐
│ Pass 5.5: Function Tree Shaking     │
│ (5.5_FunctionTreeShaking.fs)        │
└─────────────────────────────────────┘
    │
    ▼
┌─────────────────────────────────────┐
│ Pass 6: Code Generation             │
│ (6_CodeGen.fs)                      │
│ LIR → ARM64 Instructions            │
└─────────────────────────────────────┘
    │
    ▼
┌─────────────────────────────────────┐
│ Pass 7: ARM64 Emit                  │
│ (7_ARM64_Emit.fs)                   │
│ ARM64 → Machine Code → Executable   │
└─────────────────────────────────────┘
    │
    ▼
Executable Binary
```

---

## Pass 1: Parser (`1_Parser.fs`)

**Input**: Source code string
**Output**: Abstract Syntax Tree (AST)

### Responsibilities
- **Lexical analysis**: Convert character stream to tokens
- **Syntactic analysis**: Build AST using recursive descent parsing
- **Operator precedence**: Handle binary operators with Pratt parsing

### Key Algorithms
- **Recursive descent**: Each grammar production is a function
- **Pratt precedence parsing**: Handles operator precedence elegantly
- **Escape sequence processing**: Handle `\n`, `\t`, `\"`, etc. in strings

### Example Transformation
```
Input:  "let x = 1 + 2 in x * 3"
Output: Let("x", BinOp(Add, IntLiteral(1), IntLiteral(2)),
            BinOp(Mul, Var("x"), IntLiteral(3)))
```

---

## Pass 1.5: Type Checking (`1.5_TypeChecking.fs`)

**Input**: AST
**Output**: Type-checked AST (same structure, validated)

### Responsibilities
- **Type validation**: Ensure expressions have consistent types
- **Error reporting**: Clear messages with source locations
- **Free variable collection**: For closure analysis

### Key Algorithms
- **Top-down checking**: Push expected types down, validate bottom-up
- **Result-based errors**: No exceptions, explicit error propagation
- **Environment threading**: Track variable types through expressions

### Example Error
```
Input:  1 + "hello"
Error:  Type mismatch: expected Int64, got String in binary operator
```

---

## Pass 2: AST to ANF (`2_AST_to_ANF.fs`)

**Input**: AST
**Output**: A-Normal Form (ANF)

### Responsibilities
- **Flatten nested expressions**: All intermediate results get names
- **Make evaluation order explicit**: Left-to-right evaluation visible
- **Handle desugaring**: Convert high-level constructs to primitives
- **Monomorphization**: Generate specialized versions of generic functions
- **Lambda lifting**: Convert lambdas to top-level functions with closures
  - Unresolved type variables are preserved; if hashing/equality intrinsics are needed, the compiler emits `__hash_unknown`/`__key_eq_unknown` that crash if executed.
  - Optimizations like `Dict.fromList([])` → `Dict.empty` only apply when type arguments are concrete.

### Key Algorithms
- **Fresh variable generation**: VarGen creates unique temporaries
- **Let-binding normalization**: Every complex subexpression bound to temp

### Example Transformation
```
Input:  1 + 2 * 3
Output: let t0 = 2 * 3 in
        let t1 = 1 + t0 in
        return t1
```

### Why ANF?
- Makes evaluation order explicit (important for side effects)
- Simplifies code generation (no nested expressions to evaluate)
- Enables optimizations (common subexpression elimination)

---

## Pass 2.3: ANF Optimizations (`2.3_ANF_Optimize.fs`)

**Input**: ANF
**Output**: Optimized ANF

### Responsibilities
- **Constant folding**: Fold literals and algebraic identities
- **Constant propagation**: Substitute known literals
- **Copy propagation**: Remove trivial `let` bindings
- **Dead code elimination**: Drop unused bindings without side effects
- **Strength reduction**: `mul/div/mod` by powers of 2 → shifts/bitwise ops

### Sub-passes (grouped)
- `const_folding`, `const_prop`, `copy_prop`, `dce`, `strength_reduction`

---

## Pass 2.4: ANF Inlining (`2.4_ANF_Inlining.fs`)

**Input**: ANF
**Output**: ANF with selected calls inlined

### Responsibilities
- **Inline small functions**: Reduce call overhead when safe
- **Preserve semantics**: Respect evaluation order and side effects

---

## Pass 2.5: Reference Count Insertion (`2.5_RefCountInsertion.fs`)

**Input**: ANF
**Output**: ANF with RefCountInc/RefCountDec operations

### Responsibilities
- **Memory management**: Insert reference counting operations
- **Ownership tracking**: Determine when values need inc/dec

### Key Algorithms
- **Borrowed calling convention**: Callers retain ownership, no inc on call
- **Scope-based release**: Dec when value goes out of scope

---

## Pass 2.6: Print Insertion (`2.6_PrintInsertion.fs`)

**Input**: ANF
**Output**: ANF with explicit print operations

### Responsibilities
- **Ensure observable output**: Insert print calls for program results
- **Preserve types**: Use type information to select correct printers

---

## Pass 2.7: Tail Call Optimization (`2.7_TailCallDetection.fs`)

**Input**: ANF with refcounting
**Output**: ANF annotated for tail calls / self-recursion loops

### Responsibilities
- **Detect tail positions**: Identify safe tail calls
- **Self-recursion loop conversion**: Turn tail-recursive calls into jumps

---

## Pass 3: ANF to MIR (`3_ANF_to_MIR.fs`)

**Input**: ANF
**Output**: Mid-level IR as Control Flow Graph (CFG)

### Responsibilities
- **Build CFG**: Convert structured control flow to basic blocks
- **Handle branches**: If/else becomes conditional jumps
- **Literal lowering**: Keep string/float constants as symbolic values

### Key Concepts
- **Basic block**: Sequence of instructions with single entry/exit
- **CFG**: Graph of basic blocks connected by jumps
- **Virtual registers**: Unlimited registers, allocation comes later

### Example Transformation
```
Input:  if x > 0 then 1 else 2

Output: block0:
          cmp x, 0
          ble block2
        block1:
          mov result, 1
          jmp block3
        block2:
          mov result, 2
        block3:
          return result
```

---

## Pass 3.1: SSA Construction (`3.1_SSA_Construction.fs`)

**Input**: MIR CFG
**Output**: MIR CFG in SSA form

### Responsibilities
- **SSA form**: Insert phi nodes and rename variables
- **Dominance tracking**: Build dominators for SSA placement

---

## Pass 3.5: MIR Optimizations (`3.5_MIR_Optimize.fs`)

**Input**: MIR CFG in SSA
**Output**: Optimized MIR CFG

### Responsibilities
- **Constant folding**: Fold literal computations
- **CSE**: Eliminate duplicate pure expressions
- **Copy propagation**: Simplify moves and trivial phis
- **DCE**: Remove unused instructions
- **CFG simplification**: Remove empty blocks / redirect edges
- **LICM**: Hoist loop-invariant expressions

### Sub-passes (grouped)
- `const_folding`, `cse`, `copy_prop`, `dce`, `cfg_simplify`, `licm`

---

## Pass 4: MIR to LIR (`4_MIR_to_LIR.fs`)

**Input**: MIR (platform-independent)
**Output**: LIR (ARM64-specific, virtual registers)

### Responsibilities
- **Instruction selection**: Choose ARM64 instructions for MIR operations
- **Address constraints**: Handle ARM64 immediate value limits
- **Calling convention**: Set up function calls per ARM64 ABI
- **Symbolic constants**: Keep string/float constants by value until late pool resolution

### Key Algorithms
- **Pattern matching**: Each MIR operation maps to ARM64 sequence
- **Immediate splitting**: Large constants may need multiple instructions

### Example Transformation
```
Input (MIR):  Add(v1, v2, v3)      // v1 = v2 + v3
Output (LIR): ADD(V1, V2, V3)      // ARM64 ADD instruction
```

Implementation detail: LIR keeps string/float constants by value and defers
pool construction until ARM64 emission. This avoids per-function pool remapping
when mixing stdlib, preamble, and user functions.

---

## Pass 4.5: LIR Peephole (`4.5_LIR_Peephole.fs`)

**Input**: LIR (virtual regs)
**Output**: Optimized LIR (virtual regs)

### Responsibilities
- **Peephole rewrites**: Local instruction simplifications
- **Branch fusion**: Combine compare/set/branch sequences when safe

---

## Pass 5: Register Allocation (`5_RegisterAllocation.fs`)

**Input**: LIR with virtual registers
**Output**: LIR with physical registers

### Responsibilities
- **Liveness analysis**: Determine when each virtual register is live
- **Register assignment**: Map virtual to physical registers
- **Spill handling**: Use stack when registers exhausted

### Key Algorithms
- **Backward dataflow**: Compute live ranges from uses to definitions
- **Linear scan**: Efficient allocation using sorted live intervals
- **Spill code generation**: Load/store for spilled values

### Register Classes
- **Caller-saved (preferred)**: X1-X10, X14-X15
- **Callee-saved**: X19-X26 (used under high pressure)
- **Reserved**: X0 (return), X11-X13 (spill temps), X27-X28 (memory), X29-X30 (ABI)

---

## Pass 5.5: Function Tree Shaking (`5.5_FunctionTreeShaking.fs`)

**Input**: LIR (physical regs)
**Output**: LIR with only reachable functions

### Responsibilities
- **Prune unused functions**: Keep `_start` roots and reachable callees
- **Stdlib filtering**: Include only stdlib functions called by user code

---

## Pass 6: Code Generation (`6_CodeGen.fs`)

**Input**: LIR with physical registers
**Output**: ARM64 instruction list

### Responsibilities
- **Final instruction generation**: Convert LIR to ARM64 types
- **Prologue/epilogue**: Function entry/exit code
- **Stack frame setup**: Allocate space for spills and locals

### Key Outputs
- ARM64 instruction sequence per function
- Entry point setup (main function handling)

---

## Pass 7: ARM64 Emit (`7_ARM64_Emit.fs`)

**Input**: ARM64 instruction list (symbolic data labels)
**Output**: Executable file (Mach-O or ELF)

### Responsibilities
- **Literal pool resolution**: Intern string/float literals into pools
- **Instruction encoding**: Convert to binary per ARM64 spec
- **Binary generation**: Emit Mach-O (macOS) or ELF (Linux)

### Internals
Uses `7_ARM64_Encoding.fs` for encoding and `8_Binary_Generation_*.fs` for binary layout.

---

## Data Structure Files

| File | Purpose |
|------|---------|
| `AST.fs` | Abstract Syntax Tree types |
| `ANF.fs` | A-Normal Form types |
| `MIR.fs` | Mid-level IR types |
| `LIR.fs` | Low-level IR types |
| `ARM64.fs` | ARM64 instruction types |
| `ARM64Symbolic.fs` | Symbolic ARM64 instruction types |

---

## Testing Each Pass

Each pass can be tested in isolation:

- **Parser**: Test with source strings, check AST structure
- **Type Checker**: Test type errors are caught
- **ANF**: PassTestRunner validates ANF output
- **End-to-end**: `.e2e` files test full pipeline

Run all tests: `dotnet test`
