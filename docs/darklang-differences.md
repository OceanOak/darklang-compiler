# Darklang Interpreter Differences

## Overview
This document catalogs all known differences between this compiler and the
official Darklang interpreter. It serves as:
- Reference for the validation script (`scripts/validate-darklang.py`)
- Guide for fixing semantic differences
- Documentation of run mode limitations

## How to Validate

Run a single expression (limited support):
    darklang-interpreter eval "<expression>"

Run a Dark script file (full support):
    darklang-interpreter run <file>.dark

Run the full validation suite:
    python scripts/validate-darklang.py

The validation script uses file-based execution (`run` command) for all tests,
which provides broader syntax support than the `eval` command.

---

## 1. File-Based Validation Approach

The validation script generates temporary `.dark` files for each test and
executes them via `darklang-interpreter run`. This approach:

- **Supports** function definitions (converted to curried lambdas)
- **Supports** let bindings
- **Supports** lambda expressions
- **Supports** match expressions
- Uses `Builtin.debug` to output results for comparison

### 1.1 How Tests Are Converted

For a test like `let x = 5 in x + 1 = 6`:
```dark
let __result = let x = 5L in x + 1L
Builtin.debug "" __result
0L
```

For tests with `def` preambles:
```dark
// Original: def add(a: Int64, b: Int64) : Int64 = a + b add(1, 2) = 3
let add = fun a -> fun b -> a + b
let __result = add 1L 2L
Builtin.debug "" __result
0L
```

### 1.2 Run Mode Limitations

These constructs cannot be validated even with file-based execution:

**Skip reason:** `run:type_definition`
- Custom type and enum definitions are not portable

**Skip reason:** `run:record_construction`
- Record literals like `MyRecord { field = value }` require type definitions

**Skip reason:** `run:negation_parenthetical`
- Expressions like `-(5)` may have parsing differences

---

## 2. Syntactic Differences (Convertible)

These are syntax variations that the validation script automatically converts.

### 2.1 Integer Literals

**Conversion:** `5` → `5L`

This compiler allows bare integers; Darklang requires the `L` suffix.

**Example:**
    # This compiler
    1 + 2 = 3

    # Darklang
    darklang-interpreter eval "1L + 2L"
    # Returns: 3

### 2.2 List Separators

**Conversion:** `[1, 2, 3]` → `[1L; 2L; 3L]`

This compiler uses commas; Darklang uses semicolons.

**Example:**
    # This compiler
    [1, 2, 3]

    # Darklang
    darklang-interpreter eval "[1L; 2L; 3L]"
    # Returns: [1; 2; 3]

### 2.3 Function Call Syntax

**Conversion:** `Module.func(arg1, arg2)` → `Stdlib.Module.func arg1 arg2`

This compiler uses parentheses and commas; Darklang uses space-separated args.

**Example:**
    # This compiler
    Int64.add(1, 2)

    # Darklang
    darklang-interpreter eval "Stdlib.Int64.add 1L 2L"
    # Returns: 3

### 2.4 Lambda Syntax

**Conversion:** `(x: Int64) => body` → `fun x -> body`

Different arrow syntax between compilers.

### 2.5 Sized Integer Suffixes

**Skip reason:** `syntax:sized_integer`

Suffixes like `1y` (Int8), `1s` (Int16), `1l` (Int32) are not in Darklang.

### 2.6 String Interpolation

**Skip reason:** `syntax:string_interpolation`

`$"Hello {name}"` syntax not supported in Darklang eval.

---

## 3. Semantic Differences

These are cases where both compilers accept the syntax but produce different results.
**These need to be fixed in this compiler to match Darklang.**

### 3.1 Division Operator (`/`)

**Skip reason:** `semantic:division`
**Status:** Needs investigation

**This compiler:** Uses `/` for integer division (truncates toward zero)
**Darklang:** Uses `/` for float division

**Example (integers.e2e:50):**
    # This compiler
    10 / 3 = 3  (integer division)

    # Darklang - need to verify behavior
    darklang-interpreter eval "10L / 3L"

**Implementation:** `src/DarkCompiler/stdlib/Int64.dark:13`
    def Stdlib.Int64.div(a: Int64, b: Int64) : Int64 = a / b

### 3.2 Modulo Operator (`%`)

**Skip reason:** `semantic:modulo`
**Status:** Needs investigation

**Concern:** Negative number handling may differ.

**Example (integers.e2e:281):**
    # This compiler
    -10 % 3 = 2  (truncated modulo)

    # Darklang - need to verify
    darklang-interpreter eval "(-10L) % 3L"

**Test:** Check if Darklang uses floor modulo (Python-style) or truncated modulo (C-style).

### 3.3 Bitwise Operators

**Skip reason:** `semantic:bitwise`
**Status:** Needs investigation

Operators: `<<`, `>>`, `&`, `|`, `^`, `~`

**Examples (bitwise.e2e):**
    # Left shift (line 19)
    1 << 1 = 2

    # Right shift (line 58)
    256 >> 1 = 128

    # Bitwise AND (line 90)
    1 & 1 = 1

**Validation:**
    darklang-interpreter eval "1L <<< 1L"   # Note: Darklang may use <<< syntax
    darklang-interpreter eval "256L >>> 1L"

### 3.4 Boolean Not (`!`)

**Skip reason:** `semantic:boolean_not` (interpreter bug)
**Status:** Known interpreter bug - skip

The `!` operator may behave incorrectly in darklang-interpreter eval mode.

---

## 4. Stdlib Function Differences

Functions where signatures or behavior differ between compilers.

### 4.1 indexOf

**Skip reason:** `stdlib:indexOf`
**Status:** Signature difference

**This compiler:** Returns `Int64` (-1 if not found)
**Darklang:** Returns `Option<Int64>` (None if not found)

**Example (strings.e2e:210):**
    # This compiler
    Stdlib.String.indexOf("hello world", "hello") = 0
    Stdlib.String.indexOf("hello world", "xyz") = -1

    # Darklang
    darklang-interpreter eval 'Stdlib.String.indexOf "hello world" "hello"'
    # Expected: Some(0) or similar Option type

**Implementation:** `src/DarkCompiler/stdlib/String.dark:84-85`
    def Stdlib.String.indexOf(s: String, search: String) : Int64 =
        Stdlib.String.__findFrom(s, search, 0)

### 4.2 slice

**Skip reason:** `stdlib:slice`
**Status:** Argument interpretation differs

**This compiler:** `slice(string, start, length)` - length-based
**Darklang:** May use `slice(string, start, end)` - end-index-based

**Example (strings.e2e:243):**
    # This compiler
    Stdlib.String.slice("hello world", 0, 5) = "hello"
    Stdlib.String.slice("hello world", 6, 5) = "world"

    # Darklang - verify argument interpretation
    darklang-interpreter eval 'Stdlib.String.slice "hello world" 0L 5L'

**Implementation:** `src/DarkCompiler/stdlib/String.dark:102-121`

### 4.3 head, tail, last, init

**Skip reason:** `stdlib:list_accessors`
**Status:** May have signature differences

All return `Option<T>` in this compiler for safety:

    def Stdlib.List.head<a>(list: List<a>) : Stdlib.Option.Option<a>
    def Stdlib.List.tail<a>(list: List<a>) : Stdlib.Option.Option<List<a>>
    def Stdlib.List.last<a>(list: List<a>) : Stdlib.Option.Option<a>

### 4.4 take, drop, substring

**Skip reason:** `stdlib:missing`
**Status:** May not exist in Darklang

### 4.5 Byte Operations

**Skip reason:** `stdlib:byte_ops`
**Status:** May not exist in Darklang

Functions: `getByteAt`, `setByteAt`, `appendByte`, `fromBytes`, `toBytes`

### 4.6 Int64 Math Functions

**Skip reason:** `stdlib:int64_math`
**Status:** May not exist in Darklang

Functions: `Int64.sub`, `Int64.mul`, `Int64.div`, `Int64.isEven`, `Int64.isOdd`

---

## 5. Internal/Unsupported Features

Features that exist only in this compiler and should never be validated.

### 5.1 Internal Functions (`.__` prefix)

**Skip reason:** `internal:helper_function`

Functions like `__digitToString`, `__findFrom` are implementation helpers.

### 5.2 FingerTree and HAMT

**Skip reason:** `internal:data_structure`

`Stdlib.__FingerTree` and `Stdlib.__HAMT` are internal implementations.

### 5.3 Random

**Skip reason:** `stdlib:random`

`Random.*` functions may not exist or behave differently.

### 5.4 Custom Types

**Skip reason:** `eval:custom_type`

User-defined types, enums, and records cannot be validated.

### 5.5 Float Precision

**Skip reason:** `eval:float_precision`

High-precision floats (>2 decimal places) may have different representation.
Float arithmetic uses integer operators in eval mode.

---

## 6. Skip Rule Quick Reference

| Category | Skip Prefix | Example Pattern | Fixable? |
|----------|-------------|-----------------|----------|
| Run: type def | `run:` | `type ` in expr | No (requires type system) |
| Run: record | `run:` | `Name { field }` | No (requires types) |
| Run: custom type | `run:` | PascalCase not in stdlib | No (requires types) |
| Run: user func | `run:` | `func(...)` not in preamble | No (external def) |
| Syntax: integers | (converted) | bare integers | Auto-converted |
| Syntax: lists | (converted) | `[a, b]` | Auto-converted |
| Syntax: calls | (converted) | `Mod.func(a)` | Auto-converted |
| Semantic: division | `semantic:` | `\s/\s` or `\d+/\d+` | Yes - needs fix |
| Semantic: modulo | `semantic:` | `\s%\s` or `\d+%\d+` | Yes - needs fix |
| Semantic: bitwise | `semantic:` | `<<` `>>` `&` `\|` `^` `~` | Yes - needs fix |
| Stdlib: indexOf | `stdlib:` | `.indexOf` | Yes - return type |
| Stdlib: slice | `stdlib:` | `.slice` | Yes - args differ |
| Internal | `internal:` | `.__` | No (by design) |

**Note:** With file-based execution, the following are now **supported**:
- Function definitions (`def`) - converted to curried lambdas
- Let bindings (`let x = ... in ...`)
- Lambda expressions (`=>` and `fun`)
- Match expressions

---

## 7. Validation Workflow

To investigate a specific difference:

1. Find a skipped test in E2E files
2. Convert to Darklang syntax manually
3. Run through interpreter:
       darklang-interpreter eval "<darklang_expression>"
4. Compare with expected result
5. If different, fix the compiler implementation
6. Update skip rules in validate-darklang.py
