# Darklang Interpreter Differences

## Overview
This document catalogs all known differences between this compiler and the
official Darklang interpreter. It serves as:
- Reference for the validation script (`scripts/validate-darklang.py`)
- Guide for fixing semantic differences
- Documentation of eval mode limitations

## How to Validate

Run a single expression:
    darklang-interpreter eval "<expression>"

Run the full validation suite:
    python scripts/validate-darklang.py

---

## 1. Eval Mode Limitations

These constructs cannot be validated because `darklang-interpreter eval` doesn't support them.

### 1.1 Function Definitions (`def`)

**Skip reason:** `eval:function_definition`

The eval command only accepts single expressions, not function definitions.

**Example:**
    # This compiler
    def add(a: Int64, b: Int64) : Int64 = a + b
    add(1, 2) = 3

    # Cannot test - eval doesn't support def

### 1.2 Let Bindings (`let`)

**Skip reason:** `eval:let_binding`

**Example:**
    # This compiler (variables.e2e:16)
    let x = 5 in x = 5

    # Darklang eval - not supported
    darklang-interpreter eval "let x = 5L in x"
    # Returns: Parse error

### 1.3 Lambda Expressions (`=>`)

**Skip reason:** `eval:lambda`

**Example:**
    # This compiler (closures.e2e:8)
    ((x: Int64) => x + 1)(5) = 6

    # Darklang syntax would be:
    (fun x -> x + 1L) 5L
    # But complex lambdas not supported in eval

### 1.4 Match Expressions

**Skip reason:** `eval:match`

Pattern matching cannot be evaluated in single-expression mode.

### 1.5 Type Definitions

**Skip reason:** `eval:type_definition`

Custom type and enum definitions not supported in eval.

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
| Eval: def | `eval:` | `def ` in expr | No (eval limitation) |
| Eval: let | `eval:` | `let ` in expr | No (eval limitation) |
| Eval: lambda | `eval:` | `=>` or `fun ` | No (eval limitation) |
| Eval: match | `eval:` | `match ` in expr | No (eval limitation) |
| Syntax: integers | (converted) | bare integers | Auto-converted |
| Syntax: lists | (converted) | `[a, b]` | Auto-converted |
| Syntax: calls | (converted) | `Mod.func(a)` | Auto-converted |
| Semantic: division | `semantic:` | `\s/\s` or `\d+/\d+` | Yes - needs fix |
| Semantic: modulo | `semantic:` | `\s%\s` or `\d+%\d+` | Yes - needs fix |
| Semantic: bitwise | `semantic:` | `<<` `>>` `&` `\|` `^` `~` | Yes - needs fix |
| Stdlib: indexOf | `stdlib:` | `.indexOf` | Yes - return type |
| Stdlib: slice | `stdlib:` | `.slice` | Yes - args differ |
| Internal | `internal:` | `.__` | No (by design) |

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
