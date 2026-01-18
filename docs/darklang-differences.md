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

**Skip reason:** `run:immediate_lambda`
- Darklang doesn't support immediate lambda application syntax like `(fun x -> x) arg`
- Must use pipe syntax instead: `arg |> (fun x -> x)`

### 1.3 Evaluation/Output Expectations

These tests check error conditions or output that can't be validated with the interpreter:

**Skip reason:** `eval:compile_error`
- Tests expecting compile-time errors (e.g., `expect_compile_error`)

**Skip reason:** `eval:error_result`
- Tests expecting runtime errors (e.g., `= error` or `error="message"`)

**Skip reason:** `eval:stdout`
- Tests checking stdout output (e.g., `stdout=...`)

**Skip reason:** `eval:stderr`
- Tests checking stderr output (e.g., `stderr=...`)

**Skip reason:** `eval:exit_code`
- Tests checking exit codes (e.g., `exit=...`)

**Skip reason:** `eval:builtin_test`
- Tests using `Builtin.test` functions (internal test infrastructure)

**Skip reason:** `eval:float_arithmetic`
- Float arithmetic operations may have precision differences

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

### 2.6 Unsigned Integer Suffixes

**Skip reason:** `syntax:unsigned_integer`

Unsigned suffixes like `1uy` (UInt8), `1us` (UInt16), `1ul` (UInt32) are not in Darklang.

### 2.7 String Interpolation

**Skip reason:** `syntax:string_interpolation`

`$"Hello {name}"` syntax not supported in Darklang eval.

### 2.8 Type Parameters

**Skip reason:** `syntax:type_parameter`

Generic syntax like `List<Int64>` uses different notation in Darklang.

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

**Skip reason:** `run:custom_type:*`

User-defined types, enums, and records cannot be validated (PascalCase identifiers not in stdlib).

### 5.5 User Functions

**Skip reason:** `run:user_function:*`

Functions defined outside the test (not in preamble) cannot be validated.

### 5.6 Float Precision

**Skip reason:** `eval:float_precision`

High-precision floats (>2 decimal places) may have different representation.
Float arithmetic uses integer operators in eval mode.

---

## 6. Skip Rule Quick Reference

### Complete Skip Reason List

| Category | Skip Reason | Fixable? |
|----------|-------------|----------|
| **Run Mode** | | |
| | `run:type_definition` | No |
| | `run:record_construction` | No |
| | `run:negation_parenthetical` | No |
| | `run:immediate_lambda` | No |
| | `run:custom_type:*` | No |
| | `run:user_function:*` | No |
| **Eval/Output** | | |
| | `eval:compile_error` | No |
| | `eval:error_result` | No |
| | `eval:stdout` | No |
| | `eval:stderr` | No |
| | `eval:exit_code` | No |
| | `eval:builtin_test` | No |
| | `eval:float_precision` | ? |
| | `eval:float_arithmetic` | ? |
| **Syntax** | | |
| | `syntax:sized_integer` | No |
| | `syntax:unsigned_integer` | No |
| | `syntax:string_interpolation` | No |
| | `syntax:type_parameter` | No |
| **Semantic** | | |
| | `semantic:division` | **Yes** |
| | `semantic:modulo` | **Yes** |
| | `semantic:bitwise` | **Yes** |
| | `semantic:boolean_not` | ? |
| **Stdlib** | | |
| | `stdlib:random` | ? |
| | `stdlib:byte_ops` | ? |
| | `stdlib:list_accessors` | **Yes** |
| | `stdlib:indexOf` | **Yes** |
| | `stdlib:missing` | ? |
| | `stdlib:slice` | Fixed ✓ |
| | `stdlib:int64_math` | ? |
| **Internal** | | |
| | `internal:data_structure` | No |
| | `internal:helper_function` | No |

**Legend:**
- **Yes** = Should be fixed to match Darklang
- No = Cannot/should not be fixed (by design or limitation)
- ? = Needs investigation
- Fixed ✓ = Already fixed

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

---

## 8. Difference Tracking

### 8.1 Fixable Differences (TODO)

These differences SHOULD be fixed to match Darklang:

| Skip Reason | Description | Priority | Status |
|-------------|-------------|----------|--------|
| `semantic:division` | Integer division behavior | High | Not started |
| `semantic:modulo` | Negative modulo handling | High | Not started |
| `semantic:bitwise` | Bitwise operator syntax | Medium | Not started |
| `stdlib:indexOf` | Returns Int64, should return Option | Medium | Not started |
| `stdlib:slice` | Uses (start, length), should use (start, end) | Low | Fixed ✓ |
| `stdlib:list_accessors` | head/tail/last signature differences | Low | Not started |

### 8.2 Non-Fixable Differences

These cannot/should not be fixed (by design or interpreter limitation):

| Skip Reason | Reason Not Fixable |
|-------------|-------------------|
| `run:type_definition` | Requires portable type system |
| `run:record_construction` | Requires type definitions |
| `run:immediate_lambda` | Darklang syntax limitation |
| `run:negation_parenthetical` | Darklang parsing difference |
| `run:custom_type:*` | User types not portable |
| `run:user_function:*` | External definitions not available |
| `eval:compile_error` | Different error testing approach needed |
| `eval:error_result` | Different error testing approach needed |
| `eval:stdout` | Different output testing approach needed |
| `eval:stderr` | Different output testing approach needed |
| `eval:exit_code` | Different output testing approach needed |
| `eval:builtin_test` | Test infrastructure difference |
| `internal:data_structure` | Internal implementation details |
| `internal:helper_function` | Internal implementation details |
| `syntax:sized_integer` | Darklang doesn't support y/s/l suffixes |
| `syntax:unsigned_integer` | Darklang doesn't support unsigned suffixes |
| `syntax:string_interpolation` | Darklang doesn't support $"..." |
| `syntax:type_parameter` | Different generic syntax |

### 8.3 Needs Investigation

| Skip Reason | Question |
|-------------|----------|
| `semantic:boolean_not` | Is this an interpreter bug or real difference? |
| `eval:float_precision` | Can we match Darklang's float representation? |
| `eval:float_arithmetic` | Are float operators different? |
| `stdlib:random` | Does Darklang have Random module? |
| `stdlib:byte_ops` | Does Darklang have byte operations? |
| `stdlib:missing` | Do take/drop/substring exist? |
| `stdlib:int64_math` | Do sub/mul/div/isEven/isOdd exist? |
