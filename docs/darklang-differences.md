# Darklang Differences

## Overview

This compiler aims to match the Darklang interpreter. The interpreter is always correct.

This document catalogs all known differences between this compiler and the official
Darklang interpreter. It serves as:
- Reference for the validation script (`scripts/validate-darklang.py`)
- Guide for fixing semantic differences
- Documentation of compiler-only features

To validate tests against the interpreter, run:
```bash
python scripts/validate-darklang.py --help-full  # See detailed documentation
python scripts/validate-darklang.py              # Run validation
```

---

## 1. Syntactic Differences

Features supported in both with different syntax. The validation script automatically
converts from compiler syntax to interpreter syntax.

| Feature | Compiler | Interpreter | Conversion |
|---------|----------|-------------|------------|
| Integer literals | `5` | `5L` | Add L suffix |
| List separators | `[1, 2]` | `[1L; 2L]` | Comma to semicolon |
| Function calls | `Mod.fn(a, b)` | `Stdlib.Mod.fn a b` | Parentheses to spaces |
| Lambdas | `(x: T) => body` | `fun x -> body` | Different arrow syntax |

### 1.1 Integer Literals

**Conversion:** `5` → `5L`

This compiler allows bare integers; Darklang requires the `L` suffix.

```
# This compiler
1 + 2 = 3

# Darklang
darklang-interpreter eval "1L + 2L"
# Returns: 3
```

### 1.2 List Separators

**Conversion:** `[1, 2, 3]` → `[1L; 2L; 3L]`

This compiler uses commas; Darklang uses semicolons.

```
# This compiler
[1, 2, 3]

# Darklang
darklang-interpreter eval "[1L; 2L; 3L]"
# Returns: [1; 2; 3]
```

### 1.3 Function Call Syntax

**Conversion:** `Module.func(arg1, arg2)` → `Stdlib.Module.func arg1 arg2`

This compiler uses parentheses and commas; Darklang uses space-separated args.

```
# This compiler
Int64.add(1, 2)

# Darklang
darklang-interpreter eval "Stdlib.Int64.add 1L 2L"
# Returns: 3
```

### 1.4 Lambda Syntax

**Conversion:** `(x: Int64) => body` → `fun x -> body`

Different arrow syntax between compilers.

---

## 2. Semantic Bugs

Areas where compiler produces WRONG output. These need to be fixed to match Darklang.

| Bug | Skip Reason | Description | Status |
|-----|-------------|-------------|--------|
| Division `/` | `semantic:division` | Integer vs float division | Needs fix |
| Modulo `%` | `semantic:modulo` | Negative number handling | Needs fix |
| Bitwise ops | `semantic:bitwise` | `<<`, `>>`, `&`, `\|`, `^`, `~` | Needs fix |
| Boolean not `!` | `semantic:boolean_not` | Incorrect behavior | Needs fix |
| indexOf | `stdlib:indexOf` | Returns Int64, should return Option | Needs fix |
| list_accessors | `stdlib:list_accessors` | head/tail/last signature diffs | Needs fix |

### 2.1 Division Operator (`/`)

**Skip reason:** `semantic:division`

**This compiler:** Uses `/` for integer division (truncates toward zero)
**Darklang:** Uses `/` for float division

```
# This compiler
10 / 3 = 3  (integer division)

# Darklang - need to verify behavior
darklang-interpreter eval "10L / 3L"
```

**Implementation:** `src/DarkCompiler/stdlib/Int64.dark:13`

### 2.2 Modulo Operator (`%`)

**Skip reason:** `semantic:modulo`

**Concern:** Negative number handling may differ.

```
# This compiler
-10 % 3 = 2  (truncated modulo)

# Darklang - need to verify
darklang-interpreter eval "(-10L) % 3L"
```

**Test:** Check if Darklang uses floor modulo (Python-style) or truncated modulo (C-style).

### 2.3 Bitwise Operators

**Skip reason:** `semantic:bitwise`

Operators: `<<`, `>>`, `&`, `|`, `^`, `~`

```
# Left shift
1 << 1 = 2

# Right shift
256 >> 1 = 128

# Bitwise AND
1 & 1 = 1
```

**Validation:**
```bash
darklang-interpreter eval "1L <<< 1L"   # Note: Darklang may use <<< syntax
darklang-interpreter eval "256L >>> 1L"
```

### 2.4 Boolean Not (`!`)

**Skip reason:** `semantic:boolean_not`

The `!` operator may behave incorrectly in darklang-interpreter eval mode.
This may be an interpreter bug.

### 2.5 indexOf

**Skip reason:** `stdlib:indexOf`

**This compiler:** Returns `Int64` (-1 if not found)
**Darklang:** Returns `Option<Int64>` (None if not found)

```
# This compiler
Stdlib.String.indexOf("hello world", "hello") = 0
Stdlib.String.indexOf("hello world", "xyz") = -1

# Darklang
darklang-interpreter eval 'Stdlib.String.indexOf "hello world" "hello"'
# Expected: Some(0) or similar Option type
```

**Implementation:** `src/DarkCompiler/stdlib/String.dark:84-85`

### 2.6 List Accessors (head, tail, last, init)

**Skip reason:** `stdlib:list_accessors`

All return `Option<T>` in this compiler for safety:
```
def Stdlib.List.head<a>(list: List<a>) : Stdlib.Option.Option<a>
def Stdlib.List.tail<a>(list: List<a>) : Stdlib.Option.Option<List<a>>
def Stdlib.List.last<a>(list: List<a>) : Stdlib.Option.Option<a>
```

Darklang may have different signatures.

---

## 3. Tooling Differences

Acceptable differences due to compilation vs interpretation model.
These tests check error conditions or output that can't be validated with the interpreter.

| Skip Reason | Description |
|-------------|-------------|
| `eval:compile_error` | Tests expecting compile-time errors (e.g., `expect_compile_error`) |
| `eval:error_result` | Tests expecting runtime errors (e.g., `= error` or `error="message"`) |
| `eval:stdout` | Tests checking stdout output (e.g., `stdout=...`) |
| `eval:stderr` | Tests checking stderr output (e.g., `stderr=...`) |
| `eval:exit_code` | Tests checking exit codes (e.g., `exit=...`) |
| `eval:builtin_test` | Tests using `Builtin.test` functions (internal test infrastructure) |

---

## 4. Compiler-Only Features

Features in compiler not in interpreter. These are skipped during validation.

### Syntax Features

| Feature | Skip Reason | Example | Description |
|---------|-------------|---------|-------------|
| Sized integers | `syntax:sized_integer` | `1y`, `1s`, `1l` | Int8, Int16, Int32 suffixes |
| Unsigned integers | `syntax:unsigned_integer` | `1uy`, `1us`, `1ul` | UInt8, UInt16, UInt32 suffixes |
| String interpolation | `syntax:string_interpolation` | `$"Hello {x}"` | Not supported in Darklang |
| Type parameters | `syntax:type_parameter` | `List<Int64>` | Different generic syntax in Darklang |

### Internal Features

| Feature | Skip Reason | Description |
|---------|-------------|-------------|
| Internal functions | `internal:helper_function` | Functions like `__digitToString`, `__findFrom` are implementation helpers |
| FingerTree/HAMT | `internal:data_structure` | `Stdlib.__FingerTree` and `Stdlib.__HAMT` are internal implementations |

### Stdlib Extensions

| Feature | Skip Reason | Description |
|---------|-------------|-------------|
| Random | `stdlib:random` | `Random.*` functions may not exist or behave differently |
| Byte operations | `stdlib:byte_ops` | `getByteAt`, `setByteAt`, `appendByte`, `fromBytes`, `toBytes` |
| Int64 math | `stdlib:int64_math` | `Int64.sub`, `Int64.mul`, `Int64.div`, `Int64.isEven`, `Int64.isOdd` |
| Missing functions | `stdlib:missing` | `take`, `drop`, `substring` may not exist in Darklang |
| slice | `stdlib:slice` | Uses (start, length) semantics vs Darklang's (start, end) |

### Precision Differences

| Feature | Skip Reason | Description |
|---------|-------------|-------------|
| Float precision | `eval:float_precision` | High-precision floats (>2 decimal places) may have different representation |

---

## 5. Custom Types and User Functions

| Skip Reason | Description |
|-------------|-------------|
| `run:custom_type:*` | User-defined types, enums, and records (PascalCase identifiers not in stdlib) |
| `run:user_function:*` | Functions defined outside the test (not in preamble) |

---

## 6. Skip Reason Quick Reference

| Category | Skip Reason | Fixable? |
|----------|-------------|----------|
| **Semantic** | | |
| | `semantic:division` | **Yes** |
| | `semantic:modulo` | **Yes** |
| | `semantic:bitwise` | **Yes** |
| | `semantic:boolean_not` | ? |
| **Stdlib** | | |
| | `stdlib:indexOf` | **Yes** |
| | `stdlib:list_accessors` | **Yes** |
| | `stdlib:slice` | Fixed ✓ |
| | `stdlib:random` | ? |
| | `stdlib:byte_ops` | ? |
| | `stdlib:missing` | ? |
| | `stdlib:int64_math` | ? |
| **Tooling** | | |
| | `eval:compile_error` | No |
| | `eval:error_result` | No |
| | `eval:stdout` | No |
| | `eval:stderr` | No |
| | `eval:exit_code` | No |
| | `eval:builtin_test` | No |
| | `eval:float_precision` | ? |
| **Syntax** | | |
| | `syntax:sized_integer` | No |
| | `syntax:unsigned_integer` | No |
| | `syntax:string_interpolation` | No |
| | `syntax:type_parameter` | No |
| **Internal** | | |
| | `internal:data_structure` | No |
| | `internal:helper_function` | No |
| **Custom** | | |
| | `run:custom_type:*` | No |
| | `run:user_function:*` | No |

**Legend:**
- **Yes** = Should be fixed to match Darklang
- No = Cannot/should not be fixed (by design or limitation)
- ? = Needs investigation
- Fixed ✓ = Already fixed

---

## 7. Validation Workflow

To investigate a specific difference:

1. Find a skipped test in E2E files
2. Convert to Darklang syntax manually
3. Run through interpreter:
   ```bash
   darklang-interpreter eval "<darklang_expression>"
   ```
4. Compare with expected result
5. If different, fix the compiler implementation
6. Update skip rules in `validate-darklang.py`

---

## 8. Fixable Differences (TODO)

These differences SHOULD be fixed to match Darklang:

| Skip Reason | Description | Priority | Status |
|-------------|-------------|----------|--------|
| `semantic:division` | Integer division behavior | High | Not started |
| `semantic:modulo` | Negative modulo handling | High | Not started |
| `semantic:bitwise` | Bitwise operator syntax | Medium | Not started |
| `stdlib:indexOf` | Returns Int64, should return Option | Medium | Not started |
| `stdlib:slice` | Uses (start, length), should use (start, end) | Low | Fixed ✓ |
| `stdlib:list_accessors` | head/tail/last signature differences | Low | Not started |

---

## 9. Needs Investigation

| Skip Reason | Question |
|-------------|----------|
| `semantic:boolean_not` | Is this an interpreter bug or real difference? |
| `eval:float_precision` | Can we match Darklang's float representation? |
| `stdlib:random` | Does Darklang have Random module? |
| `stdlib:byte_ops` | Does Darklang have byte operations? |
| `stdlib:missing` | Do take/drop/substring exist? |
| `stdlib:int64_math` | Do sub/mul/div/isEven/isOdd exist? |
