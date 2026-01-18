# Darklang Differences

## Overview

This compiler aims to match the Darklang interpreter. The interpreter is always correct,
except in places where a developer identifies specific issues.

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
| Sized integers | `1y`, `1s`, `1l` | Not supported | Int8, Int16, Int32 suffixes |
| Unsigned integers | `1uy`, `1us`, `1ul` | Not supported | UInt8, UInt16, UInt32 suffixes |
| List separators | `[1, 2]` | `[1L; 2L]` | Comma to semicolon |
| Function calls | `Mod.fn(a, b)` | `Stdlib.Mod.fn a b` | Parentheses to spaces |
| Lambdas | `(x: T) => body` | `fun x -> body` | Different arrow syntax |
| Type parameters | `List<Int64>` | Different syntax | Generic type notation |
| String interpolation | `$"Hello {x}"` | Not supported | Interpolated strings |

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

```
# This compiler
List.map([1, 2, 3], (x: Int64) => x * 2)

# Darklang
Stdlib.List.map [1L; 2L; 3L] (fun x -> Stdlib.Int64.multiply x 2L)
```

### 1.5 Sized Integers

**Not supported in interpreter**

This compiler supports sized integer literals with suffixes:
- `y` for Int8 (e.g., `1y`)
- `s` for Int16 (e.g., `1s`)
- `l` for Int32 (e.g., `1l`)

```
# This compiler
let x: Int8 = 127y
let y: Int16 = 32767s
let z: Int32 = 2147483647l

# Darklang - not supported
```

Tests using sized integers are skipped with `syntax:sized_integer`.

### 1.6 Unsigned Integers

**Not supported in interpreter**

This compiler supports unsigned integer literals with suffixes:
- `uy` for UInt8 (e.g., `1uy`)
- `us` for UInt16 (e.g., `1us`)
- `ul` for UInt32 (e.g., `1ul`)

```
# This compiler
let x: UInt8 = 255uy
let y: UInt16 = 65535us
let z: UInt32 = 4294967295ul

# Darklang - not supported
```

Tests using unsigned integers are skipped with `syntax:unsigned_integer`.

### 1.7 Type Parameters

**Not supported in interpreter**

This compiler uses angle bracket syntax for generic types:

```
# This compiler
let x: List<Int64> = [1, 2, 3]
let y: Option<String> = Some("hello")

# Darklang - different syntax or not supported in eval
```

Tests using type parameters are skipped with `syntax:type_parameter`.

### 1.8 String Interpolation

**Not supported in interpreter**

This compiler supports string interpolation with `$"..."` syntax:

```
# This compiler
let name = "world"
$"Hello {name}!"  // Returns: "Hello world!"

# Darklang - not supported, use String.concat instead
```

Tests using string interpolation are skipped with `syntax:string_interpolation`.

---

## 2. Semantic Bugs

Areas where compiler produces WRONG output. These need to be fixed to match Darklang.

| Bug | Skip Reason | Description |
|-----|-------------|-------------|
| Division `/` | `semantic:division` | Integer vs float division |
| Modulo `%` | `semantic:modulo` | Negative number handling |
| indexOf | `stdlib:indexOf` | Returns Int64, should return Option |
| list_accessors | `stdlib:list_accessors` | head/tail/last signature diffs |
| Float precision | `eval:float_precision` | High-precision floats have different representation |

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

### 2.3 indexOf

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

### 2.4 List Accessors (head, tail, last, init)

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

Internal implementation details that only exist in this compiler. The interpreter has no
equivalent - these are inherent compiler/interpreter differences.

| Feature | Skip Reason | Description |
|---------|-------------|-------------|
| Internal functions | `internal:helper_function` | Functions like `__digitToString`, `__findFrom` are implementation helpers |
| FingerTree/HAMT | `internal:data_structure` | `Stdlib.__FingerTree` and `Stdlib.__HAMT` are internal implementations |

---

## 5. Missing from Interpreter

Features implemented in this compiler that don't exist in the Darklang interpreter yet.
These could potentially be added to the interpreter.

| Feature | Skip Reason | Functions |
|---------|-------------|-----------|
| Bitwise operators | `semantic:bitwise` | `<<`, `>>`, `&`, `\|`, `^`, `~` |
| Boolean not | `semantic:boolean_not` | `!` |
| Random | `stdlib:random` | `Random.int64` |
| Byte operations | `stdlib:byte_ops` | `getByteAt`, `setByteAt`, `appendByte`, `fromBytes`, `toBytes` |
| Int64 math | `stdlib:int64_math` | `Int64.sub`, `Int64.mul`, `Int64.div`, `Int64.isEven`, `Int64.isOdd` |
| List functions | `stdlib:missing` | `List.take`, `List.drop` |
| String functions | `stdlib:missing` | `String.substring`, `String.take`, `String.drop` |
| slice | `stdlib:slice` | Uses (start, length) semantics vs Darklang's (start, end) |

