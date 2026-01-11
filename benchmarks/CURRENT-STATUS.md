# Benchmark Suite

Micro-benchmark suite for the Darklang compiler, inspired by standard micro-benchmark suites.

## Sources

- [Computer Language Benchmarks Game](https://benchmarksgame-team.pages.debian.net/benchmarksgame/)
- [kostya/benchmarks](https://github.com/kostya/benchmarks)
- [Programming Language Benchmarks](https://programming-language-benchmarks.vercel.app/)
- [plb2 (Programming Language Benchmark v2)](https://github.com/attractivechaos/plb2)
- [Julia Micro-Benchmarks](https://julialang.org/benchmarks/)

---

## Currently Working ✓

These benchmarks compile and run reliably.

| Benchmark     | Category       | What It Tests                           |
| ------------- | -------------- | --------------------------------------- |
| factorial     | Recursion      | Basic recursion, multiplication         |
| fib           | Recursion      | Exponential recursion, addition         |
| sum_to_n      | Tail Recursion | Tail call optimization (currently slow) |
| ackermann     | Deep Recursion | Extreme recursion depth, call overhead  |
| tak           | Recursion      | Takeuchi function, nested calls         |
| binary_trees  | Memory         | Heap allocation, tree traversal         |
| primes        | Arithmetic     | Integer ops, conditionals, loops        |
| collatz       | Iteration      | Collatz sequence steps                  |
| leibniz       | Numerical      | Float arithmetic, pi approximation      |
| nqueen        | Backtracking   | N-Queens via bitwise operations         |
| merkletrees   | Tree/Hashing   | Recursive tree hashing                  |
| spectral_norm | Numerical      | Float array operations                  |
| matmul        | Matrix         | Matrix multiplication                   |

---

## CRITICAL BUG: Negative Floats in Recursive Functions

**Status: CAUSES WRONG RESULTS**

Negative float values passed to recursive functions with 4+ float parameters produce incorrect results.

### Affected Benchmarks:

| Benchmark  | Impact                                                  |
| ---------- | ------------------------------------------------------- |
| mandelbrot | Outputs 24091 instead of correct 15909 for 200x200 grid |

### Minimal Reproduction:

```dark
def iterate(cr: Float, ci: Float, zr: Float, zi: Float, iter: Int64, maxIter: Int64) : Int64 =
    if iter >= maxIter then 0
    else
        let zr2 = zr * zr in
        let zi2 = zi * zi in
        if zr2 + zi2 > 4.0 then 1  // BUG: This incorrectly returns 1 when cr is negative
        else iterate(cr, ci, zr, zi, iter + 1, maxIter)

// Returns 1 (WRONG - should be 0 since 0*0 + 0*0 = 0 < 4)
iterate(0.0 - 1.5, 0.0, 0.0, 0.0, 0, 3)

// Returns 0 (CORRECT)
iterate(1.5, 0.0, 0.0, 0.0, 0, 3)
```

### Notes:

- Bug only manifests when first float parameter is negative
- Works correctly with positive values or fewer parameters

---

## BUG: Closure Variable Capture

**Status: BLOCKS quicksort**

Closures cannot capture variables from enclosing scopes properly. The quicksort benchmark fails with "Undefined variable: pivot" because the closures passed to `filter` reference a variable defined in the outer `let` binding.

### Minimal Reproduction:

```dark
def quicksort(arr: List<Int64>) : List<Int64> =
    let len = Stdlib.List.length<Int64>(arr) in
    if len <= 1 then arr
    else
        let pivot = getAtOrDefault(arr, len / 2, 0) in
        // BUG: These closures cannot access 'pivot' from outer scope
        let left = Stdlib.List.filter<Int64>(arr, (x: Int64) => x < pivot) in
        ...
```

---

## Implemented But Limited

These benchmarks have implementations but are limited by stack depth or bugs.

| Benchmark  | Status             | Limitation                                                     |
| ---------- | ------------------ | -------------------------------------------------------------- |
| mandelbrot | WRONG OUTPUT       | Outputs 24091 instead of correct 15909 (negative float bug)    |
| quicksort  | COMPILE ERROR      | "Undefined variable: pivot" - closure variable capture bug     |
| pisum      | Working (reduced)  | Uses 5 rounds, n=1000 (full size causes stack overflow)        |
| nsieve     | Stack overflow     | Uses n=1000 (n=100000 causes stack overflow) - outputs 168     |
| fannkuch   | Stack overflow     | Uses n=6 (n=9 causes stack overflow) - outputs 10              |
| edigits    | Stack overflow     | Uses 50 digits, 1 iteration (full: 1000 digits, 10 iterations) |
| fasta      | Stack overflow     | Uses n=100 (n=100000 causes stack overflow)                    |

---

## Not Implemented

These benchmarks are in the suite for other languages but don't have Dark implementations:

| Benchmark | Notes                                             |
| --------- | ------------------------------------------------- |
| nbody     | No Dark implementation exists                     |

---

## Feature Requirements Summary

| Feature                    | Benchmarks Blocked                                                           |
| -------------------------- | ---------------------------------------------------------------------------- |
| **Negative float bug**     | mandelbrot                                                                   |
| **Closure variable capture** | quicksort                                                                  |
| Stack depth / TCO          | pisum (full), nsieve (full), fannkuch (full), edigits (full), fasta (full)  |

---

## Notes

- Expected outputs for reduced-size benchmarks need to be updated to match reduced parameters
- The non-deterministic segfault bug mentioned in previous versions appears to be fixed
