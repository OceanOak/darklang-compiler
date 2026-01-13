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
| mandelbrot    | Numerical      | Complex number iteration, fractal       |

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
| quicksort  | COMPILE ERROR      | "Undefined variable: pivot" - closure variable capture bug     |
| pisum      | Working (reduced)  | Uses 5 rounds, n=1000 (full size causes stack overflow)        |
| nsieve     | Stack overflow     | Uses n=1000 (n=100000 causes stack overflow) - outputs 168     |
| fannkuch   | Stack overflow     | Uses n=6 (n=9 causes stack overflow) - outputs 10              |
| edigits    | Stack overflow     | Uses 50 digits, 1 iteration (full: 1000 digits, 10 iterations) |

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
| **Closure variable capture** | quicksort                                                                  |
| Stack depth / TCO          | pisum (full), nsieve (full), fannkuch (full), edigits (full)               |

---

## Notes

- Expected outputs for reduced-size benchmarks need to be updated to match reduced parameters
- The non-deterministic segfault bug mentioned in previous versions appears to be fixed
- The mandelbrot "negative float bug" was actually a semantic mismatch - the Dark code was counting escaped points while the Rust reference counts points in the set. Fixed.
