# Benchmark Results

Best-known compatible routine-profile Dark performance vs audited Rust references (instruction counts).

**Snapshot timestamp:** 2026-09-12T06:53:55+00:00
**Architecture:** `arm64`
**Profile:** `routine` (schema 2)
**Measurement policy:** `cachegrind-ir-v1:cache-sim=yes,branch-sim=yes,extract=summary-I-refs`
**Workload contract:** `b4e6ba180a5f68f107005cf8867371144b93d3ed5326517cea45284c74fa6cc2`
**Compiler commit:** `648f7e2392aa78c202c4e014312d5039880998f3` - Integrate archived multiply-subtract loop copy-back optimization

| Benchmark | Dark (3.61x) | Rust |
|---|---:|---:|
| ackermann | 9,303,384,893 (1.86x) | 5,009,840,894 |
| binary_trees | 537,698,640 (0.29x) | 1,842,793,797 |
| collatz | 70,196,392 (0.91x) | 76,734,720 |
| edigits | 3,364,448,919 (247x) | 13,624,986 |
| factorial | 67,431 (0.26x) | 257,875 |
| fasta | 458,941,566 (21.4x) | 21,446,459 |
| fib | 388,196,652 (1.42x) | 272,528,736 |
| leibniz | 850,009,237 (1.21x) | 700,257,860 |
| mandelbrot | 15,230,955 (1.21x) | 12,554,856 |
| matmul | 931,429,268 (54.9x) | 16,960,641 |
| merkletrees | 193,343,665 (1.71x) | 113,305,941 |
| nbody | 883,009,458 (4.24x) | 208,256,292 |
| nqueen | 207,723,575 (1.26x) | 164,530,821 |
| pisum | 40,027,691 (0.88x) | 45,259,394 |
| primes | 1,470,326 (1.17x) | 1,251,693 |
| quicksort | 179,366,191 (27.4x) | 6,544,669 |
| spectral_norm | 63,964,730 (12.6x) | 5,095,834 |
| sum_to_n | 65,679 (0.25x) | 257,825 |
| tak | 48,023,708 (1.22x) | 39,338,221 |
| tinytemplate | 1,087,365,441 (1759x) | 618,292 |
