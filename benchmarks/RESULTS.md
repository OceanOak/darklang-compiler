# Benchmark Results

Best-known compatible routine-profile Dark performance vs audited Rust references (instruction counts).

**Snapshot timestamp:** 2026-09-09T19:10:34+00:00
**Architecture:** `arm64`
**Profile:** `routine` (schema 2)
**Measurement policy:** `cachegrind-ir-v1:cache-sim=yes,branch-sim=yes,extract=summary-I-refs`
**Workload contract:** `b4e6ba180a5f68f107005cf8867371144b93d3ed5326517cea45284c74fa6cc2`
**Compiler commit:** `e34e66c5c701647f8aba2a6d67f1e282584874eb` - Accumulate generic type applications in one traversal

| Benchmark | Dark (3.84x) | Rust |
|---|---:|---:|
| ackermann | 9,303,384,893 (1.86x) | 5,009,840,894 |
| binary_trees | 547,528,440 (0.30x) | 1,842,793,797 |
| collatz | 70,196,392 (0.91x) | 76,734,720 |
| edigits | 3,528,831,129 (259x) | 13,624,986 |
| factorial | 67,450 (0.26x) | 257,875 |
| fasta | 477,020,357 (22.2x) | 21,446,459 |
| fib | 388,196,652 (1.42x) | 272,528,736 |
| leibniz | 850,009,239 (1.21x) | 700,257,860 |
| mandelbrot | 16,367,530 (1.30x) | 12,554,856 |
| matmul | 1,941,542,486 (114x) | 16,960,641 |
| merkletrees | 193,343,665 (1.71x) | 113,305,941 |
| nbody | 883,009,464 (4.24x) | 208,256,292 |
| nqueen | 212,398,464 (1.29x) | 164,530,821 |
| pisum | 40,029,693 (0.88x) | 45,259,394 |
| primes | 1,470,326 (1.17x) | 1,251,693 |
| quicksort | 187,792,899 (28.7x) | 6,544,669 |
| spectral_norm | 69,747,736 (13.7x) | 5,095,834 |
| sum_to_n | 75,679 (0.29x) | 257,825 |
| tak | 48,023,708 (1.22x) | 39,338,221 |
| tinytemplate | 1,115,858,537 (1805x) | 618,292 |
