# Benchmark Results

Latest routine-profile Dark performance vs audited Rust references (instruction counts).

**Last Updated:** 2026-08-14 17:12:08
**Commit:** `22e9814f` - Lower integer negation directly

| Benchmark     |           Dark (2.75x) |          Rust |
|---------------|------------------------|---------------|
| ackermann     | 11,450,298,661 (2.46x) | 4,651,994,510 |
| binary_trees  |    675,306,181 (0.37x) | 1,842,791,955 |
| collatz       |     81,543,031 (0.83x) |    98,242,178 |
| edigits       |   6,661,626,266 (488x) |    13,637,551 |
| factorial     |         64,095 (0.25x) |       257,669 |
| fasta         |    714,427,634 (35.3x) |    20,252,213 |
| fib           |    642,006,191 (2.49x) |   257,598,132 |
| leibniz       |    900,001,461 (1.12x) |   800,257,637 |
| mandelbrot    |     17,541,905 (1.29x) |    13,595,721 |
| matmul        |   2,105,972,029 (132x) |    15,983,852 |
| merkletrees   |    724,164,078 (5.80x) |   124,776,610 |
| nbody         |  1,239,502,619 (5.00x) |   247,760,534 |
| nqueen        |    290,612,025 (2.08x) |   139,988,273 |
| pisum         |         95,344 (0.00x) |    50,258,602 |
| primes        |      2,075,904 (1.53x) |     1,358,980 |
| quicksort     |    378,515,575 (57.6x) |     6,574,976 |
| spectral_norm |    143,854,461 (27.2x) |     5,297,561 |
| sum_to_n      |         71,820 (0.28x) |       257,603 |
| tak           |     63,580,599 (1.88x) |    33,730,191 |
