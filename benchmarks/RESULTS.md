# Benchmark Results

Latest routine-profile Dark performance vs audited Rust references (instruction counts).

**Last Updated:** 2026-08-14 18:13:22
**Commit:** `98e18b03` - Speed up interpreter parameter parsing

| Benchmark     |           Dark (2.59x) |          Rust |
|---------------|------------------------|---------------|
| ackermann     | 10,019,035,961 (2.15x) | 4,651,994,510 |
| binary_trees  |    649,092,147 (0.35x) | 1,842,791,955 |
| collatz       |     81,142,993 (0.83x) |    98,242,178 |
| edigits       |   6,310,878,694 (463x) |    13,637,551 |
| factorial     |         63,975 (0.25x) |       257,669 |
| fasta         |    656,697,638 (32.4x) |    20,252,213 |
| fib           |    582,284,757 (2.26x) |   257,598,132 |
| leibniz       |    900,001,421 (1.12x) |   800,257,637 |
| mandelbrot    |     17,379,475 (1.28x) |    13,595,721 |
| matmul        |   2,044,461,227 (128x) |    15,983,852 |
| merkletrees   |    658,629,034 (5.28x) |   124,776,610 |
| nbody         |  1,216,502,543 (4.91x) |   247,760,534 |
| nqueen        |    235,397,869 (1.68x) |   139,988,273 |
| pisum         |         95,284 (0.00x) |    50,258,602 |
| primes        |      2,045,890 (1.51x) |     1,358,980 |
| quicksort     |    361,196,969 (54.9x) |     6,574,976 |
| spectral_norm |    134,786,863 (25.4x) |     5,297,561 |
| sum_to_n      |         71,780 (0.28x) |       257,603 |
| tak           |     52,360,519 (1.55x) |    33,730,191 |
