# Benchmark Results

Latest routine-profile Dark performance vs audited Rust references (instruction counts).

**Last Updated:** 2026-08-14 20:05:47
**Commit:** `5b802b64` - Fall through ARM64 true branch targets

| Benchmark     |           Dark (2.56x) |          Rust |
|---------------|------------------------|---------------|
| ackermann     | 10,019,035,943 (2.15x) | 4,651,994,510 |
| binary_trees  |    645,815,426 (0.35x) | 1,842,791,955 |
| collatz       |     81,142,968 (0.83x) |    98,242,178 |
| edigits       |   6,286,781,853 (461x) |    13,637,551 |
| factorial     |         63,906 (0.25x) |       257,669 |
| fasta         |    652,078,652 (32.2x) |    20,252,213 |
| fib           |    567,354,378 (2.20x) |   257,598,132 |
| leibniz       |    900,001,393 (1.12x) |   800,257,637 |
| mandelbrot    |     17,379,453 (1.28x) |    13,595,721 |
| matmul        |   2,038,968,697 (128x) |    15,983,852 |
| merkletrees   |    658,629,002 (5.28x) |   124,776,610 |
| nbody         |  1,216,502,521 (4.91x) |   247,760,534 |
| nqueen        |    230,796,676 (1.65x) |   139,988,273 |
| pisum         |         95,235 (0.00x) |    50,258,602 |
| primes        |      2,045,879 (1.51x) |     1,358,980 |
| quicksort     |    360,891,623 (54.9x) |     6,574,976 |
| spectral_norm |    133,629,025 (25.2x) |     5,297,561 |
| sum_to_n      |         61,766 (0.24x) |       257,603 |
| tak           |     52,360,511 (1.55x) |    33,730,191 |
