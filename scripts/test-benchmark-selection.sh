#!/bin/bash
# test-benchmark-selection.sh
# Basic check that run_benchmarks.sh skips known-bad benchmarks.

set -euo pipefail

repo_root="$(git rev-parse --show-toplevel)"
benchmarks="$(
  cd "$repo_root"
  ./benchmarks/run_benchmarks.sh --list
)"

if echo "$benchmarks" | grep -q "^quicksort$"; then
  echo "FAIL: quicksort should be skipped"
  exit 1
fi

if ! echo "$benchmarks" | grep -q "^ackermann$"; then
  echo "FAIL: expected ackermann in benchmark list"
  exit 1
fi

echo "OK"
