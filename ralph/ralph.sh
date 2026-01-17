#!/bin/bash
# ralph.sh - Run a playbook
# Usage: ./ralph.sh <playbook> [--iterations=N] [--dry-run]

set -euo pipefail

playbook="$1"; shift
iterations=1
dry_run=false

for arg in "$@"; do
    case $arg in
        --iterations=*) iterations="${arg#*=}" ;;
        --dry-run) dry_run=true ;;
    esac
done

prompt=$(cat "$playbook")

# If prompt contains {{benchmark_name}}, find an unanalyzed benchmark
if [[ "$prompt" == *"{{benchmark_name}}"* ]]; then
    for bench_dir in benchmarks/problems/*/dark/main.dark; do
        bench=$(basename "$(dirname "$(dirname "$bench_dir")")")
        if [[ ! -f "docs/investigations/benchmark-${bench}-optimization.md" ]]; then
            prompt="${prompt//\{\{benchmark_name\}\}/$bench}"
            break
        fi
    done
fi

for ((i=1; i<=iterations; i++)); do
    if [[ "$dry_run" == true ]]; then
        echo "=== Would send to Claude ==="
        echo "$prompt"
        exit 0
    fi

    echo "=== Iteration $i of $iterations ==="
    claude --dangerously-skip-permissions -p "$prompt"
done
