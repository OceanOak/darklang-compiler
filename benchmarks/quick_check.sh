#!/bin/bash
# Quick benchmark check for regression detection
# Usage: ./benchmarks/quick_check.sh [--fast] [--save-baseline] [--build]
#
# Runs reduced-size benchmarks under cachegrind to detect instruction count regressions.
# Deterministic instruction counts allow precise regression detection.
#
# Options:
#   --fast             Run only 5 key benchmarks (~5s instead of ~20s)
#   --save-baseline    Save current counts as new baseline (run after intentional changes)
#   --build            Force rebuild all benchmarks (otherwise only rebuilds if source changed)

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
source "$SCRIPT_DIR/infrastructure/pretty.sh"

BASELINE_FILE="$SCRIPT_DIR/QUICK_BASELINE.txt"
SAVE_BASELINE=false
FORCE_BUILD=false
FAST_MODE=false
REGRESSION_THRESHOLD=0  # Any increase is a regression (deterministic counts)

# Key benchmarks for fast mode (diverse coverage: recursion, loops, floats, lists, bitops)
FAST_BENCHMARKS="fib ackermann mandelbrot quicksort nqueen"

# Parse options
while [[ $# -gt 0 ]]; do
    case $1 in
        --fast)
            FAST_MODE=true
            shift
            ;;
        --save-baseline)
            SAVE_BASELINE=true
            shift
            ;;
        --build)
            FORCE_BUILD=true
            shift
            ;;
        *)
            echo "Unknown option: $1"
            echo "Usage: $0 [--fast] [--save-baseline] [--build]"
            exit 1
            ;;
    esac
done

# Check for valgrind
if ! command -v valgrind &> /dev/null; then
    pretty_fail "valgrind is not installed"
    pretty_info "Install with: sudo apt-get install valgrind"
    exit 1
fi

if [ "$FAST_MODE" = true ]; then
    pretty_section "Quick benchmark check (fast mode - 5 benchmarks)"
else
    pretty_section "Quick benchmark check for regression detection"
fi
echo ""

# Find all quick.dark files
if [ "$FAST_MODE" = true ]; then
    BENCHMARKS="$FAST_BENCHMARKS"
else
    BENCHMARKS=$(ls -d "$SCRIPT_DIR/problems"/*/dark/quick.dark 2>/dev/null | while read f; do
        basename "$(dirname "$(dirname "$f")")"
    done)
fi

if [ -z "$BENCHMARKS" ]; then
    pretty_fail "No quick.dark files found"
    exit 1
fi

# Load baseline if exists
declare -A BASELINE
if [ -f "$BASELINE_FILE" ] && [ "$SAVE_BASELINE" = false ]; then
    while IFS='=' read -r name count; do
        BASELINE["$name"]="$count"
    done < "$BASELINE_FILE"
fi

# Track results
declare -A RESULTS
FAILURES=()
BUILD_FAILURES=()
TOTAL_INSTRUCTIONS=0
START_TIME=$(date +%s)

for bench in $BENCHMARKS; do
    PROBLEM_DIR="$SCRIPT_DIR/problems/$bench"
    QUICK_DARK="$PROBLEM_DIR/dark/quick.dark"
    QUICK_BIN="$PROBLEM_DIR/dark/quick"

    # Build only if needed (source newer than binary, or --build flag)
    NEEDS_BUILD=false
    if [ "$FORCE_BUILD" = true ]; then
        NEEDS_BUILD=true
    elif [ ! -x "$QUICK_BIN" ]; then
        NEEDS_BUILD=true
    elif [ "$QUICK_DARK" -nt "$QUICK_BIN" ]; then
        NEEDS_BUILD=true
    elif [ "$PROJECT_ROOT/dark" -nt "$QUICK_BIN" ]; then
        NEEDS_BUILD=true
    fi

    if [ "$NEEDS_BUILD" = true ]; then
        if ! "$PROJECT_ROOT/dark" "$QUICK_DARK" -o "$QUICK_BIN" -q 2>/dev/null; then
            BUILD_FAILURES+=("$bench")
            pretty_warn "$bench: build failed"
            continue
        fi
        chmod +x "$QUICK_BIN"
    fi

    # Run under cachegrind
    CG_OUTPUT=$(valgrind --tool=cachegrind --cache-sim=no --branch-sim=no "$QUICK_BIN" 2>&1)
    I_REFS=$(echo "$CG_OUTPUT" | grep "I refs:" | sed 's/.*I refs:[[:space:]]*//' | tr -d ',')

    if [ -z "$I_REFS" ]; then
        FAILURES+=("$bench: cachegrind failed")
        pretty_warn "$bench: cachegrind failed"
        continue
    fi

    RESULTS["$bench"]="$I_REFS"
    TOTAL_INSTRUCTIONS=$((TOTAL_INSTRUCTIONS + I_REFS))

    # Compare against baseline
    if [ -n "${BASELINE[$bench]}" ]; then
        BASELINE_COUNT="${BASELINE[$bench]}"
        DIFF=$((I_REFS - BASELINE_COUNT))
        if [ "$DIFF" -gt "$REGRESSION_THRESHOLD" ]; then
            FAILURES+=("$bench: regression +$DIFF instructions ($BASELINE_COUNT -> $I_REFS)")
            pretty_fail "$bench: $I_REFS (+$DIFF regression)"
        elif [ "$DIFF" -lt 0 ]; then
            pretty_ok "$bench: $I_REFS ($DIFF improvement)"
        else
            pretty_ok "$bench: $I_REFS (unchanged)"
        fi
    else
        pretty_info "$bench: $I_REFS (no baseline)"
    fi
done

END_TIME=$(date +%s)
ELAPSED=$((END_TIME - START_TIME))

echo ""
pretty_info "Total instructions: $TOTAL_INSTRUCTIONS"
pretty_info "Elapsed time: ${ELAPSED}s"

# Save baseline if requested
if [ "$SAVE_BASELINE" = true ]; then
    echo "# Quick benchmark baseline - instruction counts" > "$BASELINE_FILE"
    echo "# Generated: $(date -Iseconds)" >> "$BASELINE_FILE"
    echo "# Compiler: $(git -C "$PROJECT_ROOT" rev-parse --short HEAD)" >> "$BASELINE_FILE"
    for bench in $BENCHMARKS; do
        if [ -n "${RESULTS[$bench]}" ]; then
            echo "$bench=${RESULTS[$bench]}" >> "$BASELINE_FILE"
        fi
    done
    pretty_ok "Baseline saved to $BASELINE_FILE"
fi

# Report failures
if [ ${#BUILD_FAILURES[@]} -ne 0 ]; then
    echo ""
    pretty_fail "Build failures: ${BUILD_FAILURES[*]}"
fi

if [ ${#FAILURES[@]} -ne 0 ]; then
    echo ""
    pretty_fail "Regressions detected:"
    for failure in "${FAILURES[@]}"; do
        echo "  - $failure"
    done
    exit 1
fi

echo ""
pretty_ok "No regressions detected"
