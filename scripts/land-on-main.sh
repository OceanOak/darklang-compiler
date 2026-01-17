#!/bin/bash
# Pre-merge validation script
# Run this before landing changes on main to ensure quality gates pass.
#
# Usage: ./scripts/land-on-main.sh

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
BOLD='\033[1m'
NC='\033[0m'

echo -e "${BOLD}Pre-merge validation${NC}"
echo ""

# Track failures
FAILURES=()

# 1. Run tests
echo -e "${BOLD}[1/2] Running tests...${NC}"
if "$PROJECT_ROOT/run-tests" 2>/dev/null; then
    echo -e "${GREEN}OK${NC} Tests passed"
else
    echo -e "${RED}FAIL${NC} Tests failed"
    FAILURES+=("tests")
fi

# 2. Run quick benchmark check
echo ""
echo -e "${BOLD}[2/2] Checking for benchmark regressions...${NC}"
if "$PROJECT_ROOT/benchmarks/quick_check.sh"; then
    : # Output already printed by quick_check.sh
else
    FAILURES+=("benchmarks")
fi

# Summary
echo ""
if [ ${#FAILURES[@]} -eq 0 ]; then
    echo -e "${GREEN}${BOLD}All checks passed - safe to land on main${NC}"
    exit 0
else
    echo -e "${RED}${BOLD}Checks failed: ${FAILURES[*]}${NC}"
    echo "Fix issues before landing on main."
    exit 1
fi
