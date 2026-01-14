#!/bin/bash
# ralph-benchmark.sh
# Usage: ./ralph-benchmark.sh [iterations]
#
# Investigates benchmark performance gaps and creates optimization issues.

set -e

iterations="${1:-1}"

for ((i=1; i<=$iterations; i++)); do
  echo "=== Benchmark Optimization Iteration $i/$iterations ==="

  result=$(codex exec --dangerously-bypass-approvals-and-sandbox -m gpt-5.2-codex \
"BENCHMARK OPTIMIZATION INVESTIGATION

## Your Task
Investigate why the Dark compiler performs worse than Rust and OCaml on benchmarks, and generate detailed optimization suggestions.

## Phase 1: Select Random Benchmark

Pick a RANDOM benchmark by running:
\`\`\`bash
ls benchmarks/problems/*/dark/main.dark | shuf -n 1 | xargs dirname | xargs dirname | xargs basename
\`\`\`

Use whatever benchmark is selected - do NOT cherry-pick.

## Phase 2: Gather All Compiler Output

For the selected benchmark, dump all intermediate representations:

### 2a. Dark Compiler IRs
\`\`\`bash
dark -vvv --dump-anf --dump-mir --dump-lir benchmarks/problems/{BENCHMARK}/dark/main.dark -o /tmp/dark_bench 2>&1 | tee /tmp/dark_ir_dump.txt
\`\`\`

### 2b. Rust Disassembly
\`\`\`bash
cd benchmarks/problems/{BENCHMARK}/rust && cargo build --release 2>/dev/null
objdump -d target/release/{BENCHMARK} > /tmp/rust_disasm.txt
\`\`\`

### 2c. OCaml Disassembly
\`\`\`bash
cd benchmarks/problems/{BENCHMARK}/ocaml
ocamlfind ocamlopt -O3 -o bench main.ml 2>/dev/null
objdump -d bench > /tmp/ocaml_disasm.txt
\`\`\`

## Phase 3: Analysis

Analyze the gathered data:
- Compare instruction counts in hot loops
- Identify missing optimizations in Dark IRs
- Find inefficient patterns vs Rust/OCaml
- Quantify potential improvements

## Phase 4: Generate Optimization Suggestions

For each optimization opportunity, document:
1. Title and impact estimate
2. Root cause with IR/asm evidence
3. Implementation approach
4. Files to modify

## Phase 5: Create Investigation Document

Write findings to: docs/investigations/benchmark-{BENCHMARK}-optimization.md

## Phase 6: Create Beads Issues

For EACH optimization suggestion, create a bd issue:
\`\`\`bash
bd create --title=\"Optimization: {TITLE}\" --type=task --priority=2 --description=\"{DETAILED_DESCRIPTION_WITH_EVIDENCE}\"
\`\`\`

## Phase 7: Completion Check

Verify:
- [ ] Investigation document created
- [ ] At least 2 optimization suggestions identified
- [ ] Each suggestion has a bd issue with IR/asm evidence

If all checks pass, output: <promise>OPTIMIZATION_COMPLETE</promise>
")

  echo "$result"

  if [[ "$result" == *"<promise>OPTIMIZATION_COMPLETE</promise>"* ]]; then
    echo "Benchmark optimization complete, exiting."
    exit 0
  fi
done

echo "Completed $iterations iterations."
