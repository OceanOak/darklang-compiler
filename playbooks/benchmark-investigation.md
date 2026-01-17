BENCHMARK OPTIMIZATION INVESTIGATION

## Your Task
Investigate why the Dark compiler performs worse than Rust and OCaml on the **{{benchmark_name}}** benchmark, and generate detailed optimization suggestions.

## Phase 1: Gather All Compiler Output

For the {{benchmark_name}} benchmark, dump all intermediate representations:

### 1a. Dark Compiler IRs
```bash
dark -vvv --dump-anf --dump-mir --dump-lir benchmarks/problems/{{benchmark_name}}/dark/main.dark -o /tmp/dark_bench 2>&1 | tee /tmp/dark_ir_dump.txt
```

### 1b. Rust Disassembly
```bash
cd benchmarks/problems/{{benchmark_name}}/rust && cargo build --release 2>/dev/null
objdump -d target/release/{{benchmark_name}} > /tmp/rust_disasm.txt
```

### 1c. OCaml Disassembly
```bash
cd benchmarks/problems/{{benchmark_name}}/ocaml
ocamlfind ocamlopt -O3 -o bench main.ml 2>/dev/null
objdump -d bench > /tmp/ocaml_disasm.txt
```

## Phase 2: Analysis

Analyze the gathered data:
- Compare instruction counts in hot loops
- Identify missing optimizations in Dark IRs
- Find inefficient patterns vs Rust/OCaml
- Quantify potential improvements

## Phase 3: Generate Optimization Suggestions

For each optimization opportunity, document:
1. Title and impact estimate
2. Root cause with IR/asm evidence
3. Implementation approach
4. Files to modify

## Phase 4: Create Investigation Document

Write findings to: docs/investigations/benchmark-{{benchmark_name}}-optimization.md

## Phase 5: Completion Check

Verify:
- [ ] Investigation document created
- [ ] At least 2 optimization suggestions identified
- [ ] Each suggestion includes IR/asm evidence
