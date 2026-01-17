# Benchmark Optimization Investigation

## Your Task

Pick a benchmark from `benchmarks/problems/` and investigate optimization opportunities. If new insights are found, update the existing investigation document.

## Phase 1: Select a Benchmark

1. List all benchmarks: `ls benchmarks/problems/`
2. Pick one at random (not deterministically)
3. Read its existing investigation at `docs/investigations/benchmark-<name>-optimization.md`

## Phase 2: Gather Compiler Output

For your chosen benchmark, dump all intermediate representations:

### 2a. Dark Compiler IRs
```bash
dark -vvv --dump-anf --dump-mir --dump-lir benchmarks/problems/<benchmark>/dark/main.dark -o /tmp/dark_bench 2>&1 | tee /tmp/dark_ir_dump.txt
```

### 2b. Rust Disassembly
```bash
cd benchmarks/problems/<benchmark>/rust && cargo build --release 2>/dev/null
objdump -d target/release/<benchmark> > /tmp/rust_disasm.txt
```

### 2c. OCaml Disassembly
```bash
cd benchmarks/problems/<benchmark>/ocaml
ocamlfind ocamlopt -O3 -o bench main.ml 2>/dev/null
objdump -d bench > /tmp/ocaml_disasm.txt
```

## Phase 3: Analysis

Compare against the existing investigation document:
- Are there new optimization opportunities not yet documented?
- Have any documented optimizations been implemented? Update their status.
- Are there new patterns in the IR/asm worth noting?

## Phase 4: Update Investigation Document

If you found anything new:
- Add new optimization suggestions with IR/asm evidence
- Update status of existing suggestions if they've been implemented
- Add any new insights about the benchmark

If nothing new was found, report that and move on.

## Phase 5: Summary

Report:
- Which benchmark was analyzed
- What new findings (if any) were added
- Current state of the investigation document
