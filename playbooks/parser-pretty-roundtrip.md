# Ralph Loop: Parser/Pretty-Printer Roundtrip

Goal: find parser/pretty-printer mismatches using real `.e2e` programs, and fix exactly one mismatch per iteration.

Only actionable unit:
1. One failing roundtrip case (not a batch of cases).

Prep (once per session)
1. `git rebase main`
2. Read `docs/adding-features.md` and `docs/result-patterns.md`.
3. Confirm key files:
   - `src/DarkCompiler/passes/1_Parser.fs`
   - `src/DarkCompiler/passes/1_InterpreterParser.fs`
   - `src/DarkCompiler/ASTPrettyPrinter.fs`
   - `src/Tests/SyntaxInteropTests.fs`
   - `src/Tests/TestDSL/E2EFormat.fs`
4. Ensure a corpus test suite exists at `src/Tests/SyntaxRoundtripCorpusTests.fs` and is registered in `src/Tests/TestRunner.fs`.
5. Corpus rules for that suite:
   - Use all files under `src/Tests/e2e/**/*.e2e`.
   - Syntax mode: `/interpreter/` => `CompilerLibrary.InterpreterSyntax`, otherwise `CompilerLibrary.CompilerSyntax`.
   - Internal identifiers: `/stdlib-internal/` => `allowInternal = true`, otherwise `allowInternal = false`.
   - Parse tests via `TestDSL.E2EFormat.parseE2ETestFile`.
   - Skip only `ExpectCompileError = true`.
   - Parse input is `test.Source` when preamble is empty, otherwise `test.Preamble + "\n" + test.Source`.
6. Roundtrip invariant (same syntax on both parses):
   - Parse original source.
   - Pretty-print.
   - Parse pretty output.
   - Pretty-print again.
   - Fail on any of: original parse failure, pretty parse failure, `ast0 <> ast1`, or `printed1 <> printed2`.
   - Failure report must include source file, test name, syntax mode, `allowInternal`, original text, printed text, and parse errors.

Loop (single Ralph iteration)
1. Run `./run-tests` and pick exactly one failing corpus case.
2. Classify the failure:
   - `ParseOriginalFailed`
   - `ParsePrettyFailed`
   - `AstChangedAfterRoundtrip`
   - `PrettyNotIdempotent`
3. Add a minimal failing regression test first.
4. Prefer `src/Tests/e2e/` for user-visible syntax behavior; use `src/Tests/SyntaxInteropTests.fs` only for tightly focused parser/printer rules.
5. Run `./run-tests` and confirm the new regression test fails.
6. Implement the smallest fix in parser and/or pretty-printer.
7. Run `./run-tests` and confirm all tests pass.
8. Keep the corpus suite enabled; do not weaken or bypass failures.
9. Commit with message `Fix parser/pretty roundtrip: <case>`.
10. End the iteration. Start the next iteration only after this one is clean.

Triage map
1. `ParseOriginalFailed`: parser bug or invalid test-selection rule.
2. `ParsePrettyFailed`: pretty-printer emitted invalid syntax.
3. `AstChangedAfterRoundtrip`: precedence/associativity/pattern/literal mismatch.
4. `PrettyNotIdempotent`: unstable formatting rule.

Common fix locations
1. Compiler syntax parser: `src/DarkCompiler/passes/1_Parser.fs`
2. Interpreter syntax parser: `src/DarkCompiler/passes/1_InterpreterParser.fs`
3. Pretty-printer canonicalization: `src/DarkCompiler/ASTPrettyPrinter.fs`

Stop criteria
1. Fix requires broad multi-pass refactors rather than a single bug fix.
2. Behavior is ambiguous or undocumented.
3. You cannot produce a minimal failing regression test for the chosen failure.
4. The selected corpus case is invalid and no valid replacement is identified quickly.

Session wrap-up (after one or more completed iterations)
1. Run full tests: `./run-tests`
2. Run full benchmarks: `./benchmarks/run_benchmarks.sh`
3. Report:
   - Corpus cases roundtripped
   - Failures fixed
   - Regression tests added
   - `Performance ratio: X.XX` from `benchmarks/RESULTS.md`

Do not land without explicit approval. If asked to land, follow repository landing flow via `scripts/land-on-main.sh`.
