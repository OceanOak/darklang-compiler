# Parser/Pretty Roundtrip Playbook

Goal: fix parser/pretty-printer roundtrip failures one-by-one, always starting from the first failing corpus case.

## Run the opt-in corpus test
1. Run:
   - `./run-tests --parser-pretty-roundtrip`
2. The suite is disabled by default and only runs with that flag.
3. The suite stops at the first failure and prints:
   - failure kind (`ParseOriginalFailed`, `ParsePrettyFailed`, `AstChangedAfterRoundtrip`, `PrettyNotIdempotent`)
   - file name
   - test name
   - snippet type (`preamble`, `source`, or `expected-value`)
   - syntax mode and `allowInternal` setting
   - original and pretty-printed code (plus parse error when relevant)

## Fix one failure at a time
1. Take only the first failing case from `./run-tests --parser-pretty-roundtrip`.
2. Add a minimal regression test first:
   - Prefer `src/Tests/e2e/*.e2e` when it is user-visible syntax behavior.
   - Use `src/Tests/SyntaxInteropTests.fs` only for narrowly scoped parser/printer rules.
3. Confirm the regression fails:
   - `./run-tests`
4. Implement the smallest fix in:
   - `src/DarkCompiler/passes/1_Parser.fs`
   - `src/DarkCompiler/passes/1_InterpreterParser.fs`
   - `src/DarkCompiler/ASTPrettyPrinter.fs`
5. Re-run:
   - `./run-tests`
   - `./run-tests --parser-pretty-roundtrip`
6. Repeat from step 1 for the next failure.

## Notes
1. Keep the corpus suite strict; do not weaken checks.
2. Always let the next iteration start from the first currently failing roundtrip case.
