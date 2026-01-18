# Fix one difference with the Darklang interpreter

This compiler aims to match the official Darklang interpreter in both syntax and semantics. The validation script `scripts/validate-darklang.py` identifies tests that may differ by skipping them. Some of these are real differences that need to be fixed.

You are going to fix EXACTLY ONE semtantic difference.

1. Pick a known issue from `docs/darklang-differences.md` Section 2 (Semantic Bugs). These are areas where the compiler produces wrong output that needs to be fixed.

3. Find a test that exercises the bug. Search `src/Tests/e2e/` for tests using the feature. Record:
   - The file name and line number
   - The original expression
   - The expected result
   - The skip reason from the docs

4. Manually convert the expression to Darklang interpreter syntax and run it:

   Create a .dark file:
   ```dark
   // test.dark
   let __result = <converted_expression>
   Builtin.debug "" __result
   0L
   ```
   Then run: `darklang-interpreter run test.dark`

   See the conversions explained in the Syntactic Differences section of docs/darklang-differences.md

5. Compare results. Determine one of:

   - **MATCH**: Results are equivalent - update `validate-darklang.py` to remove this unnecessary skip rule
   - **DIFFERENCE**: Results differ - this needs to be fixed in the compiler
   - **EVAL_LIMITATION**: The interpreter's eval mode doesn't support this syntax - document this in the darklang-differences.md file in the right place.
   - **INTERPRETER_BUG**: If the difference is caused by a Darklang interpreter bug (e.g., `!` behaves as a no-op), document this in the darklang-differences.md file in the right place.

6. If you found a DIFFERENCE, fix the compiler:

   - Locate the relevant implementation (stdlib in `src/DarkCompiler/stdlib/` or operator handling)
   - Understand what the correct Darklang behavior should be
   - Update the E2E test's expected value to match the correct Darklang output
   - Modify the compiler to produce the correct result
   - The Darklang interpreter is authoritative - our compiler must match it

7. Run the test suite (`./run-tests`). If tests fail:

   - Fix the compiler, not the tests (unless a test had the wrong expected value)
   - Repeat until all tests pass

8. Update `scripts/validate-darklang.py` and documentation:

   - If you fixed a semantic difference, remove or narrow the skip rule that was causing this test to be skipped
   - Run the validator again to confirm the test now passes

   **REQUIRED: Update docs/darklang-differences.md:**
   - If you FIXED a semantic bug, remove it from Section 2 (Semantic Bugs)
   - If you discovered something is missing from interpreter, add to Section 5 (Missing from Interpreter)
   - Ensure the skip reason in validate-darklang.py matches a documented category

9. After all tests pass, run all Dark benchmarks (`./benchmarks/run_benchmarks.sh`). Ignore the quicksort failure - it's a known issue. If RESULTS.md changed, show the results.

10. Write a short report to the developer. Include:

    - **Test investigated**: File, line number, expression
    - **Finding**: What differed between this compiler and `darklang-interpreter`
    - **Fix applied**: What changes were made to the compiler
    - **Validation**: Confirm the test now passes both `./run-tests` and `validate-darklang.py`

11. **Documentation Check** (before commit):

    Verify your changes are documented:
    - [ ] Skip rule in validate-darklang.py matches a category in darklang-differences.md
    - [ ] If fixed a semantic bug, removed it from Section 2
    - [ ] If found missing interpreter feature, it's in Section 5

12. DO NOT COMMIT OR MERGE UNTIL I SAY "approved". After that, commit the code and updated tests. Include in the commit message a description of the difference that was fixed. Land using scripts/land-in-main.sh

## Policies for fixing differences

- The `darklang-interpreter` is authoritative. When our compiler differs, our compiler is wrong.
- Fix one difference at a time. If fixing one reveals another, note it but don't fix it in this session.
- If a difference cannot be fixed without major architectural changes, report this and choose a different test.
- If the darklang-interpreter's eval mode doesn't support certain syntax (let bindings, lambdas, etc.), that's an eval limitation, not a real difference. Choose another test.
- Update both the compiler AND the skip rules in validate-darklang.py so the test can be validated going forward.
- When adding or modifying skip rules, document the difference in `docs/darklang-differences.md` with examples and status.
- Some functions exist in this compiler's stdlib but not in Darklang (e.g., helper functions like `digitToString`). Keep those tests and keep/extend `validate-darklang.py` skip rules for them; do not treat their absence in Darklang as a compiler bug or change the tests to expect errors.
- Internal stdlib helper tests (identifiers starting with `__`) must live under `src/Tests/e2e/stdlib-internal/` so the test runner enables internal identifiers.

