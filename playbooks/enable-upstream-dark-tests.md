# Upstream `.dark` Tests Enablement Playbook

## Goal
Enable all upstream `.dark` tests by default, one test at a time, while keeping the main suite reliable.

## Working Rules
1. Enable exactly one skipped test in one `.dark` file per iteration.
2. Run tests immediately after enabling it.
3. If it fails, fix compiler/runtime behavior first when feasible.
4. If behavior is already correct and only the assertion is outdated, update the test expectation.
5. Record every learning in this file.
6. If blocked and guidance is needed, ask Paul and record both question and answer here.

## Iteration Loop (Single-Test)
1. Pick one skipped test (`skip=...`) from a `.dark` file.
2. Remove `skip` and replace with the most direct assertion (`= value`, `error="..."`, or `exit/stderr`).
3. Run `./run-tests --filter=<file>.dark` to validate the local change quickly.
4. Diagnose with `./dark --syntax=interpreter -r -e '<expr>'` when output/error text is unclear.
5. Resolve by:
   1. Compiler/runtime fix, or
   2. Test expectation update if runtime/compiler behavior is already correct.
6. Re-run `./run-tests --filter=<file>.dark`.
7. Append results to the logs below.
8. Before landing a batch, run full `./run-tests`.

## Default Rollout Gate
1. Keep using focused enablement (`--filter=<file>.dark`) while a file still has unresolved failures.
2. When a file reaches the desired state (no unresolved failures for the enabled tests in that file), move it toward default execution by updating `src/Tests/TestRunner.fs` include logic.
3. Re-run full `./run-tests` after any `TestRunner` inclusion change.
4. Record the file-level rollout decision in the Learnings Log.

## Learnings Log
| Situation | How to Identify It | What to Do | How to Validate |
| --- | --- | --- | --- |
| Compile-error tests fail because expected message text no longer matches | Focused run shows `Expected error message '...' not found in stderr` while process still exits non-zero during compile stage. | Treat this as diagnostic-text drift first: inspect current compiler error text, then update expectation strings if semantics are unchanged. Only change compiler behavior if message drift reveals a real semantic regression. | After adjusting expectations or code, rerun focused tests; then run full `./run-tests` before landing. |

## Guidance Log (Paul)
| Date | Question Asked | Why It Was Needed | Paul’s Guidance | Applied |
| --- | --- | --- | --- | --- |
| _pending_ | _none yet_ | _n/a_ | _n/a_ | _n/a_ |
