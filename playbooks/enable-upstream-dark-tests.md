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
4. Record the file-level rollout decision in the Progress Log.

## Progress Log
| Date | File | Test | Change | Result | Notes |
| --- | --- | --- | --- | --- | --- |
| 2026-02-15 | `src/Tests/e2e/upstream/language/flow-control/eif.dark` | `L3: (if true then Builtin.testRuntimeError "a" else 0L)` | Replaced `skip="Builtin.testRuntimeError is not supported yet"` with `exit=1 stderr="Uncaught exception: a"` | Enabled and passing in filtered run (`6 passed, 4 failed, 6 skipped`) | This confirms `Builtin.testRuntimeError` is executable in this scenario; skip reason was stale for this case. |

## Learnings Log
| Date | Learning | Evidence |
| --- | --- | --- |
| 2026-02-15 | In `eif.dark`, one previously skipped runtime-error test can be enabled using explicit `exit/stderr` expectations. | `./dark -q -e '(if true then Builtin.testRuntimeError "a" else 0L)' --syntax=interpreter` binary run produced exit `1` and stderr `Uncaught exception: a`. |
| 2026-02-15 | Current blocker in same file is mostly diagnostic-text drift for type errors, not execution crashes. | Filtered run still fails at `L1`, `L17`, `L18`, `L19` with `Expected error message ... not found in stderr`. |

## Guidance Log (Paul)
| Date | Question Asked | Why It Was Needed | Paul’s Guidance | Applied |
| --- | --- | --- | --- | --- |
| _pending_ | _none yet_ | _n/a_ | _n/a_ | _n/a_ |

## Next Candidate Tests
1. `eif.dark:L5` (same runtime-error family as L3)
2. `eif.dark:L7`
3. `eif.dark:L8`

These are likely the fastest follow-ups because they are adjacent to the now-enabled L3 case.
