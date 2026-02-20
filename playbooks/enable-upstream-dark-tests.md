# Upstream `.dark` Tests Enablement Playbook
## Goal
Enable all upstream `.dark` tests by default, one test at a time, while keeping the main suite reliable.

## Contract (Non-Negotiable)
1. Upstream test expression semantics, expected values, and expected error text/meaning are the contract.
2. Replace `Builtin.testDerrorMessage` assertions with `error="..."` assertions carrying the same message (or equivalent wording).
3. Never edit assertions to match current compiler behavior when that changes test behavior.
4. Fix compiler/runtime/parser behavior to satisfy upstream tests.
5. If a case cannot be fixed in scope, keep it disabled and record the blocker.

## Working Rules
1. Enable exactly one skipped test in one `.dark` file per iteration.
2. Run tests immediately after enabling it.
3. If it fails, fix compiler/runtime/parser behavior so the enabled test matches existing upstream test data.
4. Convert `Builtin.testDerrorMessage` assertions to `error="..."` with the same message intent.
5. Do not rewrite expected behavior/output/error text to match current compiler behavior.
6. If not fixable in scope, re-disable the same case and record why.
7. Record every learning in this file.
8. If blocked and guidance is needed, ask user and record both question and answer here.

## Iteration Loop (Single-Test)
1. Pick one disabled upstream case (`skip=...` or a commented assertion) from a `.dark` file.
2. Uncomment/enable the existing upstream assertion; if it uses `Builtin.testDerrorMessage`, rewrite it to an equivalent `error="..."` assertion.
3. Run full `./run-tests` to validate the local change.
4. Diagnose with `./dark --syntax=interpreter -r -e '<expr>'` when output/error text is unclear.
5. Resolve by fixing compiler/runtime/parser behavior to satisfy the existing assertion contract.
6. If not fixable in scope, re-disable only that case and document the blocker in this file.
7. Re-run full `./run-tests`.
8. Commit

## Default Rollout Gate
1. Keep default inclusion scoped while a file still has unresolved failures; use `./dark -r -e ...` probes for fast diagnosis.
2. When a file reaches the desired state (no unresolved failures for the enabled tests in that file), move it toward default execution by updating `src/Tests/TestRunner.fs` include logic.
3. Re-run full `./run-tests` after any `TestRunner` inclusion change.
4. Record the file-level rollout decision in the Learnings Log.

## Order:
Go in this order for re-enabling tests
- src/Tests/e2e/upstream/language/apply/eapply.dark
- src/Tests/e2e/upstream/language/custom-data/aliases.dark
- src/Tests/e2e/upstream/language/flow-control/epipe.dark
- src/Tests/e2e/upstream/language/elambda.dark
- src/Tests/e2e/upstream/stdlib/list.dark
- src/Tests/e2e/upstream/stdlib/string.dark
- src/Tests/e2e/upstream/stdlib/date.dark
- src/Tests/e2e/upstream/stdlib/dict.dark
- src/Tests/e2e/upstream/stdlib/float.dark
- src/Tests/e2e/upstream/stdlib/http.dark
- src/Tests/e2e/upstream/stdlib/json.dark
- src/Tests/e2e/upstream/stdlib/alt-json.dark
- src/Tests/e2e/upstream/stdlib/nomodule.dark
- src/Tests/e2e/upstream/language/derror.dark
- src/Tests/e2e/upstream/language/interpreter.dark
- src/Tests/e2e/upstream/stdlib/httpclient.dark

## Learnings Log
| Situation | How to Identify It | What to Do | How to Validate |
| --- | --- | --- | --- |
| Upstream assertion semantics/text are the contract | Enabled case fails because stderr/value differs from upstream expectation. | Fix compiler/runtime/parser behavior; do not rewrite assertions to fit current output. | Re-run full `./run-tests`. |
| `Builtin.testDerrorMessage` migration | Assertion uses helper form. | Replace with equivalent `error="..."` message text. | Re-run full `./run-tests` and confirm same failure path. |
| One-case iteration scope | Nearby commented/skipped cases tempt batch edits. | Enable one case in one file only; if blocked, re-disable only that case and log blocker. | Re-run full `./run-tests`. |
| Default rollout gating | File still has unresolved failures or `--filter=<file>.dark` reports `0/0` (not runner-wired). | Keep file out of default `TestRunner` include logic until runner wiring + stability are proven. | Confirm with full `./run-tests` before and after inclusion changes. |
| Probe-first debugging | Full test failure does not clearly show parser/type/runtime root cause. | Use `./dark --syntax=interpreter -r -e '<expr>'` (and compiler syntax probes when relevant). | Apply fix, then re-run full `./run-tests`. |
| Parser blockers are real blockers (no test rewrites) | Upstream syntax forms fail to parse (for example `<'a>`, `{}`, backticked fields, multiline forms). | Implement parser support in the correct parser (`1_InterpreterParser.fs` for interpreter syntax) or keep the case disabled with a blocker note. | Add focused parser tests, then run full `./run-tests`. |
| `eapply.dark` apostrophe type-var parsing is fixed, but preamble parsing is still blocked | After wiring `eapply.dark` into `upstreamDarkPaths`, `./run-tests --filter=eapply.dark` now fails with `Preamble parse error: Expected '=' after let binding pattern` (the earlier `<'a>` char-literal blocker no longer appears). | Keep `eapply.dark` out of runner wiring/default while interpreter preamble parsing lacks support for this upstream module/definition shape; re-disable the attempted enablement. | Re-run `./run-tests --filter=eapply.dark` when touching parser behavior, and keep full `./run-tests` green between attempts. |
| Legacy runtime/stdlib names may require compatibility shims | `Undefined variable` for upstream names (`*_v0` or legacy module paths). | Add compatibility aliases/lookup support; do not rewrite upstream source to new names. | Reproduce targeted case, then run full `./run-tests`. |
| `Builtin.testRuntimeError` contracts must survive typing/short-circuiting | Branch/boolean/arith cases fail with static type errors instead of uncaught-exception behavior. | Preserve upstream runtime-error behavior and adjust typing/short-circuit rules accordingly. | Enable one failing case and run full `./run-tests`. |
| Pattern mismatch diagnostics must stay contract-compatible | Generic mismatch text appears where upstream expects `Cannot match ...` wording. | Preserve legacy-compatible mismatch messages for scalar/list/tuple/constructor cases. | Re-run full `./run-tests` and confirm exact stderr. |
| Lambda-list equality remains a known blocker | Lambda-containing equality assertions fail from nominal lambda identity/type mismatch. | Keep those cases disabled until function-value equality semantics are explicitly defined. | Log blocker and keep default suite green with full `./run-tests`. |
| `String.head` ZWJ/char contract is unresolved | Current behavior shows first-codepoint `Option<String>` while upstream cases expect `Option<Char>` semantics. | Keep affected assertions disabled until `Char` payload support + `String.head` contract are decided and implemented. | Probe with `./dark ... -e`, then run full `./run-tests`. |
| Date/time fixed-value assertions need deterministic semantics | `today`-style assertions rely on wall-clock date and/or missing legacy APIs. | Keep disabled until deterministic clock strategy and `Stdlib.DateTime.*_v0` compatibility are defined. | Document blocker and run full `./run-tests`. |
| `http.dark` `setCookie` remains parser-blocked | `{}` / `{ field = value }` / backticked-field expressions fail before runtime behavior is exercised. | Keep cases disabled and track parser-support work; do not rewrite source forms. | Preserve probe evidence and re-run full `./run-tests`. |
| `json.dark` generic cases are partially unblocked but still blocked by runtime exposure | Generic call form parses but `Stdlib.Json.*` names are unresolved. | Keep those cases disabled until compatibility/runtime exposure is implemented. | Re-probe unresolved symbol and run full `./run-tests`. |

## Guidance Log (Paul)
| Date | Question Asked | Why It Was Needed | Paul’s Guidance | Applied |
| --- | --- | --- | --- | --- |
| _pending_ | _none yet_ | _n/a_ | _n/a_ | _n/a_ |
