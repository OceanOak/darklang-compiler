# Upstream E2E Enablement Test Plan

Date: 2026-03-06

Disabled files checked:
- `language/apply/eapply.dark`
- `language/custom-data/aliases.dark`
- `language/flow-control/epipe.dark`
- `language/derror.dark`
- `language/elambda.dark`
- `stdlib/date.dark`

Method:
1. Add the target file to `defaultUpstreamDarkPaths` in `src/Tests/test-suite-tooling/TestRunner.fs`.
2. Remove its per-file line allowlist entry (if present) from `upstreamEnablementLineAllowlist`.
3. Run `./run-tests`.
4. Keep enablement only if green; otherwise restore baseline and record blockers.

## `language/apply/eapply.dark` (currently disabled)

Status: blocked.

Blocking tests/reasons:
- Roundtrip parser test fails at `L1` (`AstChangedAfterRoundtrip`).
- E2E preamble build fails for the suite with: `Type mismatch in integer literal: expected String, got Int64`.
- Representative failing tests from this file when fully enabled: `L1, L2, L3, L6, L9, L11, L18, L19, L21, L22, L25, L26, L30, L39, L43, L47, L51, L55, L60, L65, L69, L75, L76`.

## `language/custom-data/aliases.dark` (currently disabled)

Status: blocked.

Blocking tests/reasons:
- Roundtrip parser fails for `L48` preamble snippet with: `Expected ',' or '}' after record field`.
- E2E preamble type error for the suite: `Cannot apply non-function type: String`.
- Representative failing tests from this file when fully enabled: `L1, L2, L3, L6, L9, L11, L13, L14, L16, L17, L18, L19, L21, L22, L25, L26, L30, L39, L43, L47, L51, L55, L60, L65, L69, L75, L76`.

## `language/flow-control/epipe.dark` (currently disabled)

Status: blocked.

Blocking tests:
- `L9` value mismatch.
- `L11` expected error message mismatch (`Uncaught exception: err` not found).
- `L14` value mismatch.
- `L16` value mismatch.
- `L24` value mismatch.
- `L31` expected runtime-error text mismatch.
- `L42` expected-value source synthesis failure.
- `L43` value mismatch.
- `L47` value mismatch.
- `L58, L62, L66, L68, L70, L73, L80, L85, L88, L99` value mismatch.
- Total file-local failures when enabled: `19`.

## `language/derror.dark` (currently disabled)

Status: blocked.

Blocking tests/reasons:
- Roundtrip parser fails at `L2` snippet with tuple-destructuring lambda parameter (`fun (a, b) -> "1"`).
- E2E preamble parse error for the suite: `Expected ',' or '}' after record field` (record literal in `ErrorPropagation`).
- Representative failing tests from this file when fully enabled: `L2, L10, L13, L14, L15, L16, L17, L18, L19, L20, L21, L22, L23, L25, L32, L75, L76`.

## `language/elambda.dark` (currently disabled)

Status: blocked.

Blocking failure:
- Enabling this file aborts test execution before per-test reporting with:
- `payloadSize: Record type 'runtime' not found in typeReg`
- Stack root: `passes/2.5_RefCountInsertion.fs` (`payloadSize` / `insertRC*` path).
- No individual failing test line is emitted before crash.

## `stdlib/date.dark` (currently disabled)

Status: blocked.

Blocking tests/reasons:
- Roundtrip parser fails at `L9` preamble snippet (`let p` with `|> Stdlib.Result.map`) with: `Expected '=' after let binding pattern`.
- E2E preamble type error for the suite: `Stdlib.Result.map expects 2 arguments, but got 3 arguments`.
- Representative failing tests from this file when fully enabled: `L9, L188, L189, L190, L191, L192, L193, L194, L197, L199, L201, L202, L204, L205, L207, L208, L210, L212, L222, L223, L224, L225, L226, L233, L234, L235, L236, L237, L244, L245, L246, L247, L248, L249, L250, L251`.

## Additional `--roundtrip-all-dark` blocker

Status: blocked.

Blocking test/reason:
- File: `src/Tests/e2e/upstream/cloud/db.dark`
- Test: `L112` (`NestedRecordFieldAccess`)
- Snippet: `source`
- Failure: `ParseOriginalFailed`
- Parse error: `Unexpected tokens after expression (only function definitions can be followed by more definitions)`
- Source shape uses a parenthesized multiline sequence of expressions where the first statements are bare calls (not `let` bindings):
  - `Stdlib.DB.set ...`
  - `Stdlib.DB.set ...`
  - `let shouldBeJustJoe = ...`
  - `Stdlib.List.length shouldBeJustJoe`
- Current interpreter parser does not support this sequencing form as a standalone expression snippet.

## Next Steps

1. Fix parser compatibility for upstream syntax forms used in blocked files (`fun (a, b) -> ...`, multiline record literals, and the date preamble `Result.map` form).
2. Fix the `RefCountInsertion.payloadSize` crash path (`runtime` record type missing from `typeReg`) to unlock `elambda.dark`.
3. Add interpreter parser support for parenthesized multiline expression sequencing with bare call statements (the `cloud/db.dark` `L112` form), or keep this case excluded from all-upstream syntax roundtrip coverage.
4. Re-run this checklist and only add files to `defaultUpstreamDarkPaths` once `./run-tests` passes with allowlists removed.
