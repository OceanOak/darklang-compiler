# Dark Compiler - AI Agent Guidelines

Read [`docs/index.md`](docs/index.md) first. It owns navigation; this file
contains only rules specific to agents changing this repository.

## F# conventions

- Use functional constructs: no mutation, exceptions, `exit`, or throwing
  lookup helpers.
- Use `Option` only for semantic absence and `Result` for recoverable failure.
- Model invalid states out of existence; complete migrations and remove
  superseded representations rather than adding defaults or shims.
- Use `Crash.crash` for an impossible, undocumented state. Do not guess a
  default.

## Change rules

- Create a failing, focused E2E test before fixing a compiler behavior.
- Keep comments useful to a senior compiler engineer, including the required
  file-purpose comment.
- Use command-line flags rather than environment variables; use `python3` for
  scripts.
- Fix compiler warnings and errors before committing.

## Git workflow

- Perform all work in a dedicated git worktree, never in the primary checkout,
  and rebase the worktree branch on local `main` before starting. Never push.
- When work is complete, commit the intended changes automatically.
- Integrate the commit into local `main` automatically only when it is ready:
  the requested scope is complete, the final diff has been substantively
  reviewed, all relevant tests pass, relevant benchmarks show no regression,
  and no known issue or unresolved uncertainty remains.
- If readiness cannot be established, leave the commit on its worktree branch
  and report `Merged into main: ❌ not ready — <reason>`. Do not use a
  low-value mechanical check as a substitute for relevant validation.

## Completion report

Use this standard format when reporting completed work. Keep the summary brief,
include exact commands, and omit optional lines that add no useful information.
Use `✅` for success, `⏭️` for a skipped or irrelevant gate, and `❌` for a
failure or incomplete step, followed by the reason.

```markdown
Work complete: <brief description of the outcome and important details>

Committed: `<short hash>` — <commit subject>
Merged into main: ✅ `<main HEAD>`
Tests: ✅ <passed>/<total> passed — `<exact command>`
Benchmarks: ✅ no regression, ratio <ratio> — `<exact command>`
Other validation: ✅ <result> — `<exact command>`
Working tree: ✅ clean
Notes: <residual risk, preserved pre-existing changes, or other useful context>
```

For multiple commits or verification commands, put bullet points beneath the
corresponding label. A skipped gate must say why, for example:
`Tests: ⏭️ skipped — documentation-only change`.

For CLI commands, development setup, architecture, feature work, and complete
verification requirements, use the canonical sources in `docs/index.md`.
