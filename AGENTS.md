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
- When work is complete, commit the intended changes automatically. Run the
  relevant test gates and verify that relevant benchmarks have not regressed,
  then integrate the commit into local `main` automatically without waiting for
  explicit permission.
- Report the commit and integration result, the exact verification commands run,
  and any test or benchmark gate that was not relevant or could not be run.

For CLI commands, development setup, architecture, feature work, and complete
verification requirements, use the canonical sources in `docs/index.md`.
