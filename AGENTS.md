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

Use this checklist for every change and include a completed copy in the final
report:

```markdown
- [ ] Worktree: Create a dedicated git worktree; do not modify files in the
      primary checkout.
- [ ] Rebase: Rebase the worktree branch on local `main` before modifying files.
- [ ] Tests: Run every relevant test gate and record each exact command and
      result, or state why tests are not relevant.
- [ ] Benchmarks: Run every relevant benchmark gate and confirm that benchmarks
      have not regressed, or state why benchmarks are not relevant.
- [ ] Commit: Commit the intended changes automatically and record the commit
      hash and subject.
- [ ] Integration: After relevant gates pass, integrate the commit into local
      `main` automatically without waiting for explicit permission.
- [ ] Push: Never push.
```

For CLI commands, development setup, architecture, feature work, and complete
verification requirements, use the canonical sources in `docs/index.md`.
