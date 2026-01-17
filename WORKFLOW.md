# Workflow Context

This project does not require a specific task tracking tool. Use the project's issue tracker (or TODO.md) for planning and progress tracking.

> **Context Recovery**: Read WORKFLOW.md after compaction, clear, or new session

# 🚨 SESSION CLOSE PROTOCOL 🚨

**CRITICAL**: Before saying "done" or "complete", you MUST run this checklist:

```
[ ] 1. git status              (check what changed)
[ ] 2. git add <files>         (stage code changes)
[ ] 3. git commit -m "..."     (commit code)
[ ] 4. git status              (verify clean worktree)
```

**NEVER skip this.** Work is not done until the worktree is clean.

## Core Rules

- Track strategic work in the project issue tracker
- Keep tasks small and scoped
- Leave clear commit messages and update docs when needed

## Common Workflows

**Starting work:**

- Review TODO.md and any open issues relevant to your assignment
- Confirm scope and expected outcomes before coding

**Completing work:**

- Ensure tests and quality gates pass
- Update docs or issues if context changed
