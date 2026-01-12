# Do one task

Use Beads (bd) for planning and progress tracking. See WORKFLOW.md for how it works.

ONLY WORK ON A SINGLE ISSUE.

1. Decide which issue to work on next. If you've been told what to do, add it to `bd` and then do it. Otherwise select using `bd ready` / `bd list` / `bd show`, you it should be the one YOU decide has the highest priority - not necessarily the first in the list.

2. Start a new git worktree, in which to do work. (put the tree as sibling of compiler-for-dark dir)

3. Update the issue to in_progress before starting work.

4. Check any feedback loops. Verify code compiles, tests pass, and benchmarks are not slower. NEVER commit anything not hitting these quality thresholds. Do not consider an issue closed if it does not meet pass these.

5. Be eager about adding sub-issues via bd. Some issues will involve many sub-issues - add these using `bd`.

6. Record progress with `bd comments add <issue-id> "..."`.

7. If the issue is finished use `bd close` to complete it. If the issue is not complete, it is ok to finish this workflow even if the issue is incomplete if the next step is to take another issue to make progress.

8. Provide a summary of what happened.

9. Wait for the developer to say "APPROVED". Do nothing if he does not say this. Once it is approved, make a git commit of this one task and the .beads directory. Include in the commit message a large discussion of the task and choices and assumptions made. Rebase off the main branch (NOT origin/main). Do a fast-forward merge onto the main branch (NOT origin/main) after running tests again using `./run-tests`.
