# Do one task

Use Beads (bd) for planning and progress tracking. See WORKFLOW.md for how it works.

ONLY WORK ON A SINGLE ISSUE.

1. Decide which issue to work on next using `bd ready` / `bd list` / `bd show`. This should be the one YOU decide has the highest priority - not necessarily the first in the list.

2. Update the issue to in_progress before starting work.

3. Check any feedback loops. Verify code compiles, tests pass, and benchmarks are not slower. NEVER commit anything not hitting these quality thresholds. Do not consider an issue closed if it does not meet pass these.

4. Be eager about adding subi-issues via bd. Some issues will involve many sub-issues - add these using `bd`.

5. Record progress with `bd comments add <issue-id> "..."`.

6. If the issue is finished use `bd close` to complete it. If the issue is not complete, it is ok to finish this workflow even if the issue is incomplete if the next step is to take another issue to make progress.

7. Make a git commit of this one feature and the .beads directory. Include in the commit message a large discussion of the task and choices and assumptions made.
