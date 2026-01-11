# Ralph Loop Prompt

Use this with: `/ralph-loop:ralph-loop "$(cat RALPH-PROMPT.md)" --completion-promise "COMPLETE"`

---

Use Beads for planning and progress tracking.

1. Decide which issue to work on next using `bd ready` / `bd list` / `bd show`. This should be the one YOU decide has the highest priority - not necessarily the first in the list.

2. Update the issue to in_progress before starting work.

3. Check any feedback loops, such as types and tests. Verify code compiles, tests pass, and benchmarks are not slower. NEVER commit anything not hitting these quality thresholds.

4. Record progress with `bd comments add <issue-id> "..."` and close the issue with `bd close` when complete.

5. Some tasks will involve many thousands of small-ish fixes, such as improving performance or test coverage. In the main task, only add the issues - they can be solved later. EXCEPTION: truly tiny fixes.

6. Make a git commit of that feature. Include in the commit message a large discussion of the feature and choices and assumptions made

ONLY WORK ON A SINGLE ISSUE.

If, while implementing the issue, you notice that all work is complete, output `<promise>COMPLETE</promise>`.
