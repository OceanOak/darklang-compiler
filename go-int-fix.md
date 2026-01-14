# Fix one type checking issue

The compiler is filled with places where a default TInt64 type is used. This leads to incorrect code, a failure to warn about real type errors, and hidden bugs.

You are going to resolve EXACTLY ONE type checking issue.

Use this process. (Ignore WORKFLOW.md)

1. Start a new git worktree, in which to do work. (put the tree as sibling of compiler-for-dark dir). DO NOT USE MAIN FOR YOUR WORK. DO NOT WRITE ANYTHING TO MAIN UNTIL THE END, when you merge.

2. Find a place in the compiler where the compiler has chosen TInt64 as a default type, even though it is incorrect. Choose at random, do not pick deterministically, including by difficult.

3. Remove the default type assumption, using Result types if possible for error handling, and the `crash` function if that isn't possible. Add a test which highlights the problem.

4. In some cases, the removed assumption will now produce a type error. If so, add a test to check this with a number of types.

5. In some cases, the removed assumption will lead to a problem later in the compiler, where types are not properly propagates, or later passes did not handle this type. Fix these bugs. NEVER CHANGE THE TEST TO ALLOW THE TEST TO PASS (unless the test is actually wrong, which is unlikely). NEVER CHANGE OTHER TESTS.

6. Run the test suite (./run-tests). If tests now start to fail, attempt to fix the compiler to make the tests pass. Repeat until all tests pass.

7. If the failing tests are incorrect and should be failing, continue and tell the developer.

8. After all tests pass, run all Dark benchmarks. Ignore and don't mention the quicksort failure - it's a known issue. If RESULT.md has changed, show the results.

9. After all this, write a short report to the developer about this issue. Include what assumption was removed along with some context, what test was added (show it!). Explain what the test does before and after the change, and how the was fixed by the compiler changes, and what changes had to be made to the compiler to address the issue. If there is any change to the benchmark result in RESULTS.md, SHOW THE CHANGE!

10. DO NOT COMMIT OR MERGE UNTIL I SAY "approved". After that, commit the code, new tests, and new benchmark results, and include in the commit message a large discussion of the issue and the choices and assumptions made. Rebase off the main branch (NOT origin/main). Do a fast-forward merge onto the main branch (NOT origin/main) after running tests again using `./run-tests`. Then clean up the branch and worktree used for this work.

## Policies for handling removed type defaults

- Do not reintroduce a default type for missing type arguments. Preserve unresolved type variables and let later passes surface any problems.
- If a later pass needs a concrete type (for example, monomorphized intrinsics like `__hash<k>`/`__key_eq<k>`), add explicit `*_unknown` intrinsics that crash at runtime rather than picking an arbitrary type.
- Avoid semantic changes as a workaround. Optimizations like rewriting `Dict.fromList([])` to `Dict.empty` are allowed only when type arguments are concrete.
