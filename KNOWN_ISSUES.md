# Known Issues

1) Block non-stdlib access to internal identifiers.
   - Enforce parser/typechecker rules to reject `__*` and `Stdlib.__HAMT`/`Stdlib.__FingerTree` in user code.
   - Add e2e tests that assert these restrictions.
   - Owner: compiler frontend.

## Recently Fixed

### Stdlib Split + Internal Modules (Fixed)

**Change**: stdlib is now split into per-module files and loaded by the compiler; internal HAMT/FingerTree implementation is isolated under `Stdlib.__HAMT` and `Stdlib.__FingerTree`.

### Register Spilling Segfault (Fixed)

**Root Cause**: Stack frame layout collision between spill slots and callee-saved registers.

The prologue was allocating callee-saved space before spill space, causing spill slots at FP-8, FP-16, etc. to collide with the callee-saved register save area.

**Fix**: Reordered prologue/epilogue in `src/DarkCompiler/passes/6_CodeGen.fs` to allocate spill space before callee-saved space, ensuring spill slots are immediately below FP.
