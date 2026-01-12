// StdlibTestHarnessTests.fs - Unit tests for stdlib test harness plumbing
//
// Exercises shared stdlib compilation and cache reset helpers.

module StdlibTestHarnessTests

open CompilerLibrary
open TypeChecking

/// Test result type
type TestResult = Result<unit, string>

let private emptyStdlibResult () : StdlibResult =
    let emptyAst = AST.Program []
    let emptyTypeCheckEnv : TypeCheckEnv = {
        TypeReg = Map.empty
        VariantLookup = Map.empty
        FuncEnv = Map.empty
        GenericFuncReg = Map.empty
        ModuleRegistry = Map.empty
        AliasReg = Map.empty
    }
    let emptyAnfProgram = ANF.Program ([], ANF.Return ANF.UnitLiteral)
    let emptyAnfResult : AST_to_ANF.ConversionResult = {
        Program = emptyAnfProgram
        TypeReg = Map.empty
        VariantLookup = Map.empty
        FuncReg = Map.empty
        FuncParams = Map.empty
        ModuleRegistry = Map.empty
    }
    let emptyMirProgram = MIR.Program ([], Map.empty, Map.empty)
    let emptyLirProgram = LIR.Program ([], MIR.emptyStringPool, MIR.emptyFloatPool)

    {
        AST = emptyAst
        TypedAST = emptyAst
        TypeCheckEnv = emptyTypeCheckEnv
        ANFResult = emptyAnfResult
        GenericFuncDefs = Map.empty
        ModuleRegistry = Map.empty
        MIRProgram = emptyMirProgram
        LIRProgram = emptyLirProgram
        AllocatedFunctions = []
        CompileMode = StdlibCompileMode.Sequential
        StdlibCallGraph = Map.empty
        SpecCache = SpecializationCache()
        StdlibANFFunctions = Map.empty
        StdlibANFCallGraph = Map.empty
        StdlibTypeMap = Map.empty
        CompiledFuncCache = createCompiledFunctionCache ()
        ANFFuncCache = ANFFunctionCache()
        PreambleCache = PreambleCache()
        CodegenCache = CodegenCache()
    }

let testResetCachesCreatesFreshInstances () : TestResult =
    let stdlib = emptyStdlibResult ()
    let resetStdlib = StdlibTestHarness.resetCaches stdlib
    if obj.ReferenceEquals(stdlib.SpecCache, resetStdlib.SpecCache) then
        Error "Expected SpecCache to be reset"
    elif obj.ReferenceEquals(stdlib.CompiledFuncCache, resetStdlib.CompiledFuncCache) then
        Error "Expected CompiledFuncCache to be reset"
    elif obj.ReferenceEquals(stdlib.ANFFuncCache, resetStdlib.ANFFuncCache) then
        Error "Expected ANFFuncCache to be reset"
    elif obj.ReferenceEquals(stdlib.PreambleCache, resetStdlib.PreambleCache) then
        Error "Expected PreambleCache to be reset"
    elif obj.ReferenceEquals(stdlib.CodegenCache, resetStdlib.CodegenCache) then
        Error "Expected CodegenCache to be reset"
    else
        Ok ()

let tests : (string * (unit -> TestResult)) list = [
    ("reset caches", testResetCachesCreatesFreshInstances)
]
