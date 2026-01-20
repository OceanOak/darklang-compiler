// StdlibTestHarnessTests.fs - Unit tests for stdlib test harness plumbing
//
// Exercises shared stdlib compilation helpers.

module StdlibTestHarnessTests

open CompilerLibrary
open TypeChecking
open System.Reflection

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
        StdlibANFFunctions = Map.empty
        StdlibANFCallGraph = Map.empty
        StdlibTypeMap = Map.empty
    }

let testResetCachesIsNoOp () : TestResult =
    let stdlib = emptyStdlibResult ()
    let resetStdlib = StdlibTestHarness.resetCaches stdlib
    // After caching removal, resetCaches just returns the same stdlib
    if not (obj.ReferenceEquals(stdlib, resetStdlib)) then
        Error "Expected resetCaches to return the same stdlib instance"
    else
        Ok ()

let testNoSharedStdlibGetter () : TestResult =
    let asm = Assembly.GetExecutingAssembly()
    match asm.GetType("StdlibTestHarness") with
    | null -> Error "Could not locate StdlibTestHarness module type."
    | moduleType ->
        let methods =
            moduleType.GetMethods(
                BindingFlags.Static
                ||| BindingFlags.NonPublic
                ||| BindingFlags.Public
                ||| BindingFlags.DeclaredOnly
            )
        let hasMethod =
            methods |> Array.exists (fun methodInfo -> methodInfo.Name = "getSharedStdlibResult")
        if hasMethod then
            Error "StdlibTestHarness still exposes getSharedStdlibResult; remove shared stdlib accessor."
        else
            Ok ()

let tests : (string * (unit -> TestResult)) list = [
    ("reset caches is no-op", testResetCachesIsNoOp)
    ("no shared stdlib getter", testNoSharedStdlibGetter)
]
