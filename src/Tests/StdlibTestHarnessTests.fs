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
    let emptyRegistries : AST_to_ANF.Registries = {
        TypeReg = Map.empty
        VariantLookup = Map.empty
        FuncReg = Map.empty
        FuncParams = Map.empty
        ModuleRegistry = Map.empty
    }
    let emptyContext : PipelineContext = {
        TypeCheckEnv = emptyTypeCheckEnv
        GenericFuncDefs = Map.empty
        Registries = emptyRegistries
    }
    let emptyLirProgram = LIR.Program ([], MIR.emptyStringPool, MIR.emptyFloatPool)

    {
        AST = emptyAst
        TypedAST = emptyAst
        Context = emptyContext
        LIRProgram = emptyLirProgram
        AllocatedFunctions = []
        StdlibCallGraph = Map.empty
        StdlibANFFunctions = Map.empty
        StdlibANFCallGraph = Map.empty
        StdlibTypeMap = Map.empty
    }

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
    ("no shared stdlib getter", testNoSharedStdlibGetter)
]
