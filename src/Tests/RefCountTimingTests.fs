// RefCountTimingTests.fs - Unit tests for reference count insertion timing.
//
// These checks can't live in E2E tests because the E2E harness only validates
// program output, not compiler pass timing instrumentation.

module RefCountTimingTests

open AST
open ANF
open AST_to_ANF
open Parser
open TypeChecking

type TestResult = Result<unit, string>

let private convertProgramToConversionResult (typedAst: AST.Program) : Result<AST_to_ANF.ConversionResult, string> =
    let moduleRegistry = Stdlib.buildModuleRegistry ()
    let monomorphized = monomorphize typedAst
    let inlined = inlineLambdasInProgram monomorphized
    liftLambdasInProgram Map.empty Map.empty Map.empty Map.empty inlined
    |> Result.bind (fun lifted ->
        splitTopLevels lifted
        |> Result.bind (fun (typeDefs, functions, expr) ->
            let aliasReg = buildAliasRegistry typeDefs
            let resolvedFunctions = resolveAliasesInFunctions aliasReg functions
            let registries = buildRegistries moduleRegistry typeDefs aliasReg resolvedFunctions
            let varGen = VarGen 0
            convertFunctions registries varGen resolvedFunctions
            |> Result.bind (fun (anfFuncs, varGen1) ->
                convertExprToAnf registries varGen1 expr
                |> Result.map (fun (anfExpr, _) ->
                    {
                        Program = Program (anfFuncs, anfExpr)
                        TypeReg = registries.TypeReg
                        VariantLookup = registries.VariantLookup
                        FuncReg = registries.FuncReg
                        FuncParams = registries.FuncParams
                        ModuleRegistry = registries.ModuleRegistry
                    }))))

let testRcTimingCounters () : TestResult =
    let source =
        "def id(t: (Int64, Int64)) : (Int64, Int64) = t\n" +
        "let t = (1, 2) in let u = (3, 4) in id(t)"
    match parseString false source with
    | Error err -> Error $"Parse error: {err}"
    | Ok ast ->
        match checkProgram ast with
        | Error err -> Error $"Type error: {typeErrorToString err}"
        | Ok (_, typedAst) ->
            match convertProgramToConversionResult typedAst with
            | Error err -> Error $"ANF conversion error: {err}"
            | Ok convResult ->
                match RefCountInsertion.insertRCInProgramWithTiming convResult with
                | Error err -> Error $"RC insertion error: {err}"
                | Ok (_, _, timing) ->
                    let required =
                        [
                            ("AnalyzeReturns", timing.AnalyzeReturns.Calls)
                            ("CollectAliasChain", timing.CollectAliasChain.Calls)
                            ("InsertRC", timing.InsertRC.Calls)
                            ("InferCExprType", timing.InferCExprType.Calls)
                            ("PayloadSize", timing.PayloadSize.Calls)
                            ("InsertParamIncs", timing.InsertParamIncs.Calls)
                            ("InsertReturnDecs", timing.InsertReturnDecs.Calls)
                        ]
                    let missing =
                        required
                        |> List.choose (fun (name, calls) ->
                            if calls > 0 then None else Some name)
                    if not (List.isEmpty missing) then
                        let missingText = String.concat ", " missing
                        Error $"Expected timing counters to be recorded: {missingText}"
                    elif timing.MergeTypes.Calls <> 0 then
                        Error $"Expected MergeTypes to be elided, got {timing.MergeTypes.Calls} calls"
                    else
                        Ok ()

let tests = [
    ("RC timing counters", testRcTimingCounters)
]
