// E2ETestRunner.fs - End-to-end test runner
//
// Compiles source code, executes it, and validates output/exit code.

module TestDSL.E2ETestRunner

open System
open AST
open AST_to_ANF
open TypeChecking
open TestDSL.E2EFormat

// Internal identifiers are only allowed for stdlib-internal tests.
let private isInternalTestFile (sourceFile: string) : bool =
    let normalized = sourceFile.Replace('\\', '/')
    normalized.Contains("/stdlib-internal/")

// Choose parser syntax from the test suite directory.
let private sourceSyntaxForTestFile (sourceFile: string) : CompilerLibrary.SourceSyntax =
    let normalized = sourceFile.Replace('\\', '/')
    if normalized.Contains("/interpreter/")
       || normalized.Contains("/e2e/upstream/language/") then
        CompilerLibrary.InterpreterSyntax
    else
        CompilerLibrary.CompilerSyntax

// Build the source expression to execute for a test.
// For `lhs = rhs` value tests, run a synthesized equality assertion.

let private prettySyntaxForSourceSyntax (sourceSyntax: CompilerLibrary.SourceSyntax) : ASTPrettyPrinter.Syntax =
    match sourceSyntax with
    | CompilerLibrary.CompilerSyntax -> ASTPrettyPrinter.CompilerSyntax
    | CompilerLibrary.InterpreterSyntax -> ASTPrettyPrinter.InterpreterSyntax

let private asSingleExpression (program: Program) : Expr option =
    let (Program topLevels) = program
    match topLevels with
    | [Expression expr] -> Some expr
    | _ -> None

let private tryFormatProgramIfStable
    (sourceSyntax: CompilerLibrary.SourceSyntax)
    (allowInternal: bool)
    (program: Program)
    : string option =
    let prettySyntax = prettySyntaxForSourceSyntax sourceSyntax
    let formatted = ASTPrettyPrinter.formatProgram prettySyntax program
    match CompilerLibrary.parseProgram sourceSyntax allowInternal formatted with
    | Ok reparsed ->
        match sourceSyntax with
        | CompilerLibrary.CompilerSyntax ->
            if reparsed = program then Some formatted else None
        | CompilerLibrary.InterpreterSyntax ->
            // Interpreter syntax intentionally normalizes some AST details
            // (for example around lambda forms), so parse-success is enough.
            Some formatted
    | Error _ -> None

let private trySynthesizeValueEqualitySource
    (sourceSyntax: CompilerLibrary.SourceSyntax)
    (allowInternal: bool)
    (source: string)
    (rhsExpr: string)
    : string option =
    let sourceProgramResult = CompilerLibrary.parseProgram sourceSyntax allowInternal source
    let rhsProgramResult = CompilerLibrary.parseProgram sourceSyntax allowInternal rhsExpr

    match sourceProgramResult, rhsProgramResult with
    | Ok (Program sourceTopLevels), Ok rhsProgram ->
        match List.rev sourceTopLevels, asSingleExpression rhsProgram with
        | Expression lhsExpr :: sourceRestRev, Some rhsAst ->
            let directEqProgram =
                Program (List.rev (Expression (BinOp (Eq, lhsExpr, rhsAst)) :: sourceRestRev))

            let actualBindingName = "e2eValueActual"
            let letEqExpr =
                Let (actualBindingName, lhsExpr, BinOp (Eq, Var actualBindingName, rhsAst))
            let letEqProgram =
                Program (List.rev (Expression letEqExpr :: sourceRestRev))

            match tryFormatProgramIfStable sourceSyntax allowInternal directEqProgram with
            | Some rewritten -> Some rewritten
            | None -> tryFormatProgramIfStable sourceSyntax allowInternal letEqProgram
        | _ ->
            None
    | _ ->
        None

let private sourceToExecute
    (sourceSyntax: CompilerLibrary.SourceSyntax)
    (allowInternal: bool)
    (test: E2ETest)
    : Result<string, string> =
    match test.ExpectedValueExpr with
    | Some rhsExpr ->
        match trySynthesizeValueEqualitySource sourceSyntax allowInternal test.Source rhsExpr with
        | Some rewritten -> Ok rewritten
        | None ->
            // Some interpreter-specific forms (for example operator sections) can fail
            // AST pretty-print roundtrips even though the direct source is valid.
            // Fall back to textual wrapping and parse-validate before execution.
            let fallbackSource = $"({test.Source}) == ({rhsExpr})"
            match CompilerLibrary.parseProgram sourceSyntax allowInternal fallbackSource with
            | Ok _ -> Ok fallbackSource
            | Error _ ->
                Error (
                    $"Failed to synthesize expected-value source for test '{test.Name}' in {test.SourceFile}.\n"
                    + "Expected-value tests must parse as a program whose last top-level is an expression,\n"
                    + "and RHS must parse as a single expression."
                )
    | None -> Ok test.Source

/// Result of running an E2E test
type E2ERun =
    | CompileFailed of exitCode:int * error:string * compileTime:TimeSpan
    | Ran of exitCode:int * stdout:string * stderr:string * compileTime:TimeSpan * runtimeTime:TimeSpan

type E2EFailure = {
    Run: E2ERun
    Message: string
}

type E2ETestResult = Result<E2ERun, E2EFailure>

type private PreambleBuildSpec = {
    SourceFile: string
    Preamble: string
    FunctionLineMap: Map<string, int>
    AllowInternal: bool
    SourceSyntax: CompilerLibrary.SourceSyntax
}

/// Map of built preamble contexts keyed by source file
type PreambleContextMap = Map<string, CompilerLibrary.PreambleContext>

type SuiteContext = {
    Stdlib: CompilerLibrary.StdlibResult
    PreambleContexts: PreambleContextMap
}

type private PreamblePlan = {
    Spec: PreambleBuildSpec
    Analysis: CompilerLibrary.PreambleAnalysis option
    Specialization: SpecializationResult
}

let private buildPreambleBuildSpec (sourceFile: string) (tests: E2ETest list) : Result<PreambleBuildSpec, string> =
    let preambles =
        tests
        |> List.map (fun test -> test.Preamble)
        |> List.distinct
    match preambles with
    | [preamble] ->
        let funcLineMaps =
            tests
            |> List.map (fun test -> test.FunctionLineMap)
            |> List.distinct
        match funcLineMaps with
        | [funcLineMap] ->
            let sourceSyntax = sourceSyntaxForTestFile sourceFile
            Ok {
                SourceFile = sourceFile
                Preamble = preamble
                FunctionLineMap = funcLineMap
                AllowInternal = isInternalTestFile sourceFile
                SourceSyntax = sourceSyntax
            }
        | _ ->
            Error $"Multiple function line maps found for {sourceFile}"
    | _ ->
        Error $"Multiple preambles found for {sourceFile}"

let private collectTypeAppsFromProgram (program: Program) : Set<SpecKey> =
    let (Program topLevels) = program
    topLevels
    |> List.map (function
        | FunctionDef f when List.isEmpty f.TypeParams -> collectTypeAppsFromFunc f
        | Expression e -> collectTypeApps e
        | _ -> Set.empty)
    |> List.fold Set.union Set.empty

let private filterSpecsByDefs (genericDefs: GenericFuncDefs) (specs: Set<SpecKey>) : Set<SpecKey> =
    specs |> Set.filter (fun (funcName, _) -> Map.containsKey funcName genericDefs)

let private collectSpecsFromTests
    (allowInternal: bool)
    (sourceSyntax: CompilerLibrary.SourceSyntax)
    (typeCheckEnv: TypeCheckEnv)
    (tests: E2ETest list)
    : Result<Set<SpecKey>, string> =
    let testsToAnalyze =
        tests
        |> List.filter (fun test -> not test.ExpectCompileError && Option.isNone test.SkipReason)
    let rec loop remaining acc =
        match remaining with
        | [] -> Ok acc
        | test :: rest ->
            match sourceToExecute sourceSyntax allowInternal test with
            | Error _ ->
                // Best effort: source synthesis may fail for malformed tests and
                // should not block suite-level specialization discovery.
                loop rest acc
            | Ok source ->
                let specsResult =
                    CompilerLibrary.parseProgram sourceSyntax allowInternal source
                    |> Result.bind (fun testAst ->
                        checkProgramWithBaseEnv typeCheckEnv testAst
                        |> Result.mapError typeErrorToString
                        |> Result.map (fun (_programType, typedAst, _) -> collectTypeAppsFromProgram typedAst))

                match specsResult with
                | Ok specs ->
                    loop rest (Set.union acc specs)
                | Error _ ->
                    // Best effort: a test may intentionally fail to parse/typecheck,
                    // and that should not prevent building suite-level specializations.
                    loop rest acc
    loop testsToAnalyze Set.empty

let private buildPreamblePlan
    (stdlib: CompilerLibrary.StdlibResult)
    (spec: PreambleBuildSpec)
    (tests: E2ETest list)
    : Result<PreamblePlan * Set<SpecKey>, string> =
    let preambleIsEmpty = String.IsNullOrWhiteSpace spec.Preamble
    let analysisResult =
        if preambleIsEmpty then
            Ok None
        else
            CompilerLibrary.analyzePreamble spec.SourceSyntax spec.AllowInternal stdlib spec.Preamble
            |> Result.mapError (fun err -> $"Preamble parse error in {spec.SourceFile}: {err}")
            |> Result.map Some

    analysisResult
    |> Result.bind (fun analysisOpt ->
        let typeCheckEnv =
            match analysisOpt with
            | Some analysis -> analysis.TypeCheckEnv
            | None -> stdlib.Context.TypeCheckEnv
        collectSpecsFromTests spec.AllowInternal spec.SourceSyntax typeCheckEnv tests
        |> Result.bind (fun testSpecs ->
            let preambleSpecs =
                match analysisOpt with
                | None -> Set.empty
                | Some analysis -> collectTypeAppsFromProgram analysis.TypedAST
            let combinedSpecs = Set.union testSpecs preambleSpecs
            let preambleGenericDefs =
                match analysisOpt with
                | None -> Map.empty
                | Some analysis -> analysis.GenericFuncDefs
            let preambleSpecsForDefs = filterSpecsByDefs preambleGenericDefs combinedSpecs
            let stdlibSpecsFromCombined = filterSpecsByDefs stdlib.Context.GenericFuncDefs combinedSpecs
            let specialization =
                if Map.isEmpty preambleGenericDefs then
                    {
                        SpecializedFuncs = []
                        SpecRegistry = Map.empty
                        ExternalSpecs = Set.empty
                    }
                else
                    specializeFromSpecs preambleGenericDefs preambleSpecsForDefs
            let stdlibSpecsFromSpecialization =
                filterSpecsByDefs stdlib.Context.GenericFuncDefs specialization.ExternalSpecs
            let stdlibSpecs = Set.union stdlibSpecsFromCombined stdlibSpecsFromSpecialization
            let plan = {
                Spec = spec
                Analysis = analysisOpt
                Specialization = specialization
            }
            Ok (plan, stdlibSpecs)))

/// Build suite stdlib specializations and per-file preamble contexts
let buildSuiteContexts
    (stdlib: CompilerLibrary.StdlibResult)
    (tests: E2ETest array)
    (passTimingRecorder: CompilerLibrary.PassTimingRecorder option)
    : Result<SuiteContext, string> =
    let groupedTests =
        tests
        |> Array.toList
        |> List.groupBy (fun test -> test.SourceFile)

    let plansResult =
        groupedTests
        |> List.fold
            (fun acc (sourceFile, group) ->
                acc
                |> Result.bind (fun (plans, stdlibSpecs) ->
                    buildPreambleBuildSpec sourceFile group
                    |> Result.bind (fun spec ->
                        buildPreamblePlan stdlib spec group
                        |> Result.map (fun (plan, specs) ->
                            (plan :: plans, Set.union stdlibSpecs specs)))))
            (Ok ([], Set.empty))

    plansResult
    |> Result.bind (fun (plans, stdlibSpecs) ->
        CompilerLibrary.buildStdlibSpecializations stdlib stdlibSpecs passTimingRecorder
        |> Result.bind (fun stdlibWithSpecs ->
            let buildEmptyContext () : CompilerLibrary.PreambleContext =
                {
                    Context = stdlibWithSpecs.Context
                    ANFFunctions = []
                    TypeMap = stdlibWithSpecs.StdlibTypeMap
                    SymbolicFunctions = []
                }
            let contextsResult =
                plans
                |> List.fold
                    (fun acc plan ->
                        acc
                        |> Result.bind (fun (currentStdlib, contexts) ->
                            let ctxResult =
                                match plan.Analysis with
                                | None -> Ok (currentStdlib, buildEmptyContext ())
                                | Some analysis ->
                                    CompilerLibrary.buildPreambleContextFromAnalysis
                                        currentStdlib
                                        analysis
                                        plan.Specialization
                                        plan.Spec.SourceFile
                                        plan.Spec.FunctionLineMap
                                        passTimingRecorder
                                    |> Result.mapError (fun err ->
                                        $"Preamble build error ({plan.Spec.SourceFile}): {err}")
                            ctxResult
                            |> Result.map (fun (updatedStdlib, ctx) ->
                                (updatedStdlib, Map.add plan.Spec.SourceFile ctx contexts))))
                    (Ok (stdlibWithSpecs, Map.empty))
            contextsResult
            |> Result.map (fun (updatedStdlib, contexts) ->
                {
                    Stdlib = updatedStdlib
                    PreambleContexts = contexts
                })))

let private exitCodeFromRun (run: E2ERun) : int =
    match run with
    | CompileFailed (exitCode, _, _) -> exitCode
    | Ran (exitCode, _, _, _, _) -> exitCode

let private stdoutFromRun (run: E2ERun) : string =
    match run with
    | CompileFailed _ -> ""
    | Ran (_, stdout, _, _, _) -> stdout

let private stderrFromRun (run: E2ERun) : string =
    match run with
    | CompileFailed (_, error, _) -> error
    | Ran (_, _, stderr, _, _) -> stderr

let private failRun (run: E2ERun) (message: string) : E2ETestResult =
    Error { Run = run; Message = message }

let private didValueEqualityPass (run: E2ERun) : bool =
    if exitCodeFromRun run <> 0 then
        false
    else
        let lastStdoutLine =
            (stdoutFromRun run).Split([| '\n' |], StringSplitOptions.RemoveEmptyEntries)
            |> Array.tryLast
        lastStdoutLine = Some "true"

let private evaluateExpectations (test: E2ETest) (run: E2ERun) : E2ETestResult =
    if test.ExpectCompileError then
        let gotError = exitCodeFromRun run <> 0
        if not gotError then
            failRun run "Expected compilation error but compilation succeeded"
        else
            match test.ExpectedErrorMessage with
            | Some expectedMsg ->
                let output = stderrFromRun run
                if output.Contains(expectedMsg) then
                    Ok run
                else
                    failRun run $"Expected error message '{expectedMsg}' not found in stderr"
            | None ->
                Ok run
    elif Option.isSome test.ExpectedValueExpr then
        if didValueEqualityPass run then
            Ok run
        else
            failRun run "Value mismatch"
    else
        let stdoutMatches =
            match test.ExpectedStdout with
            | None -> true
            | Some expected ->
                let actual = stdoutFromRun run
                actual.Trim() = expected.Trim()

        let stderrMatches =
            match test.ExpectedStderr with
            | None -> true
            | Some expected ->
                let actual = stderrFromRun run
                actual.Trim() = expected.Trim()

        let exitCodeMatches = exitCodeFromRun run = test.ExpectedExitCode

        if stdoutMatches && stderrMatches && exitCodeMatches then
            Ok run
        else
            failRun run "Output mismatch"

let private buildCompilerOptions (test: E2ETest) : CompilerLibrary.CompilerOptions =
    { CompilerLibrary.defaultOptions with
        DisableFreeList = test.DisableFreeList
        DisableANFOpt = test.DisableANFOpt
        DisableANFConstFolding = test.DisableANFConstFolding
        DisableANFConstProp = test.DisableANFConstProp
        DisableANFCopyProp = test.DisableANFCopyProp
        DisableANFDCE = test.DisableANFDCE
        DisableANFStrengthReduction = test.DisableANFStrengthReduction
        DisableInlining = test.DisableInlining
        DisableTCO = test.DisableTCO
        DisableMIROpt = test.DisableMIROpt
        DisableMIRConstFolding = test.DisableMIRConstFolding
        DisableMIRCSE = test.DisableMIRCSE
        DisableMIRCopyProp = test.DisableMIRCopyProp
        DisableMIRDCE = test.DisableMIRDCE
        DisableMIRCFGSimplify = test.DisableMIRCFGSimplify
        DisableMIRLICM = test.DisableMIRLICM
        DisableLIROpt = test.DisableLIROpt
        DisableLIRPeephole = test.DisableLIRPeephole
        DisableFunctionTreeShaking = test.DisableFunctionTreeShaking
        EnableCoverage = false
        EnableLeakCheck = not test.DisableLeakCheck
        DumpANF = false
        DumpMIR = false
        DumpLIR = false
    }

let private tryExecuteBinary (binary: byte array) : Result<CompilerLibrary.ExecutionOutput, string> =
    try Ok (CompilerLibrary.execute 0 binary)
    with ex -> Error ex.Message

let private compileAndRun (request: CompilerLibrary.CompileRequest) : E2ERun =
    let compileReport = CompilerLibrary.compile request
    match compileReport.Result with
    | Error err ->
        CompileFailed (1, err, compileReport.CompileTime)
    | Ok binary ->
        match tryExecuteBinary binary with
        | Ok execResult ->
            Ran (execResult.ExitCode, execResult.Stdout, execResult.Stderr, compileReport.CompileTime, execResult.RuntimeTime)
        | Error err ->
            Ran (-1, "", $"Execution failed: {err}", compileReport.CompileTime, TimeSpan.Zero)

/// Run E2E test using a prebuilt preamble context
let runE2ETestWithPreambleContext
    (stdlib: CompilerLibrary.StdlibResult)
    (preambleCtx: CompilerLibrary.PreambleContext)
    (test: E2ETest)
    (passTimingRecorder: CompilerLibrary.PassTimingRecorder option)
    : E2ETestResult =
    let options = buildCompilerOptions test
    let allowInternal = isInternalTestFile test.SourceFile
    let sourceSyntax = sourceSyntaxForTestFile test.SourceFile
    match sourceToExecute sourceSyntax allowInternal test with
    | Error msg ->
        let run = CompileFailed (1, msg, TimeSpan.Zero)
        failRun run msg
    | Ok source ->
        let request : CompilerLibrary.CompileRequest = {
            Context = CompilerLibrary.StdlibWithPreamble (stdlib, preambleCtx)
            Mode = CompilerLibrary.CompileMode.TestExpression
            SourceSyntax = sourceSyntax
            Source = source
            SourceFile = test.SourceFile
            AllowInternal = allowInternal
            Verbosity = 0
            Options = options
            PassTimingRecorder = passTimingRecorder
        }
        let run = compileAndRun request
        evaluateExpectations test run
