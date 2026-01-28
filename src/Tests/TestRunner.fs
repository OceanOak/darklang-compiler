// TestRunner.fs - Test runner entrypoint and suite orchestration.
//
// Defines Dark compiler test suites and their execution order.

module TestRunner.Main

open System
open System.IO
open System.Diagnostics
open Output
open TestDSL.PassTestRunner
open TestDSL.E2EFormat
open TestDSL.E2ETestRunner
open TestDSL.OptimizationFormat
open TestDSL.OptimizationTestRunner
open TestRunnerArgs
open TestFramework

// Print help message
let printHelp () =
    println "Usage: Tests [OPTIONS]"
    println ""
    println "Options:"
    println "  --filter=PATTERN   Run only tests matching PATTERN (case-insensitive substring)"
    println "  --coverage         Show stdlib coverage percentage after running tests"
    println "  --verification     Enable verification/stress tests"
    println "  --verbose, -v      Print failing tests as soon as they occur"
    println "  --help, -h         Show this help message"
    println ""
    println "Examples:"
    println "  Tests                      Run all tests"
    println "  Tests --filter=tuple       Run tests with 'tuple' in the name"
    println "  Tests --filter=string      Run tests with 'string' in the name"
    println "  Tests --coverage           Run tests and show coverage percentage"
    println "  Tests --verification       Run verification/stress tests"

[<EntryPoint>]
let main args =
    // Check for --help flag
    if hasHelpArg args then
        printHelp ()
        0
    else

    let totalTimer = Stopwatch.StartNew()

    // Check for --filter=PATTERN argument
    let filter = parseFilterArg args

    // Check for --coverage flag (show inline coverage after tests)
    let showCoverage = hasCoverageArg args

    // Check for --verification flag (enable verification/stress tests)
    let verificationEnabled = hasVerificationArg args

    // Check for --verbose flag (print failing tests immediately)
    let verbose = hasVerboseArg args

    println $"{Colors.bold}{Colors.cyan}🧪 Running DSL-based Tests{Colors.reset}"
    match filter with
    | Some pattern -> println $"{Colors.gray}  Filter: {pattern}{Colors.reset}"
    | None -> ()
    println ""

    // Use the source tree for test data to avoid copying files into the build output.
    let testDataRoot = Path.GetFullPath(__SOURCE_DIRECTORY__)
    let getTestFiles (dir : string) (suffix: string) =
        let fulldir = Path.Combine(testDataRoot, dir)
        Directory.GetFiles(fulldir, $"*.{suffix}", SearchOption.AllDirectories)

    let e2eTestFiles = getTestFiles "e2e" "e2e"
    let verificationTestFiles = getTestFiles "verification" "e2e"
    let optTestFiles = getTestFiles "optimization" "opt"
    let typecheckTestFiles = getTestFiles "typecheck" "typecheck"
    let anf2mirTestFiles  = getTestFiles "passes/anf2mir" "anf2mir"
    let mir2lirTestFiles  = getTestFiles "passes/mir2lir" "mir2lir"
    let lir2arm64TestFiles  = getTestFiles "passes/lir2arm64" "lir2arm64"
    let arm64encTestFiles  = getTestFiles "passes/arm64enc" "arm64enc"

    let unitStdlibSuites = [ "Stdlib Compile Tests"; "Preamble Build Tests" ]
    let buildUnitTests (_stdlib: CompilerLibrary.StdlibResult) : UnitTestSuite array = [|
        { Name = "CLI Flags Tests"; Tests = CliFlagTests.tests }
        { Name = "IR Symbol Tests"; Tests = IRSymbolTests.tests }
        { Name = "IR Printer Tests"; Tests = IRPrinterTests.tests }
        { Name = "MIR Optimize Tests"; Tests = MIROptimizeTests.tests }
        { Name = "Script Helper Tests"; Tests = ScriptHelperTests.tests }
        { Name = "Pass Test Runner Tests"; Tests = PassTestRunnerTests.tests }
        { Name = "Progress Bar Tests"; Tests = ProgressBarTests.tests }
        { Name = "Encoding Tests"; Tests = EncodingTests.tests }
        { Name = "Binary Tests"; Tests = BinaryTests.tests }
        { Name = "Type Checking Tests"; Tests = TypeCheckingTests.tests }
        { Name = "Parallel Move Tests"; Tests = ParallelMoveTests.tests }
        { Name = "SSA Liveness Tests"; Tests = SSALivenessTests.tests }
        { Name = "Phi Resolution Tests"; Tests = PhiResolutionTests.tests }
        { Name = "Chordal Graph Tests"; Tests = ChordalGraphTests.tests }
        { Name = "AST to ANF Tests"; Tests = ASTToANFTests.tests }
        { Name = "ANF Inlining Tests"; Tests = ANFInliningTests.tests }
        { Name = "Monomorphization Tests"; Tests = MonomorphizationTests.tests }
        { Name = "Lambda Lifting Tests"; Tests = LambdaLiftingTests.tests }
    |]

    let enableVerification = verificationEnabled

    let symbols : OutputSymbols =
        { Pass = "✓"
          Fail = "✗"
          SectionPrefix = "└─" }

    let runState = TestFramework.createState ()
    let recordTiming = TestFramework.recordTiming runState
    let recordResults = TestFramework.recordResults runState
    let recordPassTiming = TestFramework.recordPassTiming runState
    let passTimingTotal () = TestFramework.calculatePassTimingsTotalForOverhead runState.PassTimings
    let recordNonPassTiming (name: string) (elapsed: TimeSpan) : unit =
        if elapsed > TimeSpan.Zero then
            recordPassTiming { Pass = name; Elapsed = elapsed }
    let recordPhaseOverhead
        (name: string)
        (elapsed: TimeSpan)
        (passTimingStart: TimeSpan)
        (passTimingEnd: TimeSpan)
        : unit =
        let passDelta = passTimingEnd - passTimingStart
        if passDelta < TimeSpan.Zero then
            Crash.crash $"recordPhaseOverhead: pass timing delta ({passDelta}) is negative for {name}"
        let overhead = elapsed - passDelta
        recordNonPassTiming name overhead

    let stdlibPassTimingStart = passTimingTotal ()
    let timer = Stopwatch.StartNew()
    let stdlib =
        match CompilerLibrary.buildStdlibWithTrace (Some recordPassTiming) with
        | Ok stdlib -> stdlib
        | Error err -> failwith $"Stdlib didnt build with error: {err}"
    let elapsed = timer.Elapsed
    let stdlibPassTimingEnd = passTimingTotal ()
    recordPhaseOverhead "Stdlib Build Overhead" elapsed stdlibPassTimingStart stdlibPassTimingEnd
    let allUnitTests = buildUnitTests stdlib

    let loadE2ETests (testFiles: string array) : E2ETest array * (string * string) list =
        let tests = ResizeArray<E2ETest>()
        let mutable parseErrors = []
        for testFile in testFiles do
            match parseE2ETestFile testFile with
            | Ok parsed -> tests.AddRange(parsed)
            | Error msg -> parseErrors <- (testFile, msg) :: parseErrors
        (tests.ToArray(), List.rev parseErrors)

    let reportParseErrors (suiteName: string) (parseErrors: (string * string) list) : unit =
        for (filePath, msg) in parseErrors do
            let fileName = Path.GetFileName filePath
            println $"  {Colors.red}✗ ERROR parsing {fileName}{Colors.reset}"
            println $"    {msg}"
            recordResults 0 1 [{ File = filePath; Name = $"{suiteName}: {fileName}"; Message = msg; Details = [] }]

    let runE2ESuite
        (baseStdlib: CompilerLibrary.StdlibResult)
        (suiteName: string)
        (progressLabel: string)
        (testsArray: E2ETest array)
        : unit =
        let stdlib = baseStdlib
        let numTests = testsArray.Length
        if numTests > 0 then
            let suitePassTimingStart = passTimingTotal ()
            let suiteContextTimer = Stopwatch.StartNew()
            let suiteContextsResult =
                TestDSL.E2ETestRunner.buildSuiteContexts
                    stdlib
                    testsArray
                    (Some recordPassTiming)
            suiteContextTimer.Stop()
            let suitePassTimingEnd = passTimingTotal ()
            recordPhaseOverhead
                $"{suiteName} Suite Context Overhead"
                suiteContextTimer.Elapsed
                suitePassTimingStart
                suitePassTimingEnd
            let mutable suiteContextsOpt =
                match suiteContextsResult with
                | Ok suiteContexts -> Some suiteContexts
                | Error _ -> None

            let results = Array.zeroCreate<option<E2ETest * E2ETestResult>> numTests
            let progress = ProgressBar.create progressLabel numTests
            ProgressBar.update progress

            let unpackRun (run: E2ERun) : int * string * string * TimeSpan * TimeSpan =
                match run with
                | CompileFailed (exitCode, error, compileTime) ->
                    (exitCode, "", error, compileTime, TimeSpan.Zero)
                | Ran (exitCode, stdout, stderr, compileTime, runtimeTime) ->
                    (exitCode, stdout, stderr, compileTime, runtimeTime)

            let runFromTestResult (result: E2ETestResult) : E2ERun =
                match result with
                | Ok run -> run
                | Error failure -> failure.Run

            let preambleFailureResult (message: string) : E2ETestResult =
                Error { Run = CompileFailed (1, message, TimeSpan.Zero); Message = message }

            let collectExitCodeDetails (test: E2ETest) (run: E2ERun) : string list =
                let (exitCode, _, _, _, _) = unpackRun run
                if exitCode <> test.ExpectedExitCode then
                    [ $"Expected exit code: {test.ExpectedExitCode}, Actual: {exitCode}" ]
                else
                    []

            let printE2EFailure (test: E2ETest) (failure: E2EFailure) : string list =
                let run = failure.Run
                let (_, stdout, stderr, compileTime, runtimeTime) = unpackRun run
                let cleanName = test.Name.Replace("Stdlib.", "")
                let displayName = if cleanName.Length > 60 then cleanName.Substring(0, 57) + "..." else cleanName
                println $"  {displayName}... {Colors.red}✗ FAIL{Colors.reset} {Colors.gray}(compile: {formatTime compileTime}, run: {formatTime runtimeTime}){Colors.reset}"
                println $"    {failure.Message}"

                let details = collectExitCodeDetails test run
                for detail in details do
                    println $"    {detail}"

                match test.ExpectedStdout with
                | Some expected when stdout.Trim() <> expected.Trim() ->
                    let expectedDisp = expected.Replace("\n", "\\n")
                    let actualDisp = stdout.Replace("\n", "\\n")
                    println $"    Expected stdout: {expectedDisp}"
                    println $"    Actual stdout: {actualDisp}"
                | _ -> ()
                match test.ExpectedStderr with
                | Some expected when stderr.Trim() <> expected.Trim() ->
                    let expectedDisp = expected.Replace("\n", "\\n")
                    let actualDisp = stderr.Replace("\n", "\\n")
                    println $"    Expected stderr: {expectedDisp}"
                    println $"    Actual stderr: {actualDisp}"
                | _ -> ()
                details

            for i in 0 .. numTests - 1 do
                let test = testsArray.[i]
                let result, passTimingDelta =
                    match suiteContextsResult with
                    | Error err ->
                        preambleFailureResult $"Preamble build failed: {err}", TimeSpan.Zero
                    | Ok _ ->
                        match suiteContextsOpt with
                        | None ->
                            preambleFailureResult "Missing built suite contexts", TimeSpan.Zero
                        | Some currentSuiteContexts ->
                            match Map.tryFind test.SourceFile currentSuiteContexts.PreambleContexts with
                            | None ->
                                preambleFailureResult $"Missing built preamble context for {test.SourceFile}", TimeSpan.Zero
                            | Some ctx ->
                                let passTimingStart = passTimingTotal ()
                                let testResult =
                                    TestDSL.E2ETestRunner.runE2ETestWithPreambleContext
                                        currentSuiteContexts.Stdlib
                                        ctx
                                        test
                                        (Some recordPassTiming)
                                let passTimingEnd = passTimingTotal ()
                                let passTimingDelta = passTimingEnd - passTimingStart
                                testResult, passTimingDelta

                let run = runFromTestResult result
                let (_, _, _, compileTime, runtimeTime) = unpackRun run
                let compileOverhead = compileTime - passTimingDelta
                recordNonPassTiming "Compile Overhead" compileOverhead
                recordNonPassTiming TestFramework.testRuntimeTimingName runtimeTime
                let totalTime = compileTime + runtimeTime
                results.[i] <- Some (test, result)
                recordTiming {
                    Name = $"{suiteName}: {test.Name}"
                    TotalTime = totalTime
                    CompileTime = Some compileTime
                    RuntimeTime = Some runtimeTime
                }
                let success =
                    match result with
                    | Ok _ -> true
                    | Error _ -> false
                ProgressBar.increment progress success
                match result with
                | Error failure when verbose ->
                    ProgressBar.finish progress
                    let _ = printE2EFailure test failure
                    ProgressBar.update progress
                | _ -> ()

            ProgressBar.finish progress

            let mutable sectionPassed = 0
            let mutable sectionFailed = 0
            let failedTestsLocal = ResizeArray<FailedTestInfo>()
            for result in results do
                match result with
                | Some (test, testResult) ->
                    match testResult with
                    | Ok _ ->
                        sectionPassed <- sectionPassed + 1
                    | Error failure ->
                        let details =
                            if verbose then collectExitCodeDetails test failure.Run
                            else printE2EFailure test failure
                        failedTestsLocal.Add({ File = test.SourceFile; Name = $"{suiteName}: {test.Name}"; Message = failure.Message; Details = details })
                        sectionFailed <- sectionFailed + 1
                | None -> ()

            recordResults sectionPassed sectionFailed (failedTestsLocal |> Seq.toList)

            if sectionFailed = 0 then
                println $"  {Colors.green}✓ {sectionPassed} passed{Colors.reset}"
            else
                println $"  {Colors.green}✓ {sectionPassed} passed{Colors.reset}, {Colors.red}✗ {sectionFailed} failed{Colors.reset}"

    let runPassTestFile
        (loadTest: string -> Result<'input, string>)
        (runTest: 'input -> PassTestResult)
        (testPath: string)
        : Result<PassTestResult, string> =
        match loadTest testPath with
        | Ok input -> Ok (runTest input)
        | Error msg -> Error msg

    let handlePassTestSuccess
        (suiteLabel: string)
        (progress: ProgressBar.State)
        (testPath: string)
        (testName: string)
        (elapsed: TimeSpan)
        (result: PassTestResult)
        : FileSuiteSummary =
        if result.Success then
            ProgressBar.increment progress true
            { Passed = 1; Failed = 0; FailedTests = [] }
        else
            ProgressBar.increment progress false
            ProgressBar.finish progress
            println $"  {testName}... {Colors.red}✗ FAIL{Colors.reset} {Colors.gray}({formatTime elapsed}){Colors.reset}"
            println $"    {result.Message}"
            let details = addExpectedActualDetails result.Expected result.Actual
            let failedInfo =
                { File = testPath
                  Name = $"{suiteLabel}: {testName}"
                  Message = result.Message
                  Details = details }
            ProgressBar.update progress
            { Passed = 0; Failed = 1; FailedTests = [ failedInfo ] }

    let handlePassTestError
        (suiteLabel: string)
        (progress: ProgressBar.State)
        (testPath: string)
        (testName: string)
        (elapsed: TimeSpan)
        (msg: string)
        : FileSuiteSummary =
        ProgressBar.increment progress false
        ProgressBar.finish progress
        println $"  {testName}... {Colors.red}✗ ERROR{Colors.reset} {Colors.gray}({formatTime elapsed}){Colors.reset}"
        println $"    Failed to load test: {msg}"
        let failedInfo =
            { File = testPath
              Name = $"{suiteLabel}: {testName}"
              Message = $"Failed to load test: {msg}"
              Details = [] }
        ProgressBar.update progress
        { Passed = 0; Failed = 1; FailedTests = [ failedInfo ] }

    // Run ANF→MIR tests
    let anf2mirTests = anf2mirTestFiles |> Array.filter (fun p -> matchesFilter filter (Path.GetFileName p))
    let runANF2MIRFile =
        runPassTestFile loadANF2MIRTest (fun (input, expected) -> runANF2MIRTest input expected)
    runFileSuite
        runState
        symbols
        "📦 ANF→MIR Tests"
        "ANF→MIR"
        anf2mirTests
        Path.GetFileName
        (fun testName -> $"ANF→MIR: {testName}")
        runANF2MIRFile
        (handlePassTestSuccess "ANF→MIR")
        (handlePassTestError "ANF→MIR")

    // Run MIR→LIR tests
    let mir2lirTests = mir2lirTestFiles |> Array.filter (fun p -> matchesFilter filter (Path.GetFileName p))
    let runMIR2LIRFile =
        runPassTestFile loadMIR2LIRTest (fun (input, expected) -> runMIR2LIRTest input expected)
    runFileSuite
        runState
        symbols
        "🔄 MIR→LIR Tests"
        "MIR→LIR"
        mir2lirTests
        Path.GetFileName
        (fun testName -> $"MIR→LIR: {testName}")
        runMIR2LIRFile
        (handlePassTestSuccess "MIR→LIR")
        (handlePassTestError "MIR→LIR")

    // Run LIR→ARM64 tests
    let lir2arm64Tests = lir2arm64TestFiles |> Array.filter (fun p -> matchesFilter filter (Path.GetFileName p))
    let runLIR2ARM64File =
        runPassTestFile loadLIR2ARM64Test (fun (input, expected) -> runLIR2ARM64Test input expected)
    runFileSuite
        runState
        symbols
        "🎯 LIR→ARM64 Tests"
        "LIR→ARM64"
        lir2arm64Tests
        Path.GetFileName
        (fun testName -> $"LIR→ARM64: {testName}")
        runLIR2ARM64File
        (handlePassTestSuccess "LIR→ARM64")
        (handlePassTestError "LIR→ARM64")

    // Run ARM64 encoding tests
    let arm64encTests = arm64encTestFiles |> Array.filter (fun p -> matchesFilter filter (Path.GetFileName p))
    let runARM64EncodingFile =
        runPassTestFile
            TestDSL.ARM64EncodingTestRunner.loadARM64EncodingTest
            TestDSL.ARM64EncodingTestRunner.runARM64EncodingTest
    runFileSuite
        runState
        symbols
        "⚙️  ARM64 Encoding Tests"
        "ARM64 Enc"
        arm64encTests
        Path.GetFileName
        (fun testName -> $"ARM64 Encoding: {testName}")
        runARM64EncodingFile
        (handlePassTestSuccess "ARM64 Encoding")
        (handlePassTestError "ARM64 Encoding")

    // Run Type Checking tests
    let typecheckTests =
        typecheckTestFiles
        |> Array.filter (fun p -> matchesFilter filter (Path.GetFileNameWithoutExtension p))
    let runTypecheckFile testFile =
        TestDSL.TypeCheckingTestRunner.runTypeCheckingTestFile testFile
    let handleTypecheckSuccess
        (progress: ProgressBar.State)
        (testPath: string)
        (fileName: string)
        (_: TimeSpan)
        (results: TestDSL.TypeCheckingTestRunner.TypeCheckingTestResult list)
        : FileSuiteSummary =
        let fileSuccess = results |> List.forall (fun r -> r.Success)
        let filePassCount = results |> List.filter (fun r -> r.Success) |> List.length
        let fileFailCount = results |> List.filter (fun r -> not r.Success) |> List.length
        if fileSuccess then
            ProgressBar.increment progress true
            { Passed = filePassCount; Failed = 0; FailedTests = [] }
        else
            let failures = ResizeArray<FailedTestInfo>()
            for result in results do
                if not result.Success then
                    ProgressBar.increment progress false
                    ProgressBar.finish progress
                    let typeDesc =
                        match result.ExpectedType with
                        | Some t -> TypeChecking.typeToString t
                        | None -> "error"
                    println $"  {typeDesc} ({fileName})... {Colors.red}✗ FAIL{Colors.reset}"
                    println $"    {result.Message}"
                    failures.Add({ File = testPath; Name = $"Type Checking: {typeDesc} ({fileName})"; Message = result.Message; Details = [] })
                    ProgressBar.update progress
            { Passed = filePassCount; Failed = fileFailCount; FailedTests = failures |> Seq.toList }
    let handleTypecheckError
        (progress: ProgressBar.State)
        (testPath: string)
        (_: string)
        (_: TimeSpan)
        (msg: string)
        : FileSuiteSummary =
        ProgressBar.increment progress false
        ProgressBar.finish progress
        println $"  {Colors.red}✗ ERROR parsing {Path.GetFileName testPath}{Colors.reset}"
        println $"    {msg}"
        let failedInfo =
            { File = testPath
              Name = $"Type Checking: {Path.GetFileName testPath}"
              Message = msg
              Details = [] }
        ProgressBar.update progress
        { Passed = 0; Failed = 1; FailedTests = [ failedInfo ] }
    runFileSuite
        runState
        symbols
        "📋 Type Checking Tests"
        "TypeCheck"
        typecheckTests
        (fun testPath -> Path.GetFileNameWithoutExtension (testPath: string))
        (fun fileName -> $"TypeCheck: {fileName}")
        runTypecheckFile
        handleTypecheckSuccess
        handleTypecheckError

    // Run Optimization Tests (ANF, MIR, LIR)
    if optTestFiles.Length > 0 then
        let runOptimizationFile testFile =
            let fileName = Path.GetFileNameWithoutExtension (testFile: string)
            let stage =
                if fileName.ToLower().Contains("anf") then TestDSL.OptimizationFormat.ANF
                elif fileName.ToLower().Contains("mir") then TestDSL.OptimizationFormat.MIR
                elif fileName.ToLower().Contains("lir") then TestDSL.OptimizationFormat.LIR
                else TestDSL.OptimizationFormat.ANF
            TestDSL.OptimizationTestRunner.runTestFile stdlib stage testFile
        let handleOptimizationSuccess
            (progress: ProgressBar.State)
            (testPath: string)
            (_: string)
            (_: TimeSpan)
            (results: (TestDSL.OptimizationFormat.OptimizationTest * TestDSL.OptimizationTestRunner.OptimizationTestResult) list)
            : FileSuiteSummary =
            let filteredResults = results |> List.filter (fun (test, _) -> matchesFilter filter test.Name)
            let fileSuccess = filteredResults |> List.forall (fun (_, r) -> r.Success)
            let filePassCount = filteredResults |> List.filter (fun (_, r) -> r.Success) |> List.length
            let fileFailCount = filteredResults |> List.filter (fun (_, r) -> not r.Success) |> List.length
            if fileSuccess then
                ProgressBar.increment progress true
                { Passed = filePassCount; Failed = 0; FailedTests = [] }
            else
                ProgressBar.increment progress false
                ProgressBar.finish progress
                let failures = ResizeArray<FailedTestInfo>()
                for (test, result) in filteredResults do
                    if not result.Success then
                        println $"  {test.Name}... {Colors.red}✗ FAIL{Colors.reset}"
                        println $"    {result.Message}"
                        let details = addExpectedActualDetails result.Expected result.Actual
                        failures.Add({ File = testPath; Name = $"Optimization: {test.Name}"; Message = result.Message; Details = details })
                ProgressBar.update progress
                { Passed = filePassCount; Failed = fileFailCount; FailedTests = failures |> Seq.toList }
        let handleOptimizationError
            (progress: ProgressBar.State)
            (testPath: string)
            (_: string)
            (_: TimeSpan)
            (msg: string)
            : FileSuiteSummary =
            ProgressBar.increment progress false
            ProgressBar.finish progress
            println $"  {Colors.red}✗ ERROR parsing {Path.GetFileName testPath}{Colors.reset}"
            println $"    {msg}"
            let failedInfo =
                { File = testPath
                  Name = $"Optimization: {Path.GetFileName testPath}"
                  Message = msg
                  Details = [] }
            ProgressBar.update progress
            { Passed = 0; Failed = 1; FailedTests = [ failedInfo ] }
        runFileSuite
            runState
            symbols
            "⚡ Optimization Tests"
            "Optimization"
            optTestFiles
            (fun testPath -> Path.GetFileNameWithoutExtension (testPath: string))
            (fun fileName -> $"Optimization: {fileName}")
            runOptimizationFile
            handleOptimizationSuccess
            handleOptimizationError

    // Order unit test suites so non-stdlib tests run first.
    let splitUnitTestsByStdlibNeed
        (needsStdlib: string list)
        (suites: UnitTestSuite array)
        : UnitTestSuite array * UnitTestSuite array =
        let needsStdlibSet = Set.ofList needsStdlib
        let needsStdlibSuites, noStdlibSuites =
            suites
            |> Array.partition (fun suite -> Set.contains suite.Name needsStdlibSet)
        (noStdlibSuites, needsStdlibSuites)

    let unitTests = allUnitTests |> Array.filter (fun suite -> matchesFilter filter suite.Name)

    let (unitTestsNoStdlib, unitTestsWithStdlib) =
        splitUnitTestsByStdlibNeed unitStdlibSuites unitTests
    let unitTestsOrdered = Array.append unitTestsNoStdlib unitTestsWithStdlib

    let runE2EAndVerification
        (baseStdlib: CompilerLibrary.StdlibResult)
        : unit =
        if e2eTestFiles.Length > 0 then
                let sectionTimer = Stopwatch.StartNew()
                println $"{Colors.cyan}🚀 E2E Tests{Colors.reset}"

                let parseTimer = Stopwatch.StartNew()
                let (allE2ETests, parseErrors) = loadE2ETests e2eTestFiles
                parseTimer.Stop()
                recordNonPassTiming "E2E Test Parse" parseTimer.Elapsed
                reportParseErrors "E2E" parseErrors

                if allE2ETests.Length > 0 then
                    let testsArray =
                        allE2ETests
                        |> Array.filter (fun test -> matchesFilter filter test.Name)
                    if testsArray.Length > 0 then
                        let timingText = $"(Stdlib compiled in {formatTime elapsed})"
                        println $"  {Colors.gray}{timingText}{Colors.reset}"
                        runE2ESuite baseStdlib "E2E" "E2E" testsArray

                sectionTimer.Stop()
                println $"  {Colors.gray}└─ Completed in {formatTime sectionTimer.Elapsed}{Colors.reset}"
                println ""

        if enableVerification && verificationTestFiles.Length > 0 then
                let sectionTimer = Stopwatch.StartNew()
                println $"{Colors.cyan}🔬 Verification Tests{Colors.reset}"

                let parseTimer = Stopwatch.StartNew()
                let (allVerifTests, parseErrors) = loadE2ETests verificationTestFiles
                parseTimer.Stop()
                recordNonPassTiming "Verification Test Parse" parseTimer.Elapsed
                reportParseErrors "Verification" parseErrors

                if allVerifTests.Length > 0 then
                    let testsArray =
                        allVerifTests
                        |> Array.filter (fun test -> matchesFilter filter test.Name)
                    if testsArray.Length > 0 then
                        runE2ESuite baseStdlib "Verification" "Verification" testsArray

                sectionTimer.Stop()
                println $"  {Colors.gray}└─ Completed in {formatTime sectionTimer.Elapsed}{Colors.reset}"
                println ""

    // Run tests sequentially
    runUnitTestSuites runState symbols "🔧 Unit Tests" "Unit" unitTestsOrdered
    runE2EAndVerification stdlib

    // Compute stdlib coverage only if --coverage flag is set
    let coveragePercent =
        if not showCoverage then None
        else
            let allStdlibFuncs = CompilerLibrary.getAllStdlibFunctionNamesFromStdlib stdlib
            let coveredFuncs = System.Collections.Generic.HashSet<string>()
            if e2eTestFiles.Length > 0 then
                for testFile in e2eTestFiles do
                    match TestDSL.E2EFormat.parseE2ETestFile testFile with
                    | Error _ -> ()
                    | Ok tests ->
                        for test in tests do
                            match CompilerLibrary.getReachableStdlibFunctionsFromStdlib stdlib test.Source with
                            | Error _ -> ()
                            | Ok reachable ->
                                for func in reachable do
                                    if Set.contains func allStdlibFuncs then
                                        coveredFuncs.Add(func) |> ignore
                let totalFuncs = Set.count allStdlibFuncs
                let coveredCount = coveredFuncs.Count
                if totalFuncs > 0 then Some (float coveredCount / float totalFuncs * 100.0) else None
            else None

    totalTimer.Stop()
    let unaccountedBreakdown =
        TestFramework.calculateUnaccountedTimeBreakdown
            totalTimer.Elapsed
            runState.PassTimings
            runState.Timings

    // Print slowest tests
    if runState.Timings.Count > 0 then
        println $"{Colors.bold}{Colors.gray}═══════════════════════════════════════{Colors.reset}"
        println $"{Colors.bold}{Colors.gray}🐢 Slowest Tests{Colors.reset}"
        println $"{Colors.bold}{Colors.gray}═══════════════════════════════════════{Colors.reset}"
        let slowest = runState.Timings |> Seq.sortByDescending (fun t -> t.TotalTime) |> Seq.truncate 5 |> Seq.toList
        for (i, timing) in slowest |> List.indexed do
            let baseTimingStr =
                match timing.CompileTime, timing.RuntimeTime with
                | Some ct, Some rt ->
                    $"compile: {formatTime ct}  run: {formatTime rt}  total: {formatTime timing.TotalTime}"
                | _ ->
                    $"total: {formatTime timing.TotalTime}"
            let timingStr = baseTimingStr
            let displayName = if timing.Name.Length > 45 then timing.Name.Substring(0, 42) + "..." else timing.Name
            println $"  {Colors.gray}{i + 1}. {displayName,-45} {timingStr}{Colors.reset}"
        println ""

    println $"{Colors.bold}{Colors.gray}═══════════════════════════════════════{Colors.reset}"
    println $"{Colors.bold}{Colors.gray}⏱ Suite Timings{Colors.reset}"
    println $"{Colors.bold}{Colors.gray}═══════════════════════════════════════{Colors.reset}"
    let columns =
        TestFramework.buildPassTimingColumns
            runState.PassTimings
            (runState.PassTimingOrder |> Seq.toList)
            unaccountedBreakdown.Unaccounted
    let formatColumn (sections: TestFramework.PassTimingSection list) : string list =
        let entries = sections |> List.collect (fun section -> section.Entries)
        let formatSeconds (elapsed: TimeSpan) : string =
            if elapsed.TotalMilliseconds < 100.0 then
                ">0.1s"
            else
                let seconds = elapsed.TotalSeconds.ToString("0.0")
                $"{seconds}s"
        let numberText (entry: TestFramework.PassTimingEntry) : string =
            match entry.Number with
            | Some number -> number
            | None -> ""
        let numberWidth =
            entries
            |> List.map (fun entry -> (numberText entry).Length)
            |> List.fold max 0
        let numberPadWidth = if numberWidth > 0 then numberWidth + 2 else 0
        let labelFor (entry: TestFramework.PassTimingEntry) : string =
            let numberPadded = (numberText entry).PadRight numberPadWidth
            if numberPadWidth > 0 then $"{numberPadded}{entry.Name}" else entry.Name
        let labelWidth =
            entries
            |> List.map (fun entry -> (labelFor entry).Length)
            |> List.fold max 0
        let timeWidth =
            entries
            |> List.map (fun entry -> (formatSeconds entry.Elapsed).Length)
            |> List.fold max 0
        let formatEntry (entry: TestFramework.PassTimingEntry) : string =
            let label = labelFor entry
            let timeText = formatSeconds entry.Elapsed
            let paddedTime = timeText.PadLeft timeWidth
            let timeWithColor =
                if entry.Elapsed.TotalSeconds > 3.0 then
                    $"{Colors.red}{paddedTime}{Colors.gray}"
                elif entry.Elapsed.TotalSeconds > 2.0 then
                    $"{Colors.yellow}{paddedTime}{Colors.gray}"
                elif entry.Elapsed.TotalSeconds > 1.0 then
                    $"{Colors.white}{paddedTime}{Colors.gray}"
                else
                    paddedTime
            $"  {label.PadRight labelWidth}  {timeWithColor}"
        let sectionCount = List.length sections
        sections
        |> List.mapi (fun idx section ->
            let entryLines =
                if List.isEmpty section.Entries then
                    [ "  (none)" ]
                else
                    section.Entries |> List.map formatEntry
            let lines = section.Title :: entryLines
            if idx < sectionCount - 1 then lines @ [ "" ] else lines)
        |> List.collect id
    let leftLines = formatColumn columns.Ordered
    let rightLines = formatColumn columns.ByTime
    let visibleLength (text: string) : int =
        [ Colors.red; Colors.yellow; Colors.white; Colors.gray; Colors.bold; Colors.reset ]
        |> List.fold (fun (acc: string) (code: string) -> acc.Replace(code, "")) text
        |> fun stripped -> stripped.Length
    let padRightVisible (text: string) (width: int) : string =
        let visible = visibleLength text
        if visible >= width then text else text + String(' ', width - visible)
    let leftWidth =
        leftLines
        |> List.map visibleLength
        |> List.fold max 0
    let gap = "  "
    let lineCount = max leftLines.Length rightLines.Length
    for idx in 0 .. lineCount - 1 do
        let left = if idx < leftLines.Length then leftLines.[idx] else ""
        let right = if idx < rightLines.Length then rightLines.[idx] else ""
        let paddedLeft = padRightVisible left leftWidth
        println $"  {Colors.gray}{paddedLeft}{gap}{right}{Colors.reset}"
    println ""

    println $"{Colors.bold}{Colors.cyan}═══════════════════════════════════════{Colors.reset}"
    println $"{Colors.bold}{Colors.cyan}📊 Test Results{Colors.reset}"
    println $"{Colors.bold}{Colors.cyan}═══════════════════════════════════════{Colors.reset}"
    if runState.Failed = 0 then
        println $"  {Colors.green}✓ All tests passed: {runState.Passed}/{runState.Passed + runState.Failed}{Colors.reset}"
    else
        println $"  {Colors.green}✓ Passed: {runState.Passed}{Colors.reset}"
        println $"  {Colors.red}✗ Failed: {runState.Failed}{Colors.reset}"
    match coveragePercent with
    | Some pct -> println $"  {Colors.gray}📊 Stdlib coverage: {pct:F1}%%{Colors.reset}"
    | None -> ()
    println
        $"  {Colors.gray}⏱  Unaccounted time: {formatTime unaccountedBreakdown.Unaccounted} (runtime: {formatTime unaccountedBreakdown.Runtime}, overhead: {formatTime unaccountedBreakdown.Overhead}){Colors.reset}"
    println $"  {Colors.gray}⏱  Total time: {formatTime totalTimer.Elapsed}{Colors.reset}"
    println $"{Colors.bold}{Colors.cyan}═══════════════════════════════════════{Colors.reset}"

    // Print first 10 failing tests summary
    if runState.FailedTests.Count > 0 then
        println ""
        println $"{Colors.bold}{Colors.red}═══════════════════════════════════════{Colors.reset}"
        let displayCount = min 10 runState.FailedTests.Count
        let moreCount = runState.FailedTests.Count - displayCount
        if moreCount > 0 then
            println $"{Colors.bold}{Colors.red}❌ First {displayCount} Failing Tests (of {runState.FailedTests.Count} total){Colors.reset}"
        else
            println $"{Colors.bold}{Colors.red}❌ Failing Tests ({runState.FailedTests.Count}){Colors.reset}"
        println $"{Colors.bold}{Colors.red}═══════════════════════════════════════{Colors.reset}"
        println ""

        for i in 0 .. displayCount - 1 do
            let test = runState.FailedTests.[i]
            let fileName = if test.File <> "" then Path.GetFileName test.File else ""
            // Format: "1. E2E: file.e2e:L43: expression" with file:line in cyan
            let displayName =
                if fileName <> "" && test.Name.StartsWith("E2E: L") then
                    let afterPrefix = test.Name.Substring(5)  // "L43: expression" (skip "E2E: ")
                    $"E2E: {Colors.cyan}{fileName}:{afterPrefix}{Colors.reset}"
                elif fileName <> "" then
                    $"{Colors.cyan}{fileName}: {Colors.reset}{Colors.red}{test.Name}"
                else
                    test.Name
            println $"{Colors.red}{i + 1}. {displayName}{Colors.reset}"
            println $"   {Colors.gray}{test.Message}{Colors.reset}"
            for detail in test.Details do
                println $"   {Colors.gray}{detail}{Colors.reset}"
            println ""

        if moreCount > 0 then
            println $"{Colors.gray}... and {moreCount} more failing test(s){Colors.reset}"
            println ""

    (if runState.Failed = 0 then 0 else 1)
    |> fun exitCode ->
        exitCode
