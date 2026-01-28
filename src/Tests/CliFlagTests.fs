// CliFlagTests.fs - Unit tests for CLI flag parsing
//
// Verifies that IR dump flags are accepted and mapped into CLI options.

module CliFlagTests

open Program

/// Test result type
type TestResult = Result<unit, string>

/// Test that IR dump flags parse and set the expected options
let testDumpIrFlags () : TestResult =
    let args = [| "--dump-anf"; "--dump-mir"; "--dump-lir"; "input.dark" |]
    match parseArgs args with
    | Error msg -> Error $"Expected dump flags to parse, got error: {msg}"
    | Ok opts ->
        if not opts.DumpANF then
            Error "Expected DumpANF to be true"
        else if not opts.DumpMIR then
            Error "Expected DumpMIR to be true"
        else if not opts.DumpLIR then
            Error "Expected DumpLIR to be true"
        else
            Ok ()

/// Test that verbosity levels map to the expected normal output behavior
let testShouldShowNormal () : TestResult =
    if Program.shouldShowNormal Quiet then
        Error "Expected Quiet to suppress normal output"
    else if not (Program.shouldShowNormal Normal) then
        Error "Expected Normal to show output"
    else if not (Program.shouldShowNormal Verbose) then
        Error "Expected Verbose to show output"
    else if not (Program.shouldShowNormal VeryVerbose) then
        Error "Expected VeryVerbose to show output"
    else if not (Program.shouldShowNormal DumpIR) then
        Error "Expected DumpIR to show output"
    else
        Ok ()

/// Test that leak-check flag parses and sets the expected option
let testLeakCheckFlag () : TestResult =
    let args = [| "--leak-check"; "input.dark" |]
    match parseArgs args with
    | Error msg -> Error $"Expected leak-check flag to parse, got error: {msg}"
    | Ok opts ->
        if not opts.LeakCheck then
            Error "Expected LeakCheck to be true"
        else
            Ok ()

/// Test that ARM64 emit profiling flag parses and sets the expected option
let testARM64EmitProfileFlag () : TestResult =
    let args = [| "--profile-arm64-emit"; "input.dark" |]
    match parseArgs args with
    | Error msg -> Error $"Expected ARM64 emit profiling flag to parse, got error: {msg}"
    | Ok opts ->
        if not opts.ProfileARM64Emit then
            Error "Expected ProfileARM64Emit to be true"
        else
            Ok ()

/// Test that ANF optimization flags parse and set the expected options
let testANFOptFlags () : TestResult =
    let args = [|
        "--disable-opt-anf-const-folding"
        "--disable-opt-anf-const-prop"
        "--disable-opt-anf-copy-prop"
        "--disable-opt-anf-dce"
        "--disable-opt-anf-strength-reduction"
        "input.dark"
    |]
    match parseArgs args with
    | Error msg -> Error $"Expected ANF opt flags to parse, got error: {msg}"
    | Ok opts ->
        if not opts.DisableANFConstFolding then
            Error "Expected DisableANFConstFolding to be true"
        else if not opts.DisableANFConstProp then
            Error "Expected DisableANFConstProp to be true"
        else if not opts.DisableANFCopyProp then
            Error "Expected DisableANFCopyProp to be true"
        else if not opts.DisableANFDCE then
            Error "Expected DisableANFDCE to be true"
        else if not opts.DisableANFStrengthReduction then
            Error "Expected DisableANFStrengthReduction to be true"
        else
            Ok ()

/// Test that MIR optimization flags parse and set the expected options
let testMIROptFlags () : TestResult =
    let args = [|
        "--disable-opt-mir-const-folding"
        "--disable-opt-mir-cse"
        "--disable-opt-mir-copy-prop"
        "--disable-opt-mir-dce"
        "--disable-opt-mir-cfg-simplify"
        "--disable-opt-mir-licm"
        "input.dark"
    |]
    match parseArgs args with
    | Error msg -> Error $"Expected MIR opt flags to parse, got error: {msg}"
    | Ok opts ->
        if not opts.DisableMIRConstFolding then
            Error "Expected DisableMIRConstFolding to be true"
        else if not opts.DisableMIRCSE then
            Error "Expected DisableMIRCSE to be true"
        else if not opts.DisableMIRCopyProp then
            Error "Expected DisableMIRCopyProp to be true"
        else if not opts.DisableMIRDCE then
            Error "Expected DisableMIRDCE to be true"
        else if not opts.DisableMIRCFGSimplify then
            Error "Expected DisableMIRCFGSimplify to be true"
        else if not opts.DisableMIRLICM then
            Error "Expected DisableMIRLICM to be true"
        else
            Ok ()

/// Test that LIR peephole flag parses and sets the expected option
let testLIRPeepholeFlag () : TestResult =
    let args = [| "--disable-opt-lir-peephole"; "input.dark" |]
    match parseArgs args with
    | Error msg -> Error $"Expected LIR peephole flag to parse, got error: {msg}"
    | Ok opts ->
        if not opts.DisableLIRPeephole then
            Error "Expected DisableLIRPeephole to be true"
        else
            Ok ()

/// Test that function tree shaking flag parses and sets the expected option
let testFunctionTreeShakingFlag () : TestResult =
    let args = [| "--disable-opt-function-tree-shaking"; "input.dark" |]
    match parseArgs args with
    | Error msg -> Error $"Expected function tree shaking flag to parse, got error: {msg}"
    | Ok opts ->
        if not opts.DisableFunctionTreeShaking then
            Error "Expected DisableFunctionTreeShaking to be true"
        else
            Ok ()

/// Test that cache flags are rejected now that caching is removed
let testCacheFlagsRejected () : TestResult =
    let checkReject (args: string array) (label: string) : TestResult =
        match parseArgs args with
        | Ok _ -> Error $"Expected {label} to be rejected"
        | Error _ -> Ok ()

    checkReject [| "--no-cache"; "input.dark" |] "--no-cache"
    |> Result.bind (fun () -> checkReject [| "--cache-key" |] "--cache-key")


/// Test compiler option mapping from CLI options
let testBuildCompilerOptions () : TestResult =
    let cliOpts = {
        defaultOptions with
            DisableFreeList = true
            DisableANFOpt = true
            DisableANFConstFolding = true
            DisableANFConstProp = true
            DisableANFCopyProp = true
            DisableANFDCE = true
            DisableANFStrengthReduction = true
            DisableInlining = true
            DisableTCO = true
            DisableMIROpt = true
            DisableMIRConstFolding = true
            DisableMIRCSE = true
            DisableMIRCopyProp = true
            DisableMIRDCE = true
            DisableMIRCFGSimplify = true
            DisableMIRLICM = true
            DisableLIROpt = true
            DisableLIRPeephole = true
            DisableFunctionTreeShaking = true
            LeakCheck = true
            ProfileARM64Emit = true
            DumpANF = true
            DumpMIR = true
            DumpLIR = true
    }
    let compilerOpts = Program.buildCompilerOptions cliOpts
    if not compilerOpts.DisableFreeList then
        Error "Expected DisableFreeList to map into CompilerOptions"
    else if not compilerOpts.DisableANFOpt then
        Error "Expected DisableANFOpt to map into CompilerOptions"
    else if not compilerOpts.DisableANFConstFolding then
        Error "Expected DisableANFConstFolding to map into CompilerOptions"
    else if not compilerOpts.DisableANFConstProp then
        Error "Expected DisableANFConstProp to map into CompilerOptions"
    else if not compilerOpts.DisableANFCopyProp then
        Error "Expected DisableANFCopyProp to map into CompilerOptions"
    else if not compilerOpts.DisableANFDCE then
        Error "Expected DisableANFDCE to map into CompilerOptions"
    else if not compilerOpts.DisableANFStrengthReduction then
        Error "Expected DisableANFStrengthReduction to map into CompilerOptions"
    else if not compilerOpts.DisableInlining then
        Error "Expected DisableInlining to map into CompilerOptions"
    else if not compilerOpts.DisableTCO then
        Error "Expected DisableTCO to map into CompilerOptions"
    else if not compilerOpts.DisableMIROpt then
        Error "Expected DisableMIROpt to map into CompilerOptions"
    else if not compilerOpts.DisableMIRConstFolding then
        Error "Expected DisableMIRConstFolding to map into CompilerOptions"
    else if not compilerOpts.DisableMIRCSE then
        Error "Expected DisableMIRCSE to map into CompilerOptions"
    else if not compilerOpts.DisableMIRCopyProp then
        Error "Expected DisableMIRCopyProp to map into CompilerOptions"
    else if not compilerOpts.DisableMIRDCE then
        Error "Expected DisableMIRDCE to map into CompilerOptions"
    else if not compilerOpts.DisableMIRCFGSimplify then
        Error "Expected DisableMIRCFGSimplify to map into CompilerOptions"
    else if not compilerOpts.DisableMIRLICM then
        Error "Expected DisableMIRLICM to map into CompilerOptions"
    else if not compilerOpts.DisableLIROpt then
        Error "Expected DisableLIROpt to map into CompilerOptions"
    else if not compilerOpts.DisableLIRPeephole then
        Error "Expected DisableLIRPeephole to map into CompilerOptions"
    else if not compilerOpts.DisableFunctionTreeShaking then
        Error "Expected DisableFunctionTreeShaking to map into CompilerOptions"
    else if not compilerOpts.DumpANF then
        Error "Expected DumpANF to map into CompilerOptions"
    else if not compilerOpts.DumpMIR then
        Error "Expected DumpMIR to map into CompilerOptions"
    else if not compilerOpts.DumpLIR then
        Error "Expected DumpLIR to map into CompilerOptions"
    else if not compilerOpts.EnableLeakCheck then
        Error "Expected EnableLeakCheck to map into CompilerOptions"
    else if not compilerOpts.EnableARM64EmitProfiling then
        Error "Expected EnableARM64EmitProfiling to map into CompilerOptions"
    else if compilerOpts.EnableCoverage then
        Error "Expected EnableCoverage to remain false"
    else
        Ok ()

let tests = [
    ("IR dump flags", testDumpIrFlags)
    ("show normal output", testShouldShowNormal)
    ("leak check flag", testLeakCheckFlag)
    ("ARM64 emit profiling flag", testARM64EmitProfileFlag)
    ("ANF opt flags", testANFOptFlags)
    ("MIR opt flags", testMIROptFlags)
    ("LIR peephole flag", testLIRPeepholeFlag)
    ("function tree shaking flag", testFunctionTreeShakingFlag)
    ("cache flags rejected", testCacheFlagsRejected)
    ("build compiler options", testBuildCompilerOptions)
]

/// Run all CLI flag unit tests
let runAll () : TestResult =
    let rec runTests = function
        | [] -> Ok ()
        | (name, test) :: rest ->
            match test () with
            | Ok () -> runTests rest
            | Error msg -> Error $"{name} test failed: {msg}"

    runTests tests
