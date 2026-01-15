// 5.5_FunctionTreeShaking.fs - Function Tree Shaking Pass
//
// Prunes unused user and stdlib functions by walking call graphs.
// This keeps code generation and binary size focused on reachable functions.

module FunctionTreeShaking

/// Build root set for user reachability (_start when present, otherwise all functions)
let private buildUserRoots (functions: LIR.Function list) : Set<string> =
    if functions |> List.exists (fun f -> f.Name = "_start") then
        Set.ofList ["_start"]
    else
        functions |> List.map (fun f -> f.Name) |> Set.ofList

/// Filter user functions to only include reachable ones
let filterUserFunctions (functions: LIR.Function list) : LIR.Function list =
    let roots = buildUserRoots functions
    let userCallGraph = DeadCodeElimination.buildCallGraph functions
    let reachableNames = DeadCodeElimination.findReachable userCallGraph roots
    functions |> List.filter (fun f -> Set.contains f.Name reachableNames)

/// Filter stdlib functions to only include those reachable from user code
let filterStdlibFunctions
    (stdlibCallGraph: Map<string, Set<string>>)
    (userFunctions: LIR.Function list)
    (stdlibFunctions: LIR.Function list)
    : LIR.Function list =
    DeadCodeElimination.filterFunctions stdlibCallGraph userFunctions stdlibFunctions

/// Compute reachable stdlib function names from a user ANF program
let getReachableStdlibNames
    (stdlibCallGraph: Map<string, Set<string>>)
    (userProgram: ANF.Program)
    : Set<string> =
    let (ANF.Program (userFuncs, userMainExpr)) = userProgram
    let startFunc = { ANF.Name = "_start"; ANF.TypedParams = []; ANF.ReturnType = AST.TUnit; ANF.Body = userMainExpr }
    let userFuncsWithStart = startFunc :: userFuncs
    ANFDeadCodeElimination.getReachableStdlib stdlibCallGraph userFuncsWithStart
