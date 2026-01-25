// RegisterAllocationTimingTests.fs - Unit tests for register allocation timing instrumentation
//
// Verifies timing phase reporting and ensures timed allocation matches untimed allocation.

module RegisterAllocationTimingTests

open LIR
open LIRSymbolic

/// Test result type
type TestResult = Result<unit, string>

/// Create a label
let makeLabel (name: string) = LIR.Label name

/// Create a virtual register
let vr (n: int) = LIR.Virtual n

/// Create a VReg operand
let vreg (n: int) = Reg (LIR.Virtual n)

/// Create a basic block with return terminator
let makeRetBlock (label: Label) (instrs: Instr list) : BasicBlock =
    { Label = label; Instrs = instrs; Terminator = Ret }

/// Build a CFG from a list of blocks
let makeCFG (entry: Label) (blocks: BasicBlock list) : CFG =
    let blockMap = blocks |> List.map (fun b -> (b.Label, b)) |> Map.ofList
    { Entry = entry; Blocks = blockMap }

/// Build a simple function that exercises register allocation
let buildSimpleFunction () : LIRSymbolic.Function =
    let entry = makeLabel "entry"
    let instrs = [
        Mov (vr 0, Imm 1L)
        Mov (vr 1, Imm 2L)
        Add (vr 2, vr 0, vreg 1)
        Mov (vr 3, vreg 2)
    ]
    let block = makeRetBlock entry instrs
    let cfg = makeCFG entry [block]
    { Name = "ra_timing_test"
      TypedParams = []
      CFG = cfg
      StackSize = 0
      UsedCalleeSaved = [] }

let expectedPhases = [
    "RegAlloc: Liveness"
    "RegAlloc: Interference Graph"
    "RegAlloc: Interference Graph - Live Iteration"
    "RegAlloc: Interference Graph - Adjacency Updates"
    "RegAlloc: Coalescing Prep"
    "RegAlloc: Coloring"
    "RegAlloc: Coloring - Coalesce"
    "RegAlloc: Coloring - MCS Select"
    "RegAlloc: Coloring - MCS Bucket Update"
    "RegAlloc: Coloring - Greedy"
    "RegAlloc: Coloring - Expand"
    "RegAlloc: Float Liveness"
    "RegAlloc: Float Allocation"
    "RegAlloc: Param Moves"
    "RegAlloc: Phi Resolution"
    "RegAlloc: Apply Allocation"
    "RegAlloc: Finalize"
]

let testAllocateRegistersWithTimingPhases () : TestResult =
    let func = buildSimpleFunction ()
    let (timedFunc, timings) = RegisterAllocation.allocateRegistersWithTiming func
    let baselineFunc = RegisterAllocation.allocateRegisters func

    if timedFunc <> baselineFunc then
        Error "Timed allocation differs from baseline allocation"
    else
        let phases = timings |> List.map (fun timing -> timing.Phase)
        if phases <> expectedPhases then
            let phaseList = String.concat ", " phases
            Error $"Unexpected timing phases: {phaseList}"
        else if timings |> List.exists (fun timing -> timing.ElapsedMs < 0.0) then
            Error "Timing contained a negative elapsed value"
        else
            Ok ()

let tests = [
    ("register allocation timing phases", testAllocateRegistersWithTimingPhases)
]

/// Run all register allocation timing tests
let runAll () : TestResult =
    let rec runTests tests =
        match tests with
        | [] -> Ok ()
        | (name, test) :: rest ->
            match test () with
            | Error e -> Error $"FAIL: {name}: {e}"
            | Ok () -> runTests rest

    runTests tests
