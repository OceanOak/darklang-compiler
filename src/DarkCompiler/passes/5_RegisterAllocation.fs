// 5_RegisterAllocation.fs - Register Allocation with Liveness Analysis (Pass 5)
//
// Allocates physical ARM64 registers to virtual registers in LIR CFG.
//
// Algorithm:
// 1. Compute liveness information using backward dataflow analysis
// 2. Build interference graph from liveness data
// 3. Use chordal graph coloring (SSA guarantees chordal interference graphs)
//    - Maximum Cardinality Search for Perfect Elimination Ordering (PEO)
//    - Greedy coloring in reverse PEO order (optimal for chordal graphs)
//    - Phi coalescing preferences to reduce register moves
// 4. Spill to stack when register pressure exceeds available registers
//
// General-Purpose Registers:
// - X0: reserved for return values
// - X1-X8: caller-saved (preferred for allocation)
// - X9-X10: excluded (used as compiler scratch registers)
// - X11-X13: reserved as scratch registers for spill code
// - X19-X26: callee-saved (used when caller-saved exhausted)
// - X27: reserved for free list base pointer
// - X28: reserved for heap bump pointer
// - X29: frame pointer
// - X30: link register
//
// Float Registers:
// - D0: float return value
// - D0-D7: caller-saved (saved around calls when live)
// - D8-D15: callee-saved
//
// Callee-saved registers (X19-X26, D8-D15) are saved/restored in prologue/epilogue.
//
// See docs/features/register-allocation.md for detailed documentation.

module RegisterAllocation

// ============================================================================
// Types
// ============================================================================

/// Result of register allocation
type AllocationResult = {
    Mapping: Map<int, Allocation>
    StackSize: int
    UsedCalleeSaved: LIR.PhysReg list
}

/// Allocation target for a virtual register
and Allocation =
    | PhysReg of LIR.PhysReg
    | StackSlot of int

/// Live interval for a virtual register
type LiveInterval = {
    VRegId: int
    Start: int
    End: int
}

/// Liveness information for a basic block
type BlockLiveness = {
    LiveIn: Set<int>
    LiveOut: Set<int>
}

// ============================================================================
// Chordal Graph Coloring Types
// ============================================================================

/// Interference graph for register allocation
/// In SSA form, this graph is guaranteed to be chordal
type InterferenceGraph = {
    Vertices: Set<int>              // VReg IDs
    Edges: Map<int, Set<int>>       // Adjacency list (symmetric)
}

/// Result of graph coloring
type ColoringResult = {
    Colors: Map<int, int>           // VRegId → color (0..k-1)
    Spills: Set<int>                // VRegs that must be spilled
    ChromaticNumber: int            // Max color used + 1
}

/// Profiling data for Maximum Cardinality Search
type McsProfile = {
    VertexCount: int
    SelectionChecks: int
    WeightUpdates: int
    BucketSkips: int
}
// ============================================================================
// Liveness Analysis
// ============================================================================

/// Get virtual register IDs used (read) by an instruction
let getUsedVRegs (instr: LIRSymbolic.Instr) : Set<int> =
    let regToVReg (reg: LIRSymbolic.Reg) : int option =
        match reg with
        | LIR.Virtual id -> Some id
        | LIR.Physical _ -> None

    let operandToVReg (op: LIRSymbolic.Operand) : int option =
        match op with
        | LIRSymbolic.Reg reg -> regToVReg reg
        | _ -> None

    match instr with
    | LIRSymbolic.Mov (_, src) ->
        operandToVReg src |> Option.toList |> Set.ofList
    | LIRSymbolic.Store (_, src) ->
        regToVReg src |> Option.toList |> Set.ofList
    | LIRSymbolic.Add (_, left, right) | LIRSymbolic.Sub (_, left, right) ->
        let l = regToVReg left |> Option.toList
        let r = operandToVReg right |> Option.toList
        Set.ofList (l @ r)
    | LIRSymbolic.Mul (_, left, right) | LIRSymbolic.Sdiv (_, left, right)
    | LIRSymbolic.And (_, left, right) | LIRSymbolic.Orr (_, left, right) | LIRSymbolic.Eor (_, left, right)
    | LIRSymbolic.Lsl (_, left, right) | LIRSymbolic.Lsr (_, left, right) ->
        let l = regToVReg left |> Option.toList
        let r = regToVReg right |> Option.toList
        Set.ofList (l @ r)
    | LIRSymbolic.Lsl_imm (_, src, _) | LIRSymbolic.Lsr_imm (_, src, _) | LIRSymbolic.And_imm (_, src, _) ->
        regToVReg src |> Option.toList |> Set.ofList
    | LIRSymbolic.Msub (_, mulLeft, mulRight, sub) ->
        let ml = regToVReg mulLeft |> Option.toList
        let mr = regToVReg mulRight |> Option.toList
        let s = regToVReg sub |> Option.toList
        Set.ofList (ml @ mr @ s)
    | LIRSymbolic.Madd (_, mulLeft, mulRight, add) ->
        let ml = regToVReg mulLeft |> Option.toList
        let mr = regToVReg mulRight |> Option.toList
        let a = regToVReg add |> Option.toList
        Set.ofList (ml @ mr @ a)
    | LIRSymbolic.Cmp (left, right) ->
        let l = regToVReg left |> Option.toList
        let r = operandToVReg right |> Option.toList
        Set.ofList (l @ r)
    | LIRSymbolic.Cset (_, _) -> Set.empty
    | LIRSymbolic.Mvn (_, src) ->
        regToVReg src |> Option.toList |> Set.ofList
    | LIRSymbolic.Sxtb (_, src) | LIRSymbolic.Sxth (_, src) | LIRSymbolic.Sxtw (_, src)
    | LIRSymbolic.Uxtb (_, src) | LIRSymbolic.Uxth (_, src) | LIRSymbolic.Uxtw (_, src) ->
        regToVReg src |> Option.toList |> Set.ofList
    | LIRSymbolic.Call (_, _, args) ->
        args |> List.choose operandToVReg |> Set.ofList
    | LIRSymbolic.TailCall (_, args) ->
        args |> List.choose operandToVReg |> Set.ofList
    | LIRSymbolic.IndirectCall (_, func, args) ->
        let funcVReg = regToVReg func |> Option.toList
        let argsVRegs = args |> List.choose operandToVReg
        Set.ofList (funcVReg @ argsVRegs)
    | LIRSymbolic.IndirectTailCall (func, args) ->
        let funcVReg = regToVReg func |> Option.toList
        let argsVRegs = args |> List.choose operandToVReg
        Set.ofList (funcVReg @ argsVRegs)
    | LIRSymbolic.ClosureAlloc (_, _, captures) ->
        captures |> List.choose operandToVReg |> Set.ofList
    | LIRSymbolic.ClosureCall (_, closure, args) ->
        let closureVReg = regToVReg closure |> Option.toList
        let argsVRegs = args |> List.choose operandToVReg
        Set.ofList (closureVReg @ argsVRegs)
    | LIRSymbolic.ClosureTailCall (closure, args) ->
        let closureVReg = regToVReg closure |> Option.toList
        let argsVRegs = args |> List.choose operandToVReg
        Set.ofList (closureVReg @ argsVRegs)
    | LIRSymbolic.PrintInt64 reg | LIRSymbolic.PrintBool reg
    | LIRSymbolic.PrintInt64NoNewline reg | LIRSymbolic.PrintBoolNoNewline reg
    | LIRSymbolic.PrintHeapStringNoNewline reg | LIRSymbolic.PrintList (reg, _)
    | LIRSymbolic.PrintSum (reg, _) | LIRSymbolic.PrintRecord (reg, _, _) ->
        regToVReg reg |> Option.toList |> Set.ofList
    | LIRSymbolic.PrintFloatNoNewline _ -> Set.empty  // FP register, not GP
    | LIRSymbolic.PrintChars _ -> Set.empty  // No registers used
    | LIRSymbolic.PrintBytes reg -> regToVReg reg |> Option.toList |> Set.ofList
    | LIRSymbolic.HeapAlloc (_, _) -> Set.empty
    | LIRSymbolic.HeapStore (addr, _, src, valueType) ->
        let a = regToVReg addr |> Option.toList
        // For float values, the src register is an FVirtual (handled by float allocation)
        // so we don't include it in integer liveness
        let s =
            match valueType with
            | Some AST.TFloat64 -> []
            | _ -> operandToVReg src |> Option.toList
        Set.ofList (a @ s)
    | LIRSymbolic.HeapLoad (_, addr, _) ->
        regToVReg addr |> Option.toList |> Set.ofList
    | LIRSymbolic.RefCountInc (addr, _) ->
        regToVReg addr |> Option.toList |> Set.ofList
    | LIRSymbolic.RefCountDec (addr, _) ->
        regToVReg addr |> Option.toList |> Set.ofList
    | LIRSymbolic.StringConcat (_, left, right) ->
        let l = operandToVReg left |> Option.toList
        let r = operandToVReg right |> Option.toList
        Set.ofList (l @ r)
    | LIRSymbolic.PrintHeapString reg ->
        regToVReg reg |> Option.toList |> Set.ofList
    | LIRSymbolic.FileReadText (_, path) ->
        operandToVReg path |> Option.toList |> Set.ofList
    | LIRSymbolic.FileExists (_, path) ->
        operandToVReg path |> Option.toList |> Set.ofList
    | LIRSymbolic.FileWriteText (_, path, content) ->
        let p = operandToVReg path |> Option.toList
        let c = operandToVReg content |> Option.toList
        Set.ofList (p @ c)
    | LIRSymbolic.FileAppendText (_, path, content) ->
        let p = operandToVReg path |> Option.toList
        let c = operandToVReg content |> Option.toList
        Set.ofList (p @ c)
    | LIRSymbolic.FileDelete (_, path) ->
        operandToVReg path |> Option.toList |> Set.ofList
    | LIRSymbolic.FileSetExecutable (_, path) ->
        operandToVReg path |> Option.toList |> Set.ofList
    | LIRSymbolic.FileWriteFromPtr (_, path, ptr, length) ->
        let p = operandToVReg path |> Option.toList
        let ptr' = regToVReg ptr |> Option.toList
        let len = regToVReg length |> Option.toList
        Set.ofList (p @ ptr' @ len)
    | LIRSymbolic.RawAlloc (_, numBytes) ->
        regToVReg numBytes |> Option.toList |> Set.ofList
    | LIRSymbolic.RawFree ptr ->
        regToVReg ptr |> Option.toList |> Set.ofList
    | LIRSymbolic.RawGet (_, ptr, byteOffset) ->
        let p = regToVReg ptr |> Option.toList
        let o = regToVReg byteOffset |> Option.toList
        Set.ofList (p @ o)
    | LIRSymbolic.RawGetByte (_, ptr, byteOffset) ->
        let p = regToVReg ptr |> Option.toList
        let o = regToVReg byteOffset |> Option.toList
        Set.ofList (p @ o)
    | LIRSymbolic.RawSet (ptr, byteOffset, value) ->
        let p = regToVReg ptr |> Option.toList
        let o = regToVReg byteOffset |> Option.toList
        let v = regToVReg value |> Option.toList
        Set.ofList (p @ o @ v)
    | LIRSymbolic.RawSetByte (ptr, byteOffset, value) ->
        let p = regToVReg ptr |> Option.toList
        let o = regToVReg byteOffset |> Option.toList
        let v = regToVReg value |> Option.toList
        Set.ofList (p @ o @ v)
    // Int64ToFloat uses an integer source register
    | LIRSymbolic.Int64ToFloat (_, src) ->
        regToVReg src |> Option.toList |> Set.ofList
    | LIRSymbolic.RefCountIncString str ->
        operandToVReg str |> Option.toList |> Set.ofList
    | LIRSymbolic.RefCountDecString str ->
        operandToVReg str |> Option.toList |> Set.ofList
    | LIRSymbolic.RandomInt64 _ ->
        Set.empty  // No operands to read
    | LIRSymbolic.DateNow _ ->
        Set.empty  // No operands to read
    | LIRSymbolic.FloatToString _ ->
        Set.empty  // Float value is in FP register, tracked by getUsedFVRegs
    // ArgMoves/TailArgMoves contain operands that use virtual registers
    | LIRSymbolic.ArgMoves moves ->
        moves |> List.choose (fun (_, op) -> operandToVReg op) |> Set.ofList
    | LIRSymbolic.TailArgMoves moves ->
        moves |> List.choose (fun (_, op) -> operandToVReg op) |> Set.ofList
    // Phi sources are NOT regular uses - they are used at predecessor exits, not at the phi's block
    // The liveness analysis handles phi sources specially in computeLiveness
    | LIRSymbolic.Phi _ -> Set.empty
    | _ -> Set.empty

/// Get virtual register ID defined (written) by an instruction
let getDefinedVReg (instr: LIRSymbolic.Instr) : int option =
    let regToVReg (reg: LIRSymbolic.Reg) : int option =
        match reg with
        | LIR.Virtual id -> Some id
        | LIR.Physical _ -> None

    match instr with
    | LIRSymbolic.Mov (dest, _) -> regToVReg dest
    | LIRSymbolic.Add (dest, _, _) | LIRSymbolic.Sub (dest, _, _) -> regToVReg dest
    | LIRSymbolic.Mul (dest, _, _) | LIRSymbolic.Sdiv (dest, _, _) | LIRSymbolic.Msub (dest, _, _, _) | LIRSymbolic.Madd (dest, _, _, _) -> regToVReg dest
    | LIRSymbolic.Cset (dest, _) -> regToVReg dest
    | LIRSymbolic.And (dest, _, _) | LIRSymbolic.And_imm (dest, _, _) | LIRSymbolic.Orr (dest, _, _) | LIRSymbolic.Eor (dest, _, _)
    | LIRSymbolic.Lsl (dest, _, _) | LIRSymbolic.Lsr (dest, _, _) | LIRSymbolic.Lsl_imm (dest, _, _) | LIRSymbolic.Lsr_imm (dest, _, _) -> regToVReg dest
    | LIRSymbolic.Mvn (dest, _) -> regToVReg dest
    | LIRSymbolic.Sxtb (dest, _) | LIRSymbolic.Sxth (dest, _) | LIRSymbolic.Sxtw (dest, _)
    | LIRSymbolic.Uxtb (dest, _) | LIRSymbolic.Uxth (dest, _) | LIRSymbolic.Uxtw (dest, _) -> regToVReg dest
    | LIRSymbolic.Call (dest, _, _) -> regToVReg dest
    | LIRSymbolic.TailCall _ -> None  // Tail calls don't return to caller
    | LIRSymbolic.IndirectCall (dest, _, _) -> regToVReg dest
    | LIRSymbolic.IndirectTailCall _ -> None  // Indirect tail calls don't return to caller
    | LIRSymbolic.ClosureAlloc (dest, _, _) -> regToVReg dest
    | LIRSymbolic.ClosureCall (dest, _, _) -> regToVReg dest
    | LIRSymbolic.ClosureTailCall _ -> None  // Closure tail calls don't return to caller
    | LIRSymbolic.HeapAlloc (dest, _) -> regToVReg dest
    | LIRSymbolic.HeapLoad (dest, _, _) -> regToVReg dest
    | LIRSymbolic.StringConcat (dest, _, _) -> regToVReg dest
    | LIRSymbolic.LoadFuncAddr (dest, _) -> regToVReg dest
    | LIRSymbolic.FileReadText (dest, _) -> regToVReg dest
    | LIRSymbolic.FileExists (dest, _) -> regToVReg dest
    | LIRSymbolic.FileWriteText (dest, _, _) -> regToVReg dest
    | LIRSymbolic.FileAppendText (dest, _, _) -> regToVReg dest
    | LIRSymbolic.FileDelete (dest, _) -> regToVReg dest
    | LIRSymbolic.FileSetExecutable (dest, _) -> regToVReg dest
    | LIRSymbolic.FileWriteFromPtr (dest, _, _, _) -> regToVReg dest
    | LIRSymbolic.RawAlloc (dest, _) -> regToVReg dest
    | LIRSymbolic.RawGet (dest, _, _) -> regToVReg dest
    | LIRSymbolic.RawGetByte (dest, _, _) -> regToVReg dest
    | LIRSymbolic.RawFree _ -> None
    | LIRSymbolic.RawSet _ -> None
    | LIRSymbolic.RawSetByte _ -> None
    // FloatToInt64 defines an integer destination register
    | LIRSymbolic.FloatToInt64 (dest, _) -> regToVReg dest
    // FloatToBits defines an integer destination register
    | LIRSymbolic.FloatToBits (dest, _) -> regToVReg dest
    // FpToGp defines an integer destination register
    | LIRSymbolic.FpToGp (dest, _) -> regToVReg dest
    | LIRSymbolic.RefCountIncString _ -> None
    | LIRSymbolic.RefCountDecString _ -> None
    | LIRSymbolic.RandomInt64 dest -> regToVReg dest
    | LIRSymbolic.DateNow dest -> regToVReg dest
    | LIRSymbolic.FloatToString (dest, _) -> regToVReg dest
    // Phi defines its destination at block entry
    | LIRSymbolic.Phi (dest, _, _) -> regToVReg dest
    | _ -> None

// ============================================================================
// Float Register Liveness Analysis
// ============================================================================

/// Get FVirtual register IDs used (read) by an instruction
let getUsedFVRegs (instr: LIRSymbolic.Instr) : Set<int> =
    let fregToId (freg: LIR.FReg) : int option =
        match freg with
        | LIR.FVirtual id -> Some id
        | LIR.FPhysical _ -> None

    match instr with
    | LIRSymbolic.FMov (_, src) -> fregToId src |> Option.toList |> Set.ofList
    | LIRSymbolic.FAdd (_, left, right) | LIRSymbolic.FSub (_, left, right)
    | LIRSymbolic.FMul (_, left, right) | LIRSymbolic.FDiv (_, left, right) ->
        [fregToId left; fregToId right] |> List.choose id |> Set.ofList
    | LIRSymbolic.FNeg (_, src) | LIRSymbolic.FAbs (_, src) | LIRSymbolic.FSqrt (_, src) ->
        fregToId src |> Option.toList |> Set.ofList
    | LIRSymbolic.FCmp (left, right) ->
        [fregToId left; fregToId right] |> List.choose id |> Set.ofList
    | LIRSymbolic.FloatToInt64 (_, src) -> fregToId src |> Option.toList |> Set.ofList
    | LIRSymbolic.FloatToBits (_, src) -> fregToId src |> Option.toList |> Set.ofList
    | LIRSymbolic.FpToGp (_, src) -> fregToId src |> Option.toList |> Set.ofList
    | LIRSymbolic.PrintFloat freg | LIRSymbolic.PrintFloatNoNewline freg ->
        fregToId freg |> Option.toList |> Set.ofList
    | LIRSymbolic.FArgMoves moves ->
        moves |> List.choose (fun (_, src) -> fregToId src) |> Set.ofList
    | LIRSymbolic.FPhi _ -> Set.empty  // Phi sources handled specially
    | LIRSymbolic.FloatToString (_, value) -> fregToId value |> Option.toList |> Set.ofList
    // HeapStore with float value: the Virtual register ID is shared with FVirtual
    | LIRSymbolic.HeapStore (_, _, LIRSymbolic.Reg (LIR.Virtual vregId), Some AST.TFloat64) -> Set.singleton vregId
    | _ -> Set.empty

/// Get FVirtual register ID defined (written) by an instruction
let getDefinedFVReg (instr: LIRSymbolic.Instr) : int option =
    let fregToId (freg: LIR.FReg) : int option =
        match freg with
        | LIR.FVirtual id -> Some id
        | LIR.FPhysical _ -> None

    match instr with
    | LIRSymbolic.FMov (dest, _) -> fregToId dest
    | LIRSymbolic.FAdd (dest, _, _) | LIRSymbolic.FSub (dest, _, _)
    | LIRSymbolic.FMul (dest, _, _) | LIRSymbolic.FDiv (dest, _, _) -> fregToId dest
    | LIRSymbolic.FNeg (dest, _) | LIRSymbolic.FAbs (dest, _) | LIRSymbolic.FSqrt (dest, _) -> fregToId dest
    | LIRSymbolic.FLoad (dest, _) -> fregToId dest
    | LIRSymbolic.Int64ToFloat (dest, _) -> fregToId dest
    | LIRSymbolic.GpToFp (dest, _) -> fregToId dest
    | LIRSymbolic.FPhi (dest, _) -> fregToId dest
    | _ -> None

/// Get virtual register used by terminator
let getTerminatorUsedVRegs (term: LIRSymbolic.Terminator) : Set<int> =
    match term with
    | LIRSymbolic.Branch (LIR.Virtual id, _, _) -> Set.singleton id
    | LIRSymbolic.BranchZero (LIR.Virtual id, _, _) -> Set.singleton id
    | LIRSymbolic.BranchBitZero (LIR.Virtual id, _, _, _) -> Set.singleton id
    | LIRSymbolic.BranchBitNonZero (LIR.Virtual id, _, _, _) -> Set.singleton id
    | LIRSymbolic.CondBranch _ -> Set.empty  // CondBranch uses condition flags, not a register
    | _ -> Set.empty

/// Get successor labels for a terminator
let getSuccessors (term: LIRSymbolic.Terminator) : LIR.Label list =
    match term with
    | LIRSymbolic.Ret -> []
    | LIRSymbolic.Branch (_, trueLabel, falseLabel) -> [trueLabel; falseLabel]
    | LIRSymbolic.BranchZero (_, zeroLabel, nonZeroLabel) -> [zeroLabel; nonZeroLabel]
    | LIRSymbolic.BranchBitZero (_, _, zeroLabel, nonZeroLabel) -> [zeroLabel; nonZeroLabel]
    | LIRSymbolic.BranchBitNonZero (_, _, nonZeroLabel, zeroLabel) -> [nonZeroLabel; zeroLabel]
    | LIRSymbolic.CondBranch (_, trueLabel, falseLabel) -> [trueLabel; falseLabel]
    | LIRSymbolic.Jump label -> [label]

/// Get phi uses grouped by predecessor label
/// Returns a map from predecessor label to the set of VRegIds used from that predecessor
let getPhiUsesByPredecessor (block: LIRSymbolic.BasicBlock) : Map<LIR.Label, Set<int>> =
    let operandToVReg (op: LIRSymbolic.Operand) : int option =
        match op with
        | LIRSymbolic.Reg (LIR.Virtual id) -> Some id
        | _ -> None

    block.Instrs
    |> List.choose (fun instr ->
        match instr with
        | LIRSymbolic.Phi (_, sources, _) ->
            Some (sources |> List.choose (fun (op, predLabel) ->
                operandToVReg op |> Option.map (fun vregId -> (predLabel, vregId))))
        | _ -> None)
    |> List.concat
    |> List.groupBy fst
    |> List.map (fun (label, pairs) -> (label, pairs |> List.map snd |> Set.ofList))
    |> Map.ofList

/// Get FPhi uses grouped by predecessor label
/// Returns a map from predecessor label to the set of FVRegIds used from that predecessor
let getFPhiUsesByPredecessor (block: LIRSymbolic.BasicBlock) : Map<LIR.Label, Set<int>> =
    let fregToId (freg: LIR.FReg) : int option =
        match freg with
        | LIR.FVirtual id -> Some id
        | LIR.FPhysical _ -> None

    block.Instrs
    |> List.choose (fun instr ->
        match instr with
        | LIRSymbolic.FPhi (_, sources) ->
            Some (sources |> List.choose (fun (freg, predLabel) ->
                fregToId freg |> Option.map (fun id -> (predLabel, id))))
        | _ -> None)
    |> List.concat
    |> List.groupBy fst
    |> List.map (fun (label, pairs) -> (label, pairs |> List.map snd |> Set.ofList))
    |> Map.ofList

/// Compute GEN and KILL sets for a basic block
/// GEN = variables used before being defined
/// KILL = variables defined
let computeGenKill (block: LIRSymbolic.BasicBlock) : Set<int> * Set<int> =
    // Process instructions in forward order
    let mutable gen = Set.empty
    let mutable kill = Set.empty

    for instr in block.Instrs do
        let used = getUsedVRegs instr
        let defined = getDefinedVReg instr

        // Add to GEN if used and not already killed (defined earlier in block)
        for u in used do
            if not (Set.contains u kill) then
                gen <- Set.add u gen

        // Add to KILL if defined
        match defined with
        | Some d -> kill <- Set.add d kill
        | None -> ()

    // Also add terminator uses to GEN
    let termUses = getTerminatorUsedVRegs block.Terminator
    for u in termUses do
        if not (Set.contains u kill) then
            gen <- Set.add u gen

    (gen, kill)

/// Compute liveness using backward dataflow analysis
/// Handles SSA phi nodes: phi sources are live at predecessor exits, not at phi's block entry
let computeLiveness (cfg: LIRSymbolic.CFG) : Map<LIR.Label, BlockLiveness> =
    // Initialize with empty sets
    let mutable liveness : Map<LIR.Label, BlockLiveness> =
        cfg.Blocks
        |> Map.map (fun _ _ -> { LiveIn = Set.empty; LiveOut = Set.empty })

    // Precompute GEN and KILL for each block
    let genKill =
        cfg.Blocks
        |> Map.map (fun _ block -> computeGenKill block)

    // Precompute phi uses by predecessor for each block
    // This maps: successor_label -> (predecessor_label -> vregs_used_from_that_predecessor)
    let phiUsesByBlock =
        cfg.Blocks
        |> Map.map (fun _ block -> getPhiUsesByPredecessor block)

    // Iterate until fixed point
    let mutable changed = true
    while changed do
        changed <- false
        for kvp in cfg.Blocks do
            let label = kvp.Key
            let block = kvp.Value
            let (gen, kill) = Map.find label genKill
            let oldLiveness = Map.find label liveness

            // LiveOut = union of LiveIn of all successors
            //         + phi uses from successors (for the current block as predecessor)
            let successors = getSuccessors block.Terminator
            let newLiveOut =
                successors
                |> List.fold (fun acc succLabel ->
                    // Add LiveIn of successor
                    let liveInContrib =
                        match Map.tryFind succLabel liveness with
                        | Some succ -> succ.LiveIn
                        | None -> Set.empty
                    // Add phi uses from successor that reference this block as predecessor
                    let phiContrib =
                        match Map.tryFind succLabel phiUsesByBlock with
                        | Some phiUses ->
                            match Map.tryFind label phiUses with
                            | Some vregs -> vregs
                            | None -> Set.empty
                        | None -> Set.empty
                    Set.union acc (Set.union liveInContrib phiContrib)
                ) Set.empty

            // LiveIn = GEN ∪ (LiveOut - KILL)
            let newLiveIn = Set.union gen (Set.difference newLiveOut kill)

            if newLiveIn <> oldLiveness.LiveIn || newLiveOut <> oldLiveness.LiveOut then
                changed <- true
                liveness <- Map.add label { LiveIn = newLiveIn; LiveOut = newLiveOut } liveness

    liveness

/// Compute float GEN and KILL sets for a basic block
/// GEN = float variables used before being defined
/// KILL = float variables defined
let computeFloatGenKill (block: LIRSymbolic.BasicBlock) : Set<int> * Set<int> =
    let mutable gen = Set.empty
    let mutable kill = Set.empty

    for instr in block.Instrs do
        let used = getUsedFVRegs instr
        let defined = getDefinedFVReg instr

        for u in used do
            if not (Set.contains u kill) then
                gen <- Set.add u gen

        match defined with
        | Some d -> kill <- Set.add d kill
        | None -> ()

    (gen, kill)

/// Compute float liveness using backward dataflow analysis
/// Handles SSA FPhi nodes: phi sources are live at predecessor exits, not at phi's block entry
let computeFloatLiveness (cfg: LIRSymbolic.CFG) : Map<LIR.Label, BlockLiveness> =
    let mutable liveness : Map<LIR.Label, BlockLiveness> =
        cfg.Blocks |> Map.map (fun _ _ -> { LiveIn = Set.empty; LiveOut = Set.empty })

    let genKill = cfg.Blocks |> Map.map (fun _ block -> computeFloatGenKill block)
    let fphiUsesByBlock = cfg.Blocks |> Map.map (fun _ block -> getFPhiUsesByPredecessor block)

    let mutable changed = true
    while changed do
        changed <- false
        for kvp in cfg.Blocks do
            let label = kvp.Key
            let block = kvp.Value
            let (gen, kill) = Map.find label genKill
            let oldLiveness = Map.find label liveness

            let successors = getSuccessors block.Terminator
            let newLiveOut =
                successors
                |> List.fold (fun acc succLabel ->
                    let liveInContrib =
                        match Map.tryFind succLabel liveness with
                        | Some succ -> succ.LiveIn
                        | None -> Set.empty
                    let fphiContrib =
                        match Map.tryFind succLabel fphiUsesByBlock with
                        | Some fphiUses ->
                            match Map.tryFind label fphiUses with
                            | Some vregs -> vregs
                            | None -> Set.empty
                        | None -> Set.empty
                    Set.union acc (Set.union liveInContrib fphiContrib)
                ) Set.empty

            let newLiveIn = Set.union gen (Set.difference newLiveOut kill)

            if newLiveIn <> oldLiveness.LiveIn || newLiveOut <> oldLiveness.LiveOut then
                changed <- true
                liveness <- Map.add label { LiveIn = newLiveIn; LiveOut = newLiveOut } liveness

    liveness

/// Compute liveness at each instruction index within a block
/// Returns a list of live VReg sets, one per instruction (same order as Instrs)
let computeInstructionLiveness (block: LIRSymbolic.BasicBlock) (liveOut: Set<int>) : Set<int> list =
    // Walk backwards from the terminator, tracking liveness
    let mutable live = liveOut

    // Handle terminator uses first
    let termUses = getTerminatorUsedVRegs block.Terminator
    for u in termUses do
        live <- Set.add u live

    // Walk instructions in reverse, collecting liveness at each point
    // We want liveness AFTER each instruction (what's live when that instruction completes)
    let instrsReversed = List.rev block.Instrs
    let mutable livenessListReversed = []

    for instr in instrsReversed do
        // Record liveness at this point (after the instruction executes)
        livenessListReversed <- live :: livenessListReversed

        // Update liveness: remove definition, add uses
        match getDefinedVReg instr with
        | Some def -> live <- Set.remove def live
        | None -> ()

        for used in getUsedVRegs instr do
            live <- Set.add used live

    // Since we walked backwards and prepended each result, the list is already
    // in forward order (matching the original instruction order)
    livenessListReversed

/// Compute float liveness at each instruction index within a block
/// Returns a list of live FVirtual ID sets, one per instruction (same order as Instrs)
let computeFloatInstructionLiveness (block: LIRSymbolic.BasicBlock) (liveOut: Set<int>) : Set<int> list =
    let mutable live = liveOut

    let instrsReversed = List.rev block.Instrs
    let mutable livenessListReversed = []

    for instr in instrsReversed do
        livenessListReversed <- live :: livenessListReversed

        match getDefinedFVReg instr with
        | Some def -> live <- Set.remove def live
        | None -> ()

        for used in getUsedFVRegs instr do
            live <- Set.add used live

    livenessListReversed

// ============================================================================
// Register Definitions
// ============================================================================

/// Caller-saved registers (X1-X7) - preferred for allocation
/// Note: X8 is excluded because StringConcat uses it as a scratch register for byte copying
/// Note: X9-X10 are excluded because they are used as compiler scratch registers
let callerSavedRegs = [
    LIR.X1; LIR.X2; LIR.X3; LIR.X4; LIR.X5
    LIR.X6; LIR.X7
]

/// Callee-saved registers (X19-X26) - used when caller-saved exhausted
/// These must be saved/restored in function prologue/epilogue
/// Note: X27 and X28 are reserved for free list base and heap pointer respectively
let calleeSavedRegs = [
    LIR.X19; LIR.X20; LIR.X21; LIR.X22; LIR.X23
    LIR.X24; LIR.X25; LIR.X26
]

/// Check if an instruction is a non-tail call (requires SaveRegs/RestoreRegs)
let isNonTailCall (instr: LIRSymbolic.Instr) : bool =
    match instr with
    | LIRSymbolic.Call _ | LIRSymbolic.IndirectCall _ | LIRSymbolic.ClosureCall _ -> true
    | _ -> false

/// Check if a function has any non-tail calls
/// If it does, we prefer callee-saved registers to avoid per-call save/restore overhead
let hasNonTailCalls (cfg: LIRSymbolic.CFG) : bool =
    cfg.Blocks
    |> Map.exists (fun _ block ->
        block.Instrs |> List.exists isNonTailCall)

/// Get the optimal register allocation order based on calling pattern
/// - Functions with non-tail calls: prefer callee-saved (save once in prologue/epilogue)
/// - Leaf functions / tail-call-only: prefer caller-saved (no prologue/epilogue overhead)
let getAllocatableRegs (cfg: LIRSymbolic.CFG) : LIR.PhysReg list =
    if hasNonTailCalls cfg then
        // Callee-saved first for call-heavy functions
        calleeSavedRegs @ callerSavedRegs
    else
        // Caller-saved first for leaf/tail-call-only functions
        callerSavedRegs @ calleeSavedRegs

// ============================================================================
// Chordal Graph Coloring Register Allocation
// ============================================================================

/// Build interference graph from CFG and liveness information
/// Two variables interfere if their live ranges overlap.
/// In SSA form, it is sufficient to add edges from each definition to live-out variables.
let buildInterferenceGraph
    (cfg: LIRSymbolic.CFG)
    (liveness: Map<LIR.Label, BlockLiveness>)
    (entryDefs: Set<int>)
    : InterferenceGraph =
    let mutable vertices = Set.empty<int>
    let mutable edges = Map.empty<int, Set<int>>

    /// Add a vertex to the graph
    let addVertex (v: int) =
        vertices <- Set.add v vertices
        if not (Map.containsKey v edges) then
            edges <- Map.add v Set.empty edges

    /// Add an edge between two vertices (symmetric)
    let addEdge (u: int) (v: int) =
        if u <> v then
            addVertex u
            addVertex v
            edges <- Map.add u (Set.add v (Map.find u edges)) edges
            edges <- Map.add v (Set.add u (Map.find v edges)) edges

    let addEdgesToLive (d: int) (live: Set<int>) =
        for v in live do
            if v <> d then
                addEdge d v

    // For each block, walk backward through instructions
    // Add edges from each definition to the current live set
    for kvp in cfg.Blocks do
        let block = kvp.Value
        let label = kvp.Key

        match Map.tryFind label liveness with
        | None -> ()
        | Some blockLiveness ->
            let mutable live = blockLiveness.LiveOut

            // Process terminator
            let termUses = getTerminatorUsedVRegs block.Terminator
            for v in termUses do
                live <- Set.add v live

            // Track all variables that are live at the block end
            for v in live do
                addVertex v

            // Walk instructions backward
            for instr in List.rev block.Instrs do
                // Get definition and uses
                let def = getDefinedVReg instr
                let uses = getUsedVRegs instr

                for u in uses do
                    addVertex u

                // Definition kills liveness
                match def with
                | Some d ->
                    addVertex d
                    addEdgesToLive d live
                    live <- Set.remove d live
                | None -> ()

                // Uses make variables live
                for u in uses do
                    live <- Set.add u live

            // Parameters are defined at function entry (not by an instruction).
            // Add interference edges from live entry parameters to other live-in values.
            if label = cfg.Entry then
                let entryLiveDefs = Set.intersect entryDefs live
                for d in entryLiveDefs do
                    addVertex d
                    addEdgesToLive d live

    { Vertices = vertices; Edges = edges }

/// Build float interference graph from CFG and float liveness information
/// Two float variables interfere if they are both live at any program point
let buildFloatInterferenceGraph
    (cfg: LIRSymbolic.CFG)
    (liveness: Map<LIR.Label, BlockLiveness>)
    (entryDefs: Set<int>)
    : InterferenceGraph =
    let mutable vertices = Set.empty<int>
    let mutable edges = Map.empty<int, Set<int>>

    let addVertex (v: int) =
        vertices <- Set.add v vertices
        if not (Map.containsKey v edges) then
            edges <- Map.add v Set.empty edges

    let addEdge (u: int) (v: int) =
        if u <> v then
            addVertex u
            addVertex v
            edges <- Map.add u (Set.add v (Map.find u edges)) edges
            edges <- Map.add v (Set.add u (Map.find v edges)) edges

    let addEdgesToLive (d: int) (live: Set<int>) =
        for v in live do
            if v <> d then addEdge d v

    for kvp in cfg.Blocks do
        let block = kvp.Value
        let label = kvp.Key

        match Map.tryFind label liveness with
        | None -> ()
        | Some blockLiveness ->
            let mutable live = blockLiveness.LiveOut

            for v in live do addVertex v

            for instr in List.rev block.Instrs do
                let def = getDefinedFVReg instr
                let uses = getUsedFVRegs instr

                for u in uses do
                    addVertex u

                match def with
                | Some d ->
                    addVertex d
                    addEdgesToLive d live
                    live <- Set.remove d live
                | None -> ()

                for u in uses do
                    live <- Set.add u live

            if label = cfg.Entry then
                let entryLiveDefs = Set.intersect entryDefs live
                for d in entryLiveDefs do
                    addVertex d
                    addEdgesToLive d live

    { Vertices = vertices; Edges = edges }

/// Collect phi coalescing preferences from CFG.
/// Returns Map<vregId, Set<vregId>> where each vreg maps to vregs it should prefer
/// to share a color with.
/// For each Phi(dest, sources), the dest should prefer the same color as sources.
let collectPhiPreferences (cfg: LIRSymbolic.CFG) : Map<int, Set<int>> =
    let mutable preferences = Map.empty<int, Set<int>>

    let addPreference (v1: int) (v2: int) =
        if v1 <> v2 then
            let existing1 = Map.tryFind v1 preferences |> Option.defaultValue Set.empty
            preferences <- Map.add v1 (Set.add v2 existing1) preferences
            let existing2 = Map.tryFind v2 preferences |> Option.defaultValue Set.empty
            preferences <- Map.add v2 (Set.add v1 existing2) preferences

    for kvp in cfg.Blocks do
        let block = kvp.Value
        for instr in block.Instrs do
            match instr with
            | LIRSymbolic.Phi (LIR.Virtual destId, sources, _) ->
                // Add preferences between dest and each virtual register source
                for (src, _) in sources do
                    match src with
                    | LIRSymbolic.Reg (LIR.Virtual srcId) ->
                        addPreference destId srcId
                    | _ -> ()
            | _ -> ()

    preferences

/// Collect move-related coalescing pairs from CFG.
/// Returns undirected pairs of virtual registers that are directly moved between.
let collectMovePairs (cfg: LIRSymbolic.CFG) : (int * int) list =
    let normalize (a: int) (b: int) =
        if a < b then (a, b) else (b, a)
    cfg.Blocks
    |> Map.fold (fun acc _ block ->
        block.Instrs
        |> List.fold (fun acc instr ->
            match instr with
            | LIRSymbolic.Mov (LIR.Virtual destId, LIRSymbolic.Reg (LIR.Virtual srcId)) ->
                Set.add (normalize destId srcId) acc
            | _ -> acc) acc) Set.empty
    |> Set.toList

/// Collect phi-related coalescing pairs from CFG.
/// Returns undirected pairs of virtual registers that flow into the same phi destination.
let collectPhiPairs (cfg: LIRSymbolic.CFG) : (int * int) list =
    let normalize (a: int) (b: int) =
        if a < b then (a, b) else (b, a)
    cfg.Blocks
    |> Map.fold (fun acc _ block ->
        block.Instrs
        |> List.fold (fun acc instr ->
            match instr with
            | LIRSymbolic.Phi (LIR.Virtual destId, sources, _) ->
                sources
                |> List.fold (fun acc (src, _) ->
                    match src with
                    | LIRSymbolic.Reg (LIR.Virtual srcId) when srcId <> destId ->
                        Set.add (normalize destId srcId) acc
                    | _ -> acc) acc
            | _ -> acc) acc) Set.empty
    |> Set.toList

/// Maximum Cardinality Search - computes Perfect Elimination Ordering for chordal graphs
/// Returns vertices in PEO order (first vertex is most "central")
/// Uses a bucket queue for linear-time selection in terms of vertices + edges.
let maximumCardinalitySearchWithProfile (graph: InterferenceGraph) : int list * McsProfile =
    if Set.isEmpty graph.Vertices then
        ([], { VertexCount = 0; SelectionChecks = 0; WeightUpdates = 0; BucketSkips = 0 })
    else
        let vertexList = Set.toArray graph.Vertices
        let n = vertexList.Length
        let vertexToIdx = vertexList |> Array.mapi (fun i v -> (v, i)) |> Map.ofArray

        let neighbors =
            vertexList
            |> Array.map (fun v ->
                match Map.tryFind v graph.Edges with
                | None -> [||]
                | Some adj ->
                    adj
                    |> Seq.map (fun u ->
                        match Map.tryFind u vertexToIdx with
                        | Some idx -> idx
                        | None -> Crash.crash $"Interference graph missing vertex {u}")
                    |> Seq.toArray)

        // Track weights and ordered status
        let weights = Array.zeroCreate<int> n
        let ordered = Array.create n false

        // Bucket queue state (weight -> list of vertices)
        let bucketHeads = Array.create n -1
        let next = Array.create n -1
        let prev = Array.create n -1

        // Initialize all vertices in bucket 0
        for i in 0 .. n - 1 do
            let head = bucketHeads.[0]
            next.[i] <- head
            prev.[i] <- -1
            if head <> -1 then prev.[head] <- i
            bucketHeads.[0] <- i

        let removeFromBucket (idx: int) (weight: int) : unit =
            let p = prev.[idx]
            let nidx = next.[idx]
            if p <> -1 then
                next.[p] <- nidx
            else
                bucketHeads.[weight] <- nidx
            if nidx <> -1 then
                prev.[nidx] <- p
            next.[idx] <- -1
            prev.[idx] <- -1

        let addToBucket (idx: int) (weight: int) : unit =
            let head = bucketHeads.[weight]
            next.[idx] <- head
            prev.[idx] <- -1
            if head <> -1 then prev.[head] <- idx
            bucketHeads.[weight] <- idx

        let mutable currentMax = 0
        let mutable ordering = []
        let mutable selectionChecks = 0
        let mutable weightUpdates = 0
        let mutable bucketSkips = 0

        for _ in 0 .. n - 1 do
            while currentMax >= 0 && bucketHeads.[currentMax] = -1 do
                currentMax <- currentMax - 1
                bucketSkips <- bucketSkips + 1
            if currentMax < 0 then
                Crash.crash "MCS bucket queue empty before selecting all vertices"

            let idx = bucketHeads.[currentMax]
            selectionChecks <- selectionChecks + 1
            removeFromBucket idx currentMax
            ordered.[idx] <- true
            ordering <- vertexList.[idx] :: ordering

            for nidx in neighbors.[idx] do
                if not ordered.[nidx] then
                    let oldWeight = weights.[nidx]
                    removeFromBucket nidx oldWeight
                    let newWeight = oldWeight + 1
                    if newWeight >= n then
                        Crash.crash $"MCS weight overflow: {newWeight} >= {n}"
                    weights.[nidx] <- newWeight
                    addToBucket nidx newWeight
                    if newWeight > currentMax then currentMax <- newWeight
                    weightUpdates <- weightUpdates + 1

        let profile = {
            VertexCount = n
            SelectionChecks = selectionChecks
            WeightUpdates = weightUpdates
            BucketSkips = bucketSkips
        }
        (List.rev ordering, profile)

let maximumCardinalitySearch (graph: InterferenceGraph) : int list =
    let (ordering, _profile) = maximumCardinalitySearchWithProfile graph
    ordering

type private CoalescedGraph = {
    Graph: InterferenceGraph
    RepMap: Map<int, int>
    RepMembers: Map<int, Set<int>>
    Preferences: Map<int, Set<int>>
    Precolored: Map<int, int>
}

let private coalesceGraph
    (graph: InterferenceGraph)
    (precolored: Map<int, int>)
    (movePairs: (int * int) list)
    (preferences: Map<int, Set<int>>)
    : CoalescedGraph =
    if Set.isEmpty graph.Vertices then
        { Graph = graph
          RepMap = Map.empty
          RepMembers = Map.empty
          Preferences = Map.empty
          Precolored = precolored }
    else
        let vertexList = Set.toArray graph.Vertices
        let n = vertexList.Length
        let vertexToIdx = vertexList |> Array.mapi (fun i v -> (v, i)) |> Map.ofArray

        let parent = Array.init n id
        let sizes = Array.create n 1
        let members = vertexList |> Array.map (fun v -> Set.singleton v)
        let neighbors =
            vertexList
            |> Array.map (fun v -> Map.tryFind v graph.Edges |> Option.defaultValue Set.empty)
        let precolor =
            vertexList
            |> Array.map (fun v -> Map.tryFind v precolored)

        let rec find idx =
            let p = parent.[idx]
            if p = idx then
                idx
            else
                let root = find p
                parent.[idx] <- root
                root

        let canMerge rootA rootB =
            if rootA = rootB then
                false
            else
                match precolor.[rootA], precolor.[rootB] with
                | Some c1, Some c2 when c1 <> c2 -> false
                | _ ->
                    let neighborsA = neighbors.[rootA]
                    let membersB = members.[rootB]
                    if not (Set.isEmpty (Set.intersect neighborsA membersB)) then
                        false
                    else
                        let neighborsB = neighbors.[rootB]
                        let membersA = members.[rootA]
                        Set.isEmpty (Set.intersect neighborsB membersA)

        let union rootA rootB =
            let ra, rb =
                if sizes.[rootA] < sizes.[rootB] then
                    (rootB, rootA)
                else
                    (rootA, rootB)
            parent.[rb] <- ra
            sizes.[ra] <- sizes.[ra] + sizes.[rb]
            members.[ra] <- Set.union members.[ra] members.[rb]
            neighbors.[ra] <- Set.union neighbors.[ra] neighbors.[rb]
            match precolor.[ra], precolor.[rb] with
            | None, Some color -> precolor.[ra] <- Some color
            | _ -> ()
            members.[rb] <- Set.empty
            neighbors.[rb] <- Set.empty
            precolor.[rb] <- None

        for (u, v) in movePairs do
            match Map.tryFind u vertexToIdx, Map.tryFind v vertexToIdx with
            | Some idxU, Some idxV ->
                let rootU = find idxU
                let rootV = find idxV
                if canMerge rootU rootV then
                    union rootU rootV
            | _ -> ()

        let repMembers =
            [0 .. n - 1]
            |> List.fold (fun acc idx ->
                if parent.[idx] = idx then
                    let memberSet = members.[idx]
                    if Set.isEmpty memberSet then acc
                    else
                        let rep = Set.minElement memberSet
                        Map.add rep memberSet acc
                else
                    acc) Map.empty

        let repMap =
            repMembers
            |> Map.fold (fun acc rep memberSet ->
                memberSet |> Set.fold (fun acc v -> Map.add v rep acc) acc) Map.empty

        let repPrecolored =
            precolored
            |> Map.fold (fun acc v color ->
                match Map.tryFind v repMap with
                | Some rep -> Map.add rep color acc
                | None -> acc) Map.empty

        let addPreference (v1: int) (v2: int) (prefs: Map<int, Set<int>>) : Map<int, Set<int>> =
            if v1 = v2 then prefs
            else
                let existing1 = Map.tryFind v1 prefs |> Option.defaultValue Set.empty
                let prefs = Map.add v1 (Set.add v2 existing1) prefs
                let existing2 = Map.tryFind v2 prefs |> Option.defaultValue Set.empty
                Map.add v2 (Set.add v1 existing2) prefs

        let repPreferences =
            preferences
            |> Map.fold (fun acc v partners ->
                match Map.tryFind v repMap with
                | None -> acc
                | Some repV ->
                    partners
                    |> Set.fold (fun acc partner ->
                        match Map.tryFind partner repMap with
                        | Some repP when repP <> repV -> addPreference repV repP acc
                        | _ -> acc) acc) Map.empty

        let repVertices =
            repMembers |> Map.toSeq |> Seq.map fst |> Set.ofSeq

        let addVertex (v: int) (edges: Map<int, Set<int>>) =
            if Map.containsKey v edges then edges else Map.add v Set.empty edges

        let addEdge (u: int) (v: int) (edges: Map<int, Set<int>>) =
            if u = v then edges
            else
                let edges = addVertex u edges
                let edges = addVertex v edges
                let neighborsU =
                    match Map.tryFind u edges with
                    | Some neighbors -> neighbors
                    | None -> Crash.crash $"coalesceMoves: Missing vertex {u}"
                let neighborsV =
                    match Map.tryFind v edges with
                    | Some neighbors -> neighbors
                    | None -> Crash.crash $"coalesceMoves: Missing vertex {v}"
                let edges = Map.add u (Set.add v neighborsU) edges
                Map.add v (Set.add u neighborsV) edges

        let repEdges =
            graph.Edges
            |> Map.fold (fun edges v neighbors ->
                match Map.tryFind v repMap with
                | None -> edges
                | Some repV ->
                    neighbors
                    |> Set.fold (fun edges n ->
                        match Map.tryFind n repMap with
                        | Some repN when repN <> repV -> addEdge repV repN edges
                        | _ -> edges) edges) Map.empty
            |> fun edges ->
                repVertices |> Set.fold (fun edges v -> addVertex v edges) edges

        let repGraph = { Vertices = repVertices; Edges = repEdges }

        { Graph = repGraph
          RepMap = repMap
          RepMembers = repMembers
          Preferences = repPreferences
          Precolored = repPrecolored }

let private expandColoring (result: ColoringResult) (repMembers: Map<int, Set<int>>) : ColoringResult =
    let expandedColors =
        result.Colors
        |> Map.fold (fun acc rep color ->
            match Map.tryFind rep repMembers with
            | Some members ->
                members |> Set.fold (fun acc v -> Map.add v color acc) acc
            | None -> acc) Map.empty

    let expandedSpills =
        result.Spills
        |> Set.fold (fun acc rep ->
            match Map.tryFind rep repMembers with
            | Some members -> Set.union acc members
            | None -> acc) Set.empty

    { Colors = expandedColors
      Spills = expandedSpills
      ChromaticNumber = result.ChromaticNumber }

/// Greedy color in reverse PEO order with phi coalescing preferences
/// For chordal graphs, this produces an optimal coloring.
/// When preferences are provided, try to use colors that match coalesced partners.
/// Uses two-pass approach: first color vregs with no uncolored phi partners,
/// then color deferred vregs (whose partners are now colored).
let greedyColorReverse (graph: InterferenceGraph) (peo: int list) (precolored: Map<int, int>) (numColors: int) (preferences: Map<int, Set<int>>) : ColoringResult =
    let mutable colors = precolored
    let mutable spills = Set.empty<int>
    let mutable maxColor = -1

    // Update maxColor from pre-colored vertices
    for kvp in precolored do
        if kvp.Value > maxColor then
            maxColor <- kvp.Value

    let colorVertex (v: int) : unit =
        if not (Map.containsKey v colors) then
            // Find colors used by already-colored neighbors
            let neighbors = Map.tryFind v graph.Edges |> Option.defaultValue Set.empty
            let usedColors =
                neighbors
                |> Set.toList
                |> List.choose (fun u -> Map.tryFind u colors)
                |> Set.ofList

            // Get preferred colors from coalesced partners that are already colored
            let preferredColors =
                match Map.tryFind v preferences with
                | Some partners ->
                    partners
                    |> Set.toList
                    |> List.choose (fun partner -> Map.tryFind partner colors)
                    |> List.filter (fun c -> not (Set.contains c usedColors))
                | None -> []

            let mutable assigned = false

            // First try preferred colors (in order of first appearance)
            for prefColor in preferredColors do
                if not assigned then
                    colors <- Map.add v prefColor colors
                    if prefColor > maxColor then maxColor <- prefColor
                    assigned <- true

            // If no preferred color worked, find smallest available color
            if not assigned then
                for c in 0 .. numColors - 1 do
                    if not assigned && not (Set.contains c usedColors) then
                        colors <- Map.add v c colors
                        if c > maxColor then maxColor <- c
                        assigned <- true

            // If no color available, mark for spill
            if not assigned then
                spills <- Set.add v spills

    // Check if a vreg has any uncolored phi partners
    let hasUncoloredPartners (v: int) : bool =
        match Map.tryFind v preferences with
        | Some partners ->
            partners |> Set.exists (fun p -> not (Map.containsKey p colors))
        | None -> false

    // Check if two vertices interfere
    let interferes (v1: int) (v2: int) : bool =
        match Map.tryFind v1 graph.Edges with
        | Some neighbors -> Set.contains v2 neighbors
        | None -> false

    // Color a vertex and all its non-interfering, uncolored phi partners together
    // This ensures mutual phi partners get the same color when possible
    let colorVertexWithPartners (v: int) : unit =
        if not (Map.containsKey v colors) then
            // Find non-interfering, uncolored phi partners
            // Must ensure they don't interfere with v AND don't interfere with each other
            let candidates =
                match Map.tryFind v preferences with
                | Some partners ->
                    partners
                    |> Set.filter (fun p ->
                        not (Map.containsKey p colors) &&
                        not (interferes v p))
                    |> Set.toList
                | None -> []

            // Filter to only include partners that don't interfere with each other
            let rec filterMutuallyCompatible (acc: int list) (remaining: int list) =
                match remaining with
                | [] -> List.rev acc
                | p :: rest ->
                    // Check if p interferes with any already-accepted partner
                    let compatible = acc |> List.forall (fun a -> not (interferes p a))
                    if compatible then
                        filterMutuallyCompatible (p :: acc) rest
                    else
                        filterMutuallyCompatible acc rest

            let coalesceable = filterMutuallyCompatible [] candidates

            // Find colors used by neighbors of v and all coalesceable partners
            let allVertices = v :: coalesceable
            let usedColors =
                allVertices
                |> List.collect (fun vertex ->
                    let neighbors = Map.tryFind vertex graph.Edges |> Option.defaultValue Set.empty
                    neighbors
                    |> Set.toList
                    |> List.choose (fun u -> Map.tryFind u colors))
                |> Set.ofList

            // Find smallest available color
            let mutable assigned = false
            for c in 0 .. numColors - 1 do
                if not assigned && not (Set.contains c usedColors) then
                    // Assign this color to v and all coalesceable partners
                    for vertex in allVertices do
                        if not (Map.containsKey vertex colors) then
                            colors <- Map.add vertex c colors
                    if c > maxColor then maxColor <- c
                    assigned <- true

            // If no color available, mark for spill (only the main vertex)
            if not assigned then
                spills <- Set.add v spills

    // First pass: color vregs with no preferences or all partners already colored
    // Defer vregs that have uncolored partners (so partners get colored first)
    let mutable deferred = Set.empty<int>
    for v in List.rev peo do
        if not (Map.containsKey v colors) then
            if hasUncoloredPartners v then
                deferred <- Set.add v deferred
            else
                colorVertex v

    // Second pass: color deferred vregs with aggressive coalescing
    // For mutual phi partners that don't interfere, color them together
    for v in List.rev peo do
        if Set.contains v deferred && not (Map.containsKey v colors) then
            colorVertexWithPartners v

    { Colors = colors
      Spills = spills
      ChromaticNumber = if maxColor < 0 then 0 else maxColor + 1 }

/// Main chordal graph coloring function with phi coalescing preferences
let chordalGraphColor
    (graph: InterferenceGraph)
    (precolored: Map<int, int>)
    (numColors: int)
    (preferences: Map<int, Set<int>>)
    (movePairs: (int * int) list)
    : ColoringResult =
    if Set.isEmpty graph.Vertices then
        { Colors = Map.empty; Spills = Set.empty; ChromaticNumber = 0 }
    else
        let coalesced = coalesceGraph graph precolored movePairs preferences
        let peo = maximumCardinalitySearch coalesced.Graph
        let result = greedyColorReverse coalesced.Graph peo coalesced.Precolored numColors coalesced.Preferences
        expandColoring result coalesced.RepMembers

/// Convert chordal graph coloring result to allocation result
/// Colors map to physical registers, spills map to stack slots
let coloringToAllocation (colorResult: ColoringResult) (registers: LIR.PhysReg list) : AllocationResult =
    let mutable mapping = Map.empty<int, Allocation>
    let mutable nextStackSlot = -8
    let mutable usedCalleeSaved = Set.empty<LIR.PhysReg>

    // Map colored vertices to physical registers
    for kvp in colorResult.Colors do
        let vregId = kvp.Key
        let color = kvp.Value
        if color < List.length registers then
            let reg = List.item color registers
            mapping <- Map.add vregId (PhysReg reg) mapping
            // Track callee-saved register usage
            if List.contains reg [LIR.X19; LIR.X20; LIR.X21; LIR.X22; LIR.X23; LIR.X24; LIR.X25; LIR.X26] then
                usedCalleeSaved <- Set.add reg usedCalleeSaved
        else
            // Color out of range - treat as spill
            mapping <- Map.add vregId (StackSlot nextStackSlot) mapping
            nextStackSlot <- nextStackSlot - 8

    // Map spilled vertices to stack slots
    for vregId in colorResult.Spills do
        mapping <- Map.add vregId (StackSlot nextStackSlot) mapping
        nextStackSlot <- nextStackSlot - 8

    // Compute 16-byte aligned stack size
    let stackSize =
        if nextStackSlot = -8 then 0
        else ((abs nextStackSlot + 15) / 16) * 16

    { Mapping = mapping
      StackSize = stackSize
      UsedCalleeSaved = usedCalleeSaved |> Set.toList |> List.sort }

// ============================================================================
// Float Register Allocation
// ============================================================================

/// Float caller-saved registers (D0-D7)
let floatCallerSavedRegs : LIR.PhysFPReg list = [
    LIR.D0; LIR.D1; LIR.D2; LIR.D3; LIR.D4; LIR.D5; LIR.D6; LIR.D7
]

/// Float callee-saved registers (D8-D15)
let floatCalleeSavedRegs : LIR.PhysFPReg list = [
    LIR.D8; LIR.D9; LIR.D10; LIR.D11; LIR.D12; LIR.D13; LIR.D14; LIR.D15
]

/// All allocatable float registers - caller-saved first, then callee-saved
let allocatableFloatRegs : LIR.PhysFPReg list = floatCallerSavedRegs @ floatCalleeSavedRegs

/// Float allocation result
type FAllocationResult = {
    FMapping: Map<int, LIR.PhysFPReg>
    UsedCalleeSavedF: LIR.PhysFPReg list
}

/// Convert physical FP register to integer for graph coloring
let physFPRegToInt (reg: LIR.PhysFPReg) : int =
    match reg with
    | LIR.D0 -> 0 | LIR.D1 -> 1 | LIR.D2 -> 2 | LIR.D3 -> 3
    | LIR.D4 -> 4 | LIR.D5 -> 5 | LIR.D6 -> 6 | LIR.D7 -> 7
    | LIR.D8 -> 8 | LIR.D9 -> 9 | LIR.D10 -> 10 | LIR.D11 -> 11
    | LIR.D12 -> 12 | LIR.D13 -> 13 | LIR.D14 -> 14 | LIR.D15 -> 15

/// Convert float coloring result to allocation
let floatColoringToAllocation (colorResult: ColoringResult) (registers: LIR.PhysFPReg list) : FAllocationResult =
    let mutable mapping = Map.empty<int, LIR.PhysFPReg>
    let mutable usedCalleeSaved = Set.empty<LIR.PhysFPReg>

    for kvp in colorResult.Colors do
        let fvregId = kvp.Key
        let color = kvp.Value
        if color < List.length registers then
            let reg = List.item color registers
            mapping <- Map.add fvregId reg mapping
            if List.contains reg floatCalleeSavedRegs then
                usedCalleeSaved <- Set.add reg usedCalleeSaved

    { FMapping = mapping; UsedCalleeSavedF = usedCalleeSaved |> Set.toList |> List.sort }

/// Run chordal graph coloring for float register allocation
/// additionalVRegs: FVirtual IDs that must be allocated (e.g., float parameters)
/// even if they don't appear in the CFG instructions
let chordalFloatAllocation (cfg: LIRSymbolic.CFG) (additionalVRegs: Set<int>) : FAllocationResult =
    let liveness = computeFloatLiveness cfg
    let graph = buildFloatInterferenceGraph cfg liveness additionalVRegs
    // Add additional VRegs (like float params) as isolated vertices if not already in graph
    let graphWithParams : InterferenceGraph =
        additionalVRegs
        |> Set.fold (fun (g: InterferenceGraph) vregId ->
            if Set.contains vregId g.Vertices then g
            else { g with Vertices = Set.add vregId g.Vertices
                          Edges = Map.add vregId Set.empty g.Edges }
        ) graph
    if Set.isEmpty graphWithParams.Vertices then
        // No float registers used - return empty allocation
        { FMapping = Map.empty; UsedCalleeSavedF = [] }
    else
        // No preferences for floats for now (could add FPhi coalescing later)
        let colorResult = chordalGraphColor graphWithParams Map.empty (List.length allocatableFloatRegs) Map.empty []
        floatColoringToAllocation colorResult allocatableFloatRegs

/// Apply float allocation to an FReg, converting FVirtual to FPhysical
let applyFloatAllocationToFReg (floatAllocation: FAllocationResult) (freg: LIR.FReg) : LIR.FReg =
    match freg with
    | LIR.FPhysical _ -> freg  // Already physical
    | LIR.FVirtual 1000 -> freg  // Fixed temp - keep as is, CodeGen handles it
    | LIR.FVirtual 1001 -> freg  // Fixed temp
    | LIR.FVirtual 2000 -> freg  // Fixed temp
    | LIR.FVirtual n when n >= 3000 && n < 4000 -> freg  // Call arg temps - fixed
    | LIR.FVirtual id ->
        match Map.tryFind id floatAllocation.FMapping with
        | Some physReg -> LIR.FPhysical physReg
        | None -> Crash.crash $"Float register allocation bug: FVirtual {id} not found in allocation"

/// Apply float allocation to an instruction
let applyFloatAllocationToInstr (floatAllocation: FAllocationResult) (instr: LIRSymbolic.Instr) : LIRSymbolic.Instr =
    let applyF = applyFloatAllocationToFReg floatAllocation
    match instr with
    | LIRSymbolic.FMov (dest, src) -> LIRSymbolic.FMov (applyF dest, applyF src)
    | LIRSymbolic.FAdd (dest, left, right) -> LIRSymbolic.FAdd (applyF dest, applyF left, applyF right)
    | LIRSymbolic.FSub (dest, left, right) -> LIRSymbolic.FSub (applyF dest, applyF left, applyF right)
    | LIRSymbolic.FMul (dest, left, right) -> LIRSymbolic.FMul (applyF dest, applyF left, applyF right)
    | LIRSymbolic.FDiv (dest, left, right) -> LIRSymbolic.FDiv (applyF dest, applyF left, applyF right)
    | LIRSymbolic.FNeg (dest, src) -> LIRSymbolic.FNeg (applyF dest, applyF src)
    | LIRSymbolic.FAbs (dest, src) -> LIRSymbolic.FAbs (applyF dest, applyF src)
    | LIRSymbolic.FSqrt (dest, src) -> LIRSymbolic.FSqrt (applyF dest, applyF src)
    | LIRSymbolic.FCmp (left, right) -> LIRSymbolic.FCmp (applyF left, applyF right)
    | LIRSymbolic.FLoad (dest, value) -> LIRSymbolic.FLoad (applyF dest, value)
    | LIRSymbolic.Int64ToFloat (dest, src) -> LIRSymbolic.Int64ToFloat (applyF dest, src)
    | LIRSymbolic.FloatToInt64 (dest, src) -> LIRSymbolic.FloatToInt64 (dest, applyF src)
    | LIRSymbolic.FloatToBits (dest, src) -> LIRSymbolic.FloatToBits (dest, applyF src)
    | LIRSymbolic.FpToGp (dest, src) -> LIRSymbolic.FpToGp (dest, applyF src)
    | LIRSymbolic.GpToFp (dest, src) -> LIRSymbolic.GpToFp (applyF dest, src)
    | LIRSymbolic.PrintFloat freg -> LIRSymbolic.PrintFloat (applyF freg)
    | LIRSymbolic.PrintFloatNoNewline freg -> LIRSymbolic.PrintFloatNoNewline (applyF freg)
    | LIRSymbolic.FPhi (dest, sources) ->
        LIRSymbolic.FPhi (applyF dest, sources |> List.map (fun (src, label) -> (applyF src, label)))
    | LIRSymbolic.FArgMoves moves ->
        LIRSymbolic.FArgMoves (moves |> List.map (fun (physReg, src) -> (physReg, applyF src)))
    | LIRSymbolic.FloatToString (dest, value) -> LIRSymbolic.FloatToString (dest, applyF value)
    // HeapStore with float value: the Virtual register ID is shared with FVirtual
    // We need to apply float allocation to convert Virtual(n) to the allocated physical register
    | LIRSymbolic.HeapStore (addr, offset, LIRSymbolic.Reg (LIR.Virtual vregId), Some AST.TFloat64) ->
        // Convert Virtual to the allocated FPhysical if it's in the float mapping
        let allocatedFReg = applyF (LIR.FVirtual vregId)
        // Convert the FReg back to a Virtual/Physical Reg for HeapStore
        let allocatedReg =
            match allocatedFReg with
            | LIR.FPhysical physFReg ->
                // Convert physical float reg to physical GP reg (for HeapStore operand format)
                // The actual STR_fp instruction will be generated in CodeGen based on valueType
                let physReg =
                    match physFReg with
                    | LIR.D0 -> LIR.X0 | LIR.D1 -> LIR.X1 | LIR.D2 -> LIR.X2 | LIR.D3 -> LIR.X3
                    | LIR.D4 -> LIR.X4 | LIR.D5 -> LIR.X5 | LIR.D6 -> LIR.X6 | LIR.D7 -> LIR.X7
                    | LIR.D8 -> LIR.X8 | LIR.D9 -> LIR.X9 | LIR.D10 -> LIR.X10 | LIR.D11 -> LIR.X11
                    | LIR.D12 -> LIR.X12 | LIR.D13 -> LIR.X13 | LIR.D14 -> LIR.X14 | LIR.D15 -> LIR.X15
                LIR.Physical physReg
            | LIR.FVirtual n -> LIR.Virtual n
        LIRSymbolic.HeapStore (addr, offset, LIRSymbolic.Reg allocatedReg, Some AST.TFloat64)
    | _ -> instr  // Non-float instructions unchanged

/// Apply float allocation to a basic block
let applyFloatAllocationToBlock (floatAllocation: FAllocationResult) (block: LIRSymbolic.BasicBlock) : LIRSymbolic.BasicBlock =
    { block with Instrs = block.Instrs |> List.map (applyFloatAllocationToInstr floatAllocation) }

/// Apply float allocation to a CFG
let applyFloatAllocationToCFG (floatAllocation: FAllocationResult) (cfg: LIRSymbolic.CFG) : LIRSymbolic.CFG =
    { cfg with Blocks = cfg.Blocks |> Map.map (fun _ block -> applyFloatAllocationToBlock floatAllocation block) }

// ============================================================================
// Linear Scan Register Allocation (kept for reference, not used)
// ============================================================================

/// Get the caller-saved physical registers that contain live values
let getLiveCallerSavedRegs (liveVRegs: Set<int>) (mapping: Map<int, Allocation>) : LIR.PhysReg list =
    liveVRegs
    |> Set.toList
    |> List.choose (fun vregId ->
        match Map.tryFind vregId mapping with
        | Some (PhysReg reg) when List.contains reg callerSavedRegs -> Some reg
        | _ -> None)
    |> List.distinct
    |> List.sort  // Keep consistent order for deterministic output

/// Get the caller-saved physical float registers that contain live values
let getLiveCallerSavedFloatRegs (liveFVRegs: Set<int>) (floatAllocation: FAllocationResult) : LIR.PhysFPReg list =
    liveFVRegs
    |> Set.toList
    |> List.choose (fun vregId -> Map.tryFind vregId floatAllocation.FMapping)
    |> List.filter (fun reg -> List.contains reg floatCallerSavedRegs)
    |> List.distinct
    |> List.sort

// ============================================================================
// Apply Allocation to LIR
// ============================================================================

/// Apply allocation to a register, returning the physical register and allocation info
let applyToReg (mapping: Map<int, Allocation>) (reg: LIRSymbolic.Reg) : LIRSymbolic.Reg * Allocation option =
    match reg with
    | LIR.Physical p -> (LIR.Physical p, None)
    | LIR.Virtual id ->
        match Map.tryFind id mapping with
        | Some (PhysReg physReg) -> (LIR.Physical physReg, None)
        | Some (StackSlot offset) -> (LIR.Physical LIR.X11, Some (StackSlot offset))
        | None -> (LIR.Physical LIR.X11, None)

/// Apply allocation to an operand, returning load instructions if needed
let applyToOperand (mapping: Map<int, Allocation>) (operand: LIRSymbolic.Operand) (tempReg: LIR.PhysReg)
    : LIRSymbolic.Operand * LIRSymbolic.Instr list =
    match operand with
    | LIRSymbolic.Imm n -> (LIRSymbolic.Imm n, [])
    | LIRSymbolic.FloatImm f -> (LIRSymbolic.FloatImm f, [])
    | LIRSymbolic.StringSymbol value -> (LIRSymbolic.StringSymbol value, [])
    | LIRSymbolic.FloatSymbol value -> (LIRSymbolic.FloatSymbol value, [])
    | LIRSymbolic.StackSlot s -> (LIRSymbolic.StackSlot s, [])
    | LIRSymbolic.Reg reg ->
        match reg with
        | LIR.Physical p -> (LIRSymbolic.Reg (LIR.Physical p), [])
        | LIR.Virtual id ->
            match Map.tryFind id mapping with
            | Some (PhysReg physReg) -> (LIRSymbolic.Reg (LIR.Physical physReg), [])
            | Some (StackSlot offset) ->
                let loadInstr = LIRSymbolic.Mov (LIR.Physical tempReg, LIRSymbolic.StackSlot offset)
                (LIRSymbolic.Reg (LIR.Physical tempReg), [loadInstr])
            // Keep Virtual unchanged if not in integer mapping - it may be a float register
            // that will be handled by float allocation later
            | None -> (LIRSymbolic.Reg (LIR.Virtual id), [])
    | LIRSymbolic.FuncAddr name -> (LIRSymbolic.FuncAddr name, [])

/// Apply allocation to an operand WITHOUT generating load instructions for spills.
/// Returns StackSlot for spilled values so CodeGen can load them at the right time.
/// Used for TailArgMoves where loads must be deferred to avoid using the same temp register.
let applyToOperandNoLoad (mapping: Map<int, Allocation>) (operand: LIRSymbolic.Operand) : LIRSymbolic.Operand =
    match operand with
    | LIRSymbolic.Imm n -> LIRSymbolic.Imm n
    | LIRSymbolic.FloatImm f -> LIRSymbolic.FloatImm f
    | LIRSymbolic.StringSymbol value -> LIRSymbolic.StringSymbol value
    | LIRSymbolic.FloatSymbol value -> LIRSymbolic.FloatSymbol value
    | LIRSymbolic.StackSlot s -> LIRSymbolic.StackSlot s
    | LIRSymbolic.Reg reg ->
        match reg with
        | LIR.Physical p -> LIRSymbolic.Reg (LIR.Physical p)
        | LIR.Virtual id ->
            match Map.tryFind id mapping with
            | Some (PhysReg physReg) -> LIRSymbolic.Reg (LIR.Physical physReg)
            | Some (StackSlot offset) -> LIRSymbolic.StackSlot offset
            // Keep Virtual unchanged if not in integer mapping - it may be a float register
            | None -> LIRSymbolic.Reg (LIR.Virtual id)
    | LIRSymbolic.FuncAddr name -> LIRSymbolic.FuncAddr name

/// Helper to load a spilled register
let loadSpilled (mapping: Map<int, Allocation>) (reg: LIRSymbolic.Reg) (tempReg: LIR.PhysReg)
    : LIRSymbolic.Reg * LIRSymbolic.Instr list =
    match reg with
    | LIR.Physical p -> (LIR.Physical p, [])
    | LIR.Virtual id ->
        match Map.tryFind id mapping with
        | Some (PhysReg physReg) -> (LIR.Physical physReg, [])
        | Some (StackSlot offset) ->
            let loadInstr = LIRSymbolic.Mov (LIR.Physical tempReg, LIRSymbolic.StackSlot offset)
            (LIR.Physical tempReg, [loadInstr])
        | None -> (LIR.Physical tempReg, [])

/// Apply allocation to an instruction
let applyToInstr (mapping: Map<int, Allocation>) (instr: LIRSymbolic.Instr) : LIRSymbolic.Instr list =
    match instr with
    | LIRSymbolic.Phi _ ->
        // Phi nodes are handled specially by resolvePhiNodes after allocation.
        // Skip them here - they will be removed and converted to moves at predecessor exits.
        []

    | LIRSymbolic.FPhi _ ->
        // Float phi nodes are handled specially by resolvePhiNodes after allocation.
        // Skip them here - they will be removed and converted to FMov at predecessor exits.
        []

    | LIRSymbolic.Mov (dest, src) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let (srcOp, srcLoads) = applyToOperand mapping src LIR.X12
        let movInstr = LIRSymbolic.Mov (destReg, srcOp)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIRSymbolic.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        srcLoads @ [movInstr] @ storeInstrs

    | LIRSymbolic.Store (offset, src) ->
        let (srcReg, srcLoads) = loadSpilled mapping src LIR.X12
        srcLoads @ [LIRSymbolic.Store (offset, srcReg)]

    | LIRSymbolic.Add (dest, left, right) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let (leftReg, leftLoads) = loadSpilled mapping left LIR.X12
        let (rightOp, rightLoads) = applyToOperand mapping right LIR.X13
        let addInstr = LIRSymbolic.Add (destReg, leftReg, rightOp)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIRSymbolic.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        leftLoads @ rightLoads @ [addInstr] @ storeInstrs

    | LIRSymbolic.Sub (dest, left, right) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let (leftReg, leftLoads) = loadSpilled mapping left LIR.X12
        let (rightOp, rightLoads) = applyToOperand mapping right LIR.X13
        let subInstr = LIRSymbolic.Sub (destReg, leftReg, rightOp)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIRSymbolic.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        leftLoads @ rightLoads @ [subInstr] @ storeInstrs

    | LIRSymbolic.Mul (dest, left, right) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let (leftReg, leftLoads) = loadSpilled mapping left LIR.X12
        let (rightReg, rightLoads) = loadSpilled mapping right LIR.X13
        let mulInstr = LIRSymbolic.Mul (destReg, leftReg, rightReg)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIRSymbolic.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        leftLoads @ rightLoads @ [mulInstr] @ storeInstrs

    | LIRSymbolic.Sdiv (dest, left, right) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let (leftReg, leftLoads) = loadSpilled mapping left LIR.X12
        let (rightReg, rightLoads) = loadSpilled mapping right LIR.X13
        let divInstr = LIRSymbolic.Sdiv (destReg, leftReg, rightReg)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIRSymbolic.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        leftLoads @ rightLoads @ [divInstr] @ storeInstrs

    | LIRSymbolic.Msub (dest, mulLeft, mulRight, sub) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let (mulLeftReg, mulLeftLoads) = loadSpilled mapping mulLeft LIR.X12
        let (mulRightReg, mulRightLoads) = loadSpilled mapping mulRight LIR.X13
        let (subReg, subLoads) = loadSpilled mapping sub LIR.X14
        let msubInstr = LIRSymbolic.Msub (destReg, mulLeftReg, mulRightReg, subReg)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIRSymbolic.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        mulLeftLoads @ mulRightLoads @ subLoads @ [msubInstr] @ storeInstrs

    | LIRSymbolic.Madd (dest, mulLeft, mulRight, add) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let (mulLeftReg, mulLeftLoads) = loadSpilled mapping mulLeft LIR.X12
        let (mulRightReg, mulRightLoads) = loadSpilled mapping mulRight LIR.X13
        let (addReg, addLoads) = loadSpilled mapping add LIR.X14
        let maddInstr = LIRSymbolic.Madd (destReg, mulLeftReg, mulRightReg, addReg)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIRSymbolic.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        mulLeftLoads @ mulRightLoads @ addLoads @ [maddInstr] @ storeInstrs

    | LIRSymbolic.Cmp (left, right) ->
        let (leftReg, leftLoads) = loadSpilled mapping left LIR.X12
        let (rightOp, rightLoads) = applyToOperand mapping right LIR.X13
        leftLoads @ rightLoads @ [LIRSymbolic.Cmp (leftReg, rightOp)]

    | LIRSymbolic.Cset (dest, cond) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let csetInstr = LIRSymbolic.Cset (destReg, cond)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIRSymbolic.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        [csetInstr] @ storeInstrs

    | LIRSymbolic.And (dest, left, right) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let (leftReg, leftLoads) = loadSpilled mapping left LIR.X12
        let (rightReg, rightLoads) = loadSpilled mapping right LIR.X13
        let andInstr = LIRSymbolic.And (destReg, leftReg, rightReg)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIRSymbolic.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        leftLoads @ rightLoads @ [andInstr] @ storeInstrs

    | LIRSymbolic.And_imm (dest, src, imm) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let (srcReg, srcLoads) = loadSpilled mapping src LIR.X12
        let andInstr = LIRSymbolic.And_imm (destReg, srcReg, imm)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIRSymbolic.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        srcLoads @ [andInstr] @ storeInstrs

    | LIRSymbolic.Orr (dest, left, right) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let (leftReg, leftLoads) = loadSpilled mapping left LIR.X12
        let (rightReg, rightLoads) = loadSpilled mapping right LIR.X13
        let orrInstr = LIRSymbolic.Orr (destReg, leftReg, rightReg)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIRSymbolic.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        leftLoads @ rightLoads @ [orrInstr] @ storeInstrs

    | LIRSymbolic.Eor (dest, left, right) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let (leftReg, leftLoads) = loadSpilled mapping left LIR.X12
        let (rightReg, rightLoads) = loadSpilled mapping right LIR.X13
        let eorInstr = LIRSymbolic.Eor (destReg, leftReg, rightReg)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIRSymbolic.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        leftLoads @ rightLoads @ [eorInstr] @ storeInstrs

    | LIRSymbolic.Lsl (dest, src, shift) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let (srcReg, srcLoads) = loadSpilled mapping src LIR.X12
        let (shiftReg, shiftLoads) = loadSpilled mapping shift LIR.X13
        let lslInstr = LIRSymbolic.Lsl (destReg, srcReg, shiftReg)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIRSymbolic.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        srcLoads @ shiftLoads @ [lslInstr] @ storeInstrs

    | LIRSymbolic.Lsr (dest, src, shift) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let (srcReg, srcLoads) = loadSpilled mapping src LIR.X12
        let (shiftReg, shiftLoads) = loadSpilled mapping shift LIR.X13
        let lsrInstr = LIRSymbolic.Lsr (destReg, srcReg, shiftReg)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIRSymbolic.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        srcLoads @ shiftLoads @ [lsrInstr] @ storeInstrs

    | LIRSymbolic.Lsl_imm (dest, src, shift) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let (srcReg, srcLoads) = loadSpilled mapping src LIR.X12
        let lslInstr = LIRSymbolic.Lsl_imm (destReg, srcReg, shift)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIRSymbolic.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        srcLoads @ [lslInstr] @ storeInstrs

    | LIRSymbolic.Lsr_imm (dest, src, shift) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let (srcReg, srcLoads) = loadSpilled mapping src LIR.X12
        let lsrInstr = LIRSymbolic.Lsr_imm (destReg, srcReg, shift)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIRSymbolic.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        srcLoads @ [lsrInstr] @ storeInstrs

    | LIRSymbolic.Mvn (dest, src) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let (srcReg, srcLoads) = loadSpilled mapping src LIR.X12
        let mvnInstr = LIRSymbolic.Mvn (destReg, srcReg)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIRSymbolic.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        srcLoads @ [mvnInstr] @ storeInstrs

    // Sign/zero extension instructions (for integer overflow)
    | LIRSymbolic.Sxtb (dest, src) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let (srcReg, srcLoads) = loadSpilled mapping src LIR.X12
        let extInstr = LIRSymbolic.Sxtb (destReg, srcReg)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIRSymbolic.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        srcLoads @ [extInstr] @ storeInstrs

    | LIRSymbolic.Sxth (dest, src) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let (srcReg, srcLoads) = loadSpilled mapping src LIR.X12
        let extInstr = LIRSymbolic.Sxth (destReg, srcReg)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIRSymbolic.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        srcLoads @ [extInstr] @ storeInstrs

    | LIRSymbolic.Sxtw (dest, src) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let (srcReg, srcLoads) = loadSpilled mapping src LIR.X12
        let extInstr = LIRSymbolic.Sxtw (destReg, srcReg)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIRSymbolic.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        srcLoads @ [extInstr] @ storeInstrs

    | LIRSymbolic.Uxtb (dest, src) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let (srcReg, srcLoads) = loadSpilled mapping src LIR.X12
        let extInstr = LIRSymbolic.Uxtb (destReg, srcReg)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIRSymbolic.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        srcLoads @ [extInstr] @ storeInstrs

    | LIRSymbolic.Uxth (dest, src) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let (srcReg, srcLoads) = loadSpilled mapping src LIR.X12
        let extInstr = LIRSymbolic.Uxth (destReg, srcReg)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIRSymbolic.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        srcLoads @ [extInstr] @ storeInstrs

    | LIRSymbolic.Uxtw (dest, src) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let (srcReg, srcLoads) = loadSpilled mapping src LIR.X12
        let extInstr = LIRSymbolic.Uxtw (destReg, srcReg)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIRSymbolic.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        srcLoads @ [extInstr] @ storeInstrs

    | LIRSymbolic.Call (dest, funcName, args) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let allocatedArgs =
            args |> List.mapi (fun i arg ->
                let tempReg = if i = 0 then LIR.X12 else LIR.X13
                applyToOperand mapping arg tempReg
            )
        let argLoads = allocatedArgs |> List.collect snd
        let argOps = allocatedArgs |> List.map fst
        let callInstr = LIRSymbolic.Call (destReg, funcName, argOps)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIRSymbolic.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        argLoads @ [callInstr] @ storeInstrs

    | LIRSymbolic.TailCall (funcName, args) ->
        // Tail calls have no destination - just apply allocation to args
        let allocatedArgs =
            args |> List.mapi (fun i arg ->
                let tempReg = if i = 0 then LIR.X12 else LIR.X13
                applyToOperand mapping arg tempReg
            )
        let argLoads = allocatedArgs |> List.collect snd
        let argOps = allocatedArgs |> List.map fst
        let callInstr = LIRSymbolic.TailCall (funcName, argOps)
        argLoads @ [callInstr]

    | LIRSymbolic.IndirectCall (dest, func, args) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let (funcReg, funcLoads) = loadSpilled mapping func LIR.X14
        let allocatedArgs =
            args |> List.mapi (fun i arg ->
                let tempReg = if i = 0 then LIR.X12 else LIR.X13
                applyToOperand mapping arg tempReg
            )
        let argLoads = allocatedArgs |> List.collect snd
        let argOps = allocatedArgs |> List.map fst
        let callInstr = LIRSymbolic.IndirectCall (destReg, funcReg, argOps)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIRSymbolic.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        funcLoads @ argLoads @ [callInstr] @ storeInstrs

    | LIRSymbolic.IndirectTailCall (func, args) ->
        // Indirect tail calls have no destination
        let (funcReg, funcLoads) = loadSpilled mapping func LIR.X14
        let allocatedArgs =
            args |> List.mapi (fun i arg ->
                let tempReg = if i = 0 then LIR.X12 else LIR.X13
                applyToOperand mapping arg tempReg
            )
        let argLoads = allocatedArgs |> List.collect snd
        let argOps = allocatedArgs |> List.map fst
        let callInstr = LIRSymbolic.IndirectTailCall (funcReg, argOps)
        funcLoads @ argLoads @ [callInstr]

    | LIRSymbolic.ClosureAlloc (dest, funcName, captures) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let allocatedCaptures =
            captures |> List.mapi (fun i cap ->
                let tempReg = if i = 0 then LIR.X12 else LIR.X13
                applyToOperand mapping cap tempReg
            )
        let capLoads = allocatedCaptures |> List.collect snd
        let capOps = allocatedCaptures |> List.map fst
        let allocInstr = LIRSymbolic.ClosureAlloc (destReg, funcName, capOps)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIRSymbolic.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        capLoads @ [allocInstr] @ storeInstrs

    | LIRSymbolic.ClosureCall (dest, closure, args) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let (closureReg, closureLoads) = loadSpilled mapping closure LIR.X14
        let allocatedArgs =
            args |> List.mapi (fun i arg ->
                let tempReg = if i = 0 then LIR.X12 else LIR.X13
                applyToOperand mapping arg tempReg
            )
        let argLoads = allocatedArgs |> List.collect snd
        let argOps = allocatedArgs |> List.map fst
        let callInstr = LIRSymbolic.ClosureCall (destReg, closureReg, argOps)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIRSymbolic.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        closureLoads @ argLoads @ [callInstr] @ storeInstrs

    | LIRSymbolic.ClosureTailCall (closure, args) ->
        // Closure tail calls have no destination
        let (closureReg, closureLoads) = loadSpilled mapping closure LIR.X14
        let allocatedArgs =
            args |> List.mapi (fun i arg ->
                let tempReg = if i = 0 then LIR.X12 else LIR.X13
                applyToOperand mapping arg tempReg
            )
        let argLoads = allocatedArgs |> List.collect snd
        let argOps = allocatedArgs |> List.map fst
        let callInstr = LIRSymbolic.ClosureTailCall (closureReg, argOps)
        closureLoads @ argLoads @ [callInstr]

    // SaveRegs/RestoreRegs are handled specially in applyToBlockWithLiveness
    // These patterns handle the case where they've already been populated
    | LIRSymbolic.SaveRegs (intRegs, floatRegs) -> [LIRSymbolic.SaveRegs (intRegs, floatRegs)]
    | LIRSymbolic.RestoreRegs (intRegs, floatRegs) -> [LIRSymbolic.RestoreRegs (intRegs, floatRegs)]

    | LIRSymbolic.ArgMoves moves ->
        // ArgMoves must preserve distinct sources for each argument.
        // Use no-load allocation so spilled values remain StackSlot and are
        // loaded per-move in CodeGen (avoids reusing a single temp).
        let allocatedMoves =
            moves |> List.map (fun (destReg, srcOp) ->
                let allocatedOp = applyToOperandNoLoad mapping srcOp
                (destReg, allocatedOp))
        [LIRSymbolic.ArgMoves allocatedMoves]

    | LIRSymbolic.TailArgMoves moves ->
        // Apply allocation WITHOUT loading spilled values into a temp register.
        // This is different from ArgMoves: for tail calls, we can't use a shared temp
        // because there's no SaveRegs to preserve values. CodeGen will handle StackSlots
        // by loading them directly into the destination register.
        let allocatedMoves =
            moves |> List.map (fun (destReg, srcOp) ->
                (destReg, applyToOperandNoLoad mapping srcOp))
        [LIRSymbolic.TailArgMoves allocatedMoves]

    | LIRSymbolic.FArgMoves moves ->
        // Pass through unchanged for now - float argument moves use physical registers only
        [LIRSymbolic.FArgMoves moves]

    | LIRSymbolic.PrintInt64 reg ->
        let (regFinal, regLoads) = loadSpilled mapping reg LIR.X12
        regLoads @ [LIRSymbolic.PrintInt64 regFinal]

    | LIRSymbolic.PrintBool reg ->
        let (regFinal, regLoads) = loadSpilled mapping reg LIR.X12
        regLoads @ [LIRSymbolic.PrintBool regFinal]

    | LIRSymbolic.PrintInt64NoNewline reg ->
        let (regFinal, regLoads) = loadSpilled mapping reg LIR.X12
        regLoads @ [LIRSymbolic.PrintInt64NoNewline regFinal]

    | LIRSymbolic.PrintBoolNoNewline reg ->
        let (regFinal, regLoads) = loadSpilled mapping reg LIR.X12
        regLoads @ [LIRSymbolic.PrintBoolNoNewline regFinal]

    | LIRSymbolic.PrintFloatNoNewline freg -> [LIRSymbolic.PrintFloatNoNewline freg]

    | LIRSymbolic.PrintHeapStringNoNewline reg ->
        let (regFinal, regLoads) = loadSpilled mapping reg LIR.X12
        regLoads @ [LIRSymbolic.PrintHeapStringNoNewline regFinal]

    | LIRSymbolic.PrintList (listPtr, elemType) ->
        let (ptrFinal, ptrLoads) = loadSpilled mapping listPtr LIR.X12
        ptrLoads @ [LIRSymbolic.PrintList (ptrFinal, elemType)]

    | LIRSymbolic.PrintSum (sumPtr, variants) ->
        let (ptrFinal, ptrLoads) = loadSpilled mapping sumPtr LIR.X12
        ptrLoads @ [LIRSymbolic.PrintSum (ptrFinal, variants)]

    | LIRSymbolic.PrintRecord (recordPtr, typeName, fields) ->
        let (ptrFinal, ptrLoads) = loadSpilled mapping recordPtr LIR.X12
        ptrLoads @ [LIRSymbolic.PrintRecord (ptrFinal, typeName, fields)]

    | LIRSymbolic.PrintFloat freg -> [LIRSymbolic.PrintFloat freg]
    | LIRSymbolic.PrintString value -> [LIRSymbolic.PrintString value]
    | LIRSymbolic.PrintChars chars -> [LIRSymbolic.PrintChars chars]
    | LIRSymbolic.PrintBytes reg ->
        let (regFinal, regLoads) = loadSpilled mapping reg LIR.X12
        regLoads @ [LIRSymbolic.PrintBytes regFinal]

    // FP instructions pass through unchanged
    | LIRSymbolic.FMov (dest, src) -> [LIRSymbolic.FMov (dest, src)]
    | LIRSymbolic.FLoad (dest, value) -> [LIRSymbolic.FLoad (dest, value)]
    | LIRSymbolic.FAdd (dest, left, right) -> [LIRSymbolic.FAdd (dest, left, right)]
    | LIRSymbolic.FSub (dest, left, right) -> [LIRSymbolic.FSub (dest, left, right)]
    | LIRSymbolic.FMul (dest, left, right) -> [LIRSymbolic.FMul (dest, left, right)]
    | LIRSymbolic.FDiv (dest, left, right) -> [LIRSymbolic.FDiv (dest, left, right)]
    | LIRSymbolic.FNeg (dest, src) -> [LIRSymbolic.FNeg (dest, src)]
    | LIRSymbolic.FAbs (dest, src) -> [LIRSymbolic.FAbs (dest, src)]
    | LIRSymbolic.FSqrt (dest, src) -> [LIRSymbolic.FSqrt (dest, src)]
    | LIRSymbolic.FCmp (left, right) -> [LIRSymbolic.FCmp (left, right)]
    // Int64ToFloat: src is integer register, dest is FP register
    | LIRSymbolic.Int64ToFloat (dest, src) ->
        let (srcReg, srcLoads) = loadSpilled mapping src LIR.X12
        srcLoads @ [LIRSymbolic.Int64ToFloat (dest, srcReg)]
    // GpToFp: move bits from GP register to FP register (src is integer, dest is FP)
    | LIRSymbolic.GpToFp (dest, src) ->
        let (srcReg, srcLoads) = loadSpilled mapping src LIR.X12
        srcLoads @ [LIRSymbolic.GpToFp (dest, srcReg)]
    // FloatToInt64: src is FP register, dest is integer register
    | LIRSymbolic.FloatToInt64 (dest, src) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let instr = LIRSymbolic.FloatToInt64 (destReg, src)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIRSymbolic.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        [instr] @ storeInstrs

    // FpToGp: src is FP register, dest is integer register
    | LIRSymbolic.FpToGp (dest, src) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let instr = LIRSymbolic.FpToGp (destReg, src)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIRSymbolic.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        [instr] @ storeInstrs

    // FloatToBits: src is FP register, dest is integer register (bit copy)
    | LIRSymbolic.FloatToBits (dest, src) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let instr = LIRSymbolic.FloatToBits (destReg, src)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIRSymbolic.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        [instr] @ storeInstrs

    // Heap operations
    | LIRSymbolic.HeapAlloc (dest, size) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let allocInstr = LIRSymbolic.HeapAlloc (destReg, size)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIRSymbolic.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        [allocInstr] @ storeInstrs

    | LIRSymbolic.HeapStore (addr, offset, src, vt) ->
        let (addrReg, addrLoads) = loadSpilled mapping addr LIR.X12
        let (srcOp, srcLoads) = applyToOperand mapping src LIR.X13
        addrLoads @ srcLoads @ [LIRSymbolic.HeapStore (addrReg, offset, srcOp, vt)]

    | LIRSymbolic.HeapLoad (dest, addr, offset) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let (addrReg, addrLoads) = loadSpilled mapping addr LIR.X12
        let loadInstr = LIRSymbolic.HeapLoad (destReg, addrReg, offset)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIRSymbolic.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        addrLoads @ [loadInstr] @ storeInstrs

    | LIRSymbolic.RefCountInc (addr, payloadSize) ->
        let (addrReg, addrLoads) = loadSpilled mapping addr LIR.X12
        addrLoads @ [LIRSymbolic.RefCountInc (addrReg, payloadSize)]

    | LIRSymbolic.RefCountDec (addr, payloadSize) ->
        let (addrReg, addrLoads) = loadSpilled mapping addr LIR.X12
        addrLoads @ [LIRSymbolic.RefCountDec (addrReg, payloadSize)]

    | LIRSymbolic.StringConcat (dest, left, right) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let (leftOp, leftLoads) = applyToOperand mapping left LIR.X12
        let (rightOp, rightLoads) = applyToOperand mapping right LIR.X13
        let concatInstr = LIRSymbolic.StringConcat (destReg, leftOp, rightOp)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIRSymbolic.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        leftLoads @ rightLoads @ [concatInstr] @ storeInstrs

    | LIRSymbolic.PrintHeapString reg ->
        let (regPhys, regLoads) = loadSpilled mapping reg LIR.X12
        regLoads @ [LIRSymbolic.PrintHeapString regPhys]

    | LIRSymbolic.LoadFuncAddr (dest, funcName) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let loadInstr = LIRSymbolic.LoadFuncAddr (destReg, funcName)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIRSymbolic.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        [loadInstr] @ storeInstrs

    | LIRSymbolic.FileReadText (dest, path) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let (pathOp, pathLoads) = applyToOperand mapping path LIR.X12
        let fileInstr = LIRSymbolic.FileReadText (destReg, pathOp)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIRSymbolic.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        pathLoads @ [fileInstr] @ storeInstrs

    | LIRSymbolic.FileExists (dest, path) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let (pathOp, pathLoads) = applyToOperand mapping path LIR.X12
        let fileInstr = LIRSymbolic.FileExists (destReg, pathOp)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIRSymbolic.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        pathLoads @ [fileInstr] @ storeInstrs

    | LIRSymbolic.FileWriteText (dest, path, content) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let (pathOp, pathLoads) = applyToOperand mapping path LIR.X12
        let (contentOp, contentLoads) = applyToOperand mapping content LIR.X13
        let fileInstr = LIRSymbolic.FileWriteText (destReg, pathOp, contentOp)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIRSymbolic.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        pathLoads @ contentLoads @ [fileInstr] @ storeInstrs

    | LIRSymbolic.FileAppendText (dest, path, content) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let (pathOp, pathLoads) = applyToOperand mapping path LIR.X12
        let (contentOp, contentLoads) = applyToOperand mapping content LIR.X13
        let fileInstr = LIRSymbolic.FileAppendText (destReg, pathOp, contentOp)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIRSymbolic.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        pathLoads @ contentLoads @ [fileInstr] @ storeInstrs

    | LIRSymbolic.FileDelete (dest, path) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let (pathOp, pathLoads) = applyToOperand mapping path LIR.X12
        let fileInstr = LIRSymbolic.FileDelete (destReg, pathOp)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIRSymbolic.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        pathLoads @ [fileInstr] @ storeInstrs

    | LIRSymbolic.FileSetExecutable (dest, path) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let (pathOp, pathLoads) = applyToOperand mapping path LIR.X12
        let fileInstr = LIRSymbolic.FileSetExecutable (destReg, pathOp)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIRSymbolic.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        pathLoads @ [fileInstr] @ storeInstrs

    | LIRSymbolic.FileWriteFromPtr (dest, path, ptr, length) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let (pathOp, pathLoads) = applyToOperand mapping path LIR.X12
        let (ptrReg, ptrLoads) = loadSpilled mapping ptr LIR.X13
        let (lengthReg, lengthLoads) = loadSpilled mapping length LIR.X14
        let fileInstr = LIRSymbolic.FileWriteFromPtr (destReg, pathOp, ptrReg, lengthReg)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIRSymbolic.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        pathLoads @ ptrLoads @ lengthLoads @ [fileInstr] @ storeInstrs

    | LIRSymbolic.RawAlloc (dest, numBytes) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let (numBytesReg, numBytesLoads) = loadSpilled mapping numBytes LIR.X12
        let allocInstr = LIRSymbolic.RawAlloc (destReg, numBytesReg)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIRSymbolic.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        numBytesLoads @ [allocInstr] @ storeInstrs

    | LIRSymbolic.RawFree ptr ->
        let (ptrReg, ptrLoads) = loadSpilled mapping ptr LIR.X12
        ptrLoads @ [LIRSymbolic.RawFree ptrReg]

    | LIRSymbolic.RawGet (dest, ptr, byteOffset) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let (ptrReg, ptrLoads) = loadSpilled mapping ptr LIR.X12
        let (offsetReg, offsetLoads) = loadSpilled mapping byteOffset LIR.X13
        let getInstr = LIRSymbolic.RawGet (destReg, ptrReg, offsetReg)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIRSymbolic.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        ptrLoads @ offsetLoads @ [getInstr] @ storeInstrs

    | LIRSymbolic.RawGetByte (dest, ptr, byteOffset) ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let (ptrReg, ptrLoads) = loadSpilled mapping ptr LIR.X12
        let (offsetReg, offsetLoads) = loadSpilled mapping byteOffset LIR.X13
        let getInstr = LIRSymbolic.RawGetByte (destReg, ptrReg, offsetReg)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIRSymbolic.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        ptrLoads @ offsetLoads @ [getInstr] @ storeInstrs

    | LIRSymbolic.RawSet (ptr, byteOffset, value) ->
        let (ptrReg, ptrLoads) = loadSpilled mapping ptr LIR.X12
        let (offsetReg, offsetLoads) = loadSpilled mapping byteOffset LIR.X13
        let (valueReg, valueLoads) = loadSpilled mapping value LIR.X14
        ptrLoads @ offsetLoads @ valueLoads @ [LIRSymbolic.RawSet (ptrReg, offsetReg, valueReg)]

    | LIRSymbolic.RawSetByte (ptr, byteOffset, value) ->
        let (ptrReg, ptrLoads) = loadSpilled mapping ptr LIR.X12
        let (offsetReg, offsetLoads) = loadSpilled mapping byteOffset LIR.X13
        let (valueReg, valueLoads) = loadSpilled mapping value LIR.X14
        ptrLoads @ offsetLoads @ valueLoads @ [LIRSymbolic.RawSetByte (ptrReg, offsetReg, valueReg)]

    | LIRSymbolic.RefCountIncString str ->
        let (strOp, strLoads) = applyToOperand mapping str LIR.X12
        strLoads @ [LIRSymbolic.RefCountIncString strOp]

    | LIRSymbolic.RefCountDecString str ->
        let (strOp, strLoads) = applyToOperand mapping str LIR.X12
        strLoads @ [LIRSymbolic.RefCountDecString strOp]

    | LIRSymbolic.RandomInt64 dest ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let randomInstr = LIRSymbolic.RandomInt64 destReg
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIRSymbolic.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        [randomInstr] @ storeInstrs

    | LIRSymbolic.DateNow dest ->
        let (destReg, destAlloc) = applyToReg mapping dest
        let dateInstr = LIRSymbolic.DateNow destReg
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIRSymbolic.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        [dateInstr] @ storeInstrs

    | LIRSymbolic.FloatToString (dest, value) ->
        // FP register value is already physical after float allocation
        let (destReg, destAlloc) = applyToReg mapping dest
        let floatToStrInstr = LIRSymbolic.FloatToString (destReg, value)
        let storeInstrs =
            match destAlloc with
            | Some (StackSlot offset) -> [LIRSymbolic.Store (offset, LIR.Physical LIR.X11)]
            | _ -> []
        [floatToStrInstr] @ storeInstrs

    | LIRSymbolic.CoverageHit exprId ->
        [LIRSymbolic.CoverageHit exprId]  // No registers to allocate

    | LIRSymbolic.Exit -> [LIRSymbolic.Exit]

/// Apply allocation to terminator
let applyToTerminator (mapping: Map<int, Allocation>) (term: LIRSymbolic.Terminator)
    : LIRSymbolic.Instr list * LIRSymbolic.Terminator =
    match term with
    | LIRSymbolic.Ret -> ([], LIRSymbolic.Ret)
    | LIRSymbolic.Branch (cond, trueLabel, falseLabel) ->
        match cond with
        | LIR.Virtual id ->
            match Map.tryFind id mapping with
            | Some (PhysReg physReg) ->
                ([], LIRSymbolic.Branch (LIR.Physical physReg, trueLabel, falseLabel))
            | Some (StackSlot offset) ->
                // Load condition from stack before branching
                let loadInstr = LIRSymbolic.Mov (LIR.Physical LIR.X11, LIRSymbolic.StackSlot offset)
                ([loadInstr], LIRSymbolic.Branch (LIR.Physical LIR.X11, trueLabel, falseLabel))
            | None ->
                ([], LIRSymbolic.Branch (LIR.Physical LIR.X11, trueLabel, falseLabel))
        | LIR.Physical p ->
            ([], LIRSymbolic.Branch (LIR.Physical p, trueLabel, falseLabel))
    | LIRSymbolic.BranchZero (cond, zeroLabel, nonZeroLabel) ->
        match cond with
        | LIR.Virtual id ->
            match Map.tryFind id mapping with
            | Some (PhysReg physReg) ->
                ([], LIRSymbolic.BranchZero (LIR.Physical physReg, zeroLabel, nonZeroLabel))
            | Some (StackSlot offset) ->
                // Load condition from stack before branching
                let loadInstr = LIRSymbolic.Mov (LIR.Physical LIR.X11, LIRSymbolic.StackSlot offset)
                ([loadInstr], LIRSymbolic.BranchZero (LIR.Physical LIR.X11, zeroLabel, nonZeroLabel))
            | None ->
                ([], LIRSymbolic.BranchZero (LIR.Physical LIR.X11, zeroLabel, nonZeroLabel))
        | LIR.Physical p ->
            ([], LIRSymbolic.BranchZero (LIR.Physical p, zeroLabel, nonZeroLabel))
    | LIRSymbolic.BranchBitZero (reg, bit, zeroLabel, nonZeroLabel) ->
        match reg with
        | LIR.Virtual id ->
            match Map.tryFind id mapping with
            | Some (PhysReg physReg) ->
                ([], LIRSymbolic.BranchBitZero (LIR.Physical physReg, bit, zeroLabel, nonZeroLabel))
            | Some (StackSlot offset) ->
                let loadInstr = LIRSymbolic.Mov (LIR.Physical LIR.X11, LIRSymbolic.StackSlot offset)
                ([loadInstr], LIRSymbolic.BranchBitZero (LIR.Physical LIR.X11, bit, zeroLabel, nonZeroLabel))
            | None ->
                ([], LIRSymbolic.BranchBitZero (LIR.Physical LIR.X11, bit, zeroLabel, nonZeroLabel))
        | LIR.Physical p ->
            ([], LIRSymbolic.BranchBitZero (LIR.Physical p, bit, zeroLabel, nonZeroLabel))
    | LIRSymbolic.BranchBitNonZero (reg, bit, nonZeroLabel, zeroLabel) ->
        match reg with
        | LIR.Virtual id ->
            match Map.tryFind id mapping with
            | Some (PhysReg physReg) ->
                ([], LIRSymbolic.BranchBitNonZero (LIR.Physical physReg, bit, nonZeroLabel, zeroLabel))
            | Some (StackSlot offset) ->
                let loadInstr = LIRSymbolic.Mov (LIR.Physical LIR.X11, LIRSymbolic.StackSlot offset)
                ([loadInstr], LIRSymbolic.BranchBitNonZero (LIR.Physical LIR.X11, bit, nonZeroLabel, zeroLabel))
            | None ->
                ([], LIRSymbolic.BranchBitNonZero (LIR.Physical LIR.X11, bit, nonZeroLabel, zeroLabel))
        | LIR.Physical p ->
            ([], LIRSymbolic.BranchBitNonZero (LIR.Physical p, bit, nonZeroLabel, zeroLabel))
    | LIRSymbolic.Jump label -> ([], LIRSymbolic.Jump label)
    | LIRSymbolic.CondBranch (cond, trueLabel, falseLabel) ->
        // CondBranch uses condition flags, not a register - pass through unchanged
        ([], LIRSymbolic.CondBranch (cond, trueLabel, falseLabel))

/// Apply allocation to a basic block with liveness-aware SaveRegs/RestoreRegs population
let applyToBlockWithLiveness
    (mapping: Map<int, Allocation>)
    (floatAllocation: FAllocationResult)
    (liveOut: Set<int>)
    (floatLiveOut: Set<int>)
    (block: LIRSymbolic.BasicBlock)
    : LIRSymbolic.BasicBlock =

    // Compute liveness at each instruction point
    let instrLiveness = computeInstructionLiveness block liveOut
    let floatInstrLiveness = computeFloatInstructionLiveness block floatLiveOut

    // Process each instruction with its corresponding liveness
    // Debug: check lengths match
    let instrCount = List.length block.Instrs
    let livenessCount = List.length instrLiveness
    let floatLivenessCount = List.length floatInstrLiveness
    if instrCount <> livenessCount || instrCount <> floatLivenessCount then
        let message =
            $"Instruction count ({instrCount}) doesn't match liveness count ({livenessCount}) or float liveness count ({floatLivenessCount})"
        Crash.crash message

    // First pass: find SaveRegs/RestoreRegs pairs and compute the registers to save
    // For each SaveRegs, look ahead to find the matching RestoreRegs and use its liveness
    // This ensures SaveRegs and RestoreRegs have matching register lists
    let mutable savedRegsStack : (LIR.PhysReg list * LIR.PhysFPReg list) list = []

    let allocatedInstrs =
        List.zip3 block.Instrs instrLiveness floatInstrLiveness
        |> List.collect (fun (instr, liveAfter, floatLiveAfter) ->
            match instr with
            | LIRSymbolic.SaveRegs ([], []) ->
                // At SaveRegs, we need to save registers that are:
                // 1. Currently live (have values that might be clobbered by the call)
                // 2. Needed after the call
                // The liveAfter here includes both categories, so we use it
                let liveCallerSaved = getLiveCallerSavedRegs liveAfter mapping
                let liveCallerSavedFloat = getLiveCallerSavedFloatRegs floatLiveAfter floatAllocation
                // Push onto stack for matching RestoreRegs
                savedRegsStack <- (liveCallerSaved, liveCallerSavedFloat) :: savedRegsStack
                applyToInstr mapping (LIRSymbolic.SaveRegs (liveCallerSaved, liveCallerSavedFloat))
            | LIRSymbolic.RestoreRegs ([], []) ->
                // Pop the matching SaveRegs registers
                let (liveCallerSaved, liveCallerSavedFloat) =
                    match savedRegsStack with
                    | (intRegs, floatRegs) :: tail ->
                        savedRegsStack <- tail
                        (intRegs, floatRegs)
                    | [] ->
                        Crash.crash "Unmatched RestoreRegs: SaveRegs stack is empty"
                applyToInstr mapping (LIRSymbolic.RestoreRegs (liveCallerSaved, liveCallerSavedFloat))
            | _ ->
                applyToInstr mapping instr)

    let (termLoads, allocatedTerm) = applyToTerminator mapping block.Terminator
    { Label = block.Label
      Instrs = allocatedInstrs @ termLoads
      Terminator = allocatedTerm }

/// Apply allocation to CFG with liveness info
let applyToCFGWithLiveness
    (mapping: Map<int, Allocation>)
    (floatAllocation: FAllocationResult)
    (cfg: LIRSymbolic.CFG)
    (liveness: Map<LIR.Label, BlockLiveness>)
    (floatLiveness: Map<LIR.Label, BlockLiveness>)
    : LIRSymbolic.CFG =
    { Entry = cfg.Entry
      Blocks =
        cfg.Blocks
        |> Map.map (fun label block ->
            let blockLiveness = Map.find label liveness
            let floatBlockLiveness =
                match Map.tryFind label floatLiveness with
                | Some fl -> fl
                | None -> { LiveIn = Set.empty; LiveOut = Set.empty }
            applyToBlockWithLiveness mapping floatAllocation blockLiveness.LiveOut floatBlockLiveness.LiveOut block) }

// ============================================================================
// Float Move Generation (used by both phi resolution and param copies)
// ============================================================================

/// Generate float move instructions using allocation-based register mapping.
/// Uses the float allocation result instead of modulo-based mapping.
let generateFloatMoveInstrsWithAllocation
    (moves: (LIR.FReg * LIR.FReg) list)
    (floatAllocation: FAllocationResult) : LIRSymbolic.Instr list =

    if List.isEmpty moves then []
    else
        // Map FVirtual to physical register ID using allocation
        let fregToPhysId (freg: LIR.FReg) : int =
            match freg with
            | LIR.FPhysical p -> physFPRegToInt p
            | LIR.FVirtual 1000 -> 18  // D18 (left temp for binary ops) - fixed
            | LIR.FVirtual 1001 -> 17  // D17 (right temp for binary ops) - fixed
            | LIR.FVirtual 2000 -> 16  // D16 (cycle resolution temp) - fixed
            | LIR.FVirtual n when n >= 3000 && n < 4000 ->
                19 + ((n - 3000) % 8)  // D19-D26 (call arg temps) - fixed
            | LIR.FVirtual id ->
                // Look up in allocation
                match Map.tryFind id floatAllocation.FMapping with
                | Some physReg -> physFPRegToInt physReg
                | None ->
                    // Fallback to old modulo mapping for unallocated VRegs
                    // This can happen for VRegs that weren't in the CFG (e.g., dead code)
                    if id >= 0 && id <= 7 then 2 + id  // D2-D9 for params 0-7
                    elif id < 10000 then
                        let tempRegs = [| 0; 1; 10; 11; 12; 13; 14; 15; 27; 28; 29; 30; 31 |]
                        tempRegs[(id - 8) % 13]
                    else
                        let tempRegs = [| 0; 1; 10; 11; 12; 13; 14; 15; 27; 28; 29; 30; 31 |]
                        tempRegs[((id - 10000) + 7) % 13]

        // Convert moves to physical register IDs for cycle detection
        let physMoves = moves |> List.map (fun (dest, src) -> (fregToPhysId dest, fregToPhysId src))

        let getSrcPhysId (src: int) : int option = Some src

        let actions = ParallelMoves.resolve physMoves getSrcPhysId

        // Convert actions back to FMov instructions using original FRegs
        let destMap = moves |> List.map (fun (dest, _) -> (fregToPhysId dest, dest)) |> Map.ofList
        let srcMap = moves |> List.map (fun (_, src) -> (fregToPhysId src, src)) |> Map.ofList

        actions
        |> List.collect (fun action ->
            match action with
            | ParallelMoves.SaveToTemp physId ->
                match Map.tryFind physId srcMap with
                | Some srcFreg -> [LIRSymbolic.FMov (LIR.FVirtual 2000, srcFreg)]
                | None -> []
            | ParallelMoves.Move (destPhysId, srcPhysId) ->
                match Map.tryFind destPhysId destMap, Map.tryFind srcPhysId srcMap with
                | Some destFreg, Some srcFreg -> [LIRSymbolic.FMov (destFreg, srcFreg)]
                | _ -> []
            | ParallelMoves.MoveFromTemp destPhysId ->
                match Map.tryFind destPhysId destMap with
                | Some destFreg -> [LIRSymbolic.FMov (destFreg, LIR.FVirtual 2000)]
                | None -> [])

// ============================================================================
// Phi Resolution
// ============================================================================

/// Resolve phi nodes by inserting parallel moves at predecessor block exits.
/// This function:
/// 1. Finds all phi nodes in each block
/// 2. Drops phis whose destination is never used
/// 3. For each predecessor, collects all (dest, src) pairs for moves
/// 4. Uses ParallelMoves.resolve to sequence the moves properly (handling cycles)
/// 5. Inserts the moves at the end of each predecessor (before terminator)
/// 6. Removes phi nodes from blocks
let resolvePhiNodes
    (cfg: LIRSymbolic.CFG)
    (allocation: Map<int, Allocation>)
    (floatAllocation: FAllocationResult)
    : LIRSymbolic.CFG =
    let collectNonPhiUses (cfg: LIRSymbolic.CFG) : Set<int> =
        cfg.Blocks
        |> Map.fold (fun acc _ block ->
            let instrUses =
                block.Instrs
                |> List.fold (fun acc instr ->
                    match instr with
                    | LIRSymbolic.Phi _ -> acc
                    | LIRSymbolic.FPhi _ -> acc
                    | _ -> Set.union acc (getUsedVRegs instr)) Set.empty
            let termUses = getTerminatorUsedVRegs block.Terminator
            Set.union acc (Set.union instrUses termUses)
        ) Set.empty

    let collectPhiSources (cfg: LIRSymbolic.CFG) : Map<int, Set<int>> =
        let addSource (dest: int) (src: int) (acc: Map<int, Set<int>>) =
            let existing = Map.tryFind dest acc |> Option.defaultValue Set.empty
            Map.add dest (Set.add src existing) acc

        cfg.Blocks
        |> Map.fold (fun acc _ block ->
            block.Instrs
            |> List.fold (fun acc instr ->
                match instr with
                | LIRSymbolic.Phi (LIR.Virtual destId, sources, _) ->
                    sources
                    |> List.fold (fun acc (src, _) ->
                        match src with
                        | LIRSymbolic.Reg (LIR.Virtual srcId) -> addSource destId srcId acc
                        | _ -> acc) acc
                | _ -> acc) acc
        ) Map.empty

    let collectPhysicalPhiSources (cfg: LIRSymbolic.CFG) : Set<int> =
        cfg.Blocks
        |> Map.fold (fun acc _ block ->
            block.Instrs
            |> List.fold (fun acc instr ->
                match instr with
                | LIRSymbolic.Phi (LIR.Physical _, sources, _) ->
                    sources
                    |> List.fold (fun acc (src, _) ->
                        match src with
                        | LIRSymbolic.Reg (LIR.Virtual srcId) -> Set.add srcId acc
                        | _ -> acc) acc
                | _ -> acc) acc
        ) Set.empty

    let computeNeededVRegs (cfg: LIRSymbolic.CFG) : Set<int> =
        let phiSources = collectPhiSources cfg
        let rootUses = Set.union (collectNonPhiUses cfg) (collectPhysicalPhiSources cfg)
        let rec expand (needed: Set<int>) (worklist: int list) : Set<int> =
            match worklist with
            | [] -> needed
            | v :: rest ->
                match Map.tryFind v phiSources with
                | None -> expand needed rest
                | Some sources ->
                    let newSources = Set.difference sources needed
                    let needed' = Set.union needed newSources
                    let worklist' = (Set.toList newSources) @ rest
                    expand needed' worklist'

        expand rootUses (Set.toList rootUses)

    let neededVRegs = computeNeededVRegs cfg

    let phiDestNeeded (dest: LIRSymbolic.Reg) : bool =
        match dest with
        | LIR.Virtual id -> Set.contains id neededVRegs
        | LIR.Physical _ -> true

    // Get the allocation for a virtual register (register or stack slot)
    let getDestAllocation (reg: LIRSymbolic.Reg) : Allocation =
        match reg with
        | LIR.Virtual id ->
            match Map.tryFind id allocation with
            | Some alloc -> alloc
            | None -> Crash.crash $"RegisterAllocation: Virtual register {id} not found in allocation"
        | LIR.Physical p -> PhysReg p

    // Helper to convert a LIRSymbolic.Operand to allocated version
    let operandToAllocated (op: LIRSymbolic.Operand) : LIRSymbolic.Operand =
        match op with
        | LIRSymbolic.Reg (LIR.Virtual id) ->
            match Map.tryFind id allocation with
            | Some (PhysReg r) -> LIRSymbolic.Reg (LIR.Physical r)
            | Some (StackSlot offset) -> LIRSymbolic.StackSlot offset
            | None -> op
        | LIRSymbolic.Reg (LIR.Physical p) -> LIRSymbolic.Reg (LIR.Physical p)
        | _ -> op

    // Collect all int phi info: for each phi, get (dest_reg, src_operand, pred_label)
    // This gives us: List of (dest, sources, valueType)
    let intPhiInfo =
        cfg.Blocks
        |> Map.toList
        |> List.collect (fun (_, block) ->
            block.Instrs
            |> List.choose (fun instr ->
                match instr with
                | LIRSymbolic.Phi (dest, sources, valueType) ->
                    if phiDestNeeded dest then
                        Some (dest, sources, valueType)
                    else
                        None
                | _ -> None))

    // Collect all float phi info: (dest FReg, source FRegs with labels)
    let floatPhiInfo =
        cfg.Blocks
        |> Map.toList
        |> List.collect (fun (_, block) ->
            block.Instrs
            |> List.choose (fun instr ->
                match instr with
                | LIRSymbolic.FPhi (dest, sources) -> Some (dest, sources)
                | _ -> None))

    // Group int phis by predecessor: Map<pred_label, List<(dest_allocation, src_operand)>>
    // Keep the full Allocation type to handle both register and stack destinations
    let predecessorIntMoves : Map<LIR.Label, (Allocation * LIRSymbolic.Operand) list> =
        intPhiInfo
        |> List.collect (fun (dest, sources, _valueType) ->
            let destAlloc = getDestAllocation dest
            sources |> List.map (fun (src, predLabel) ->
                let srcAllocated = operandToAllocated src
                (predLabel, (destAlloc, srcAllocated))))
        |> List.groupBy fst
        |> List.map (fun (predLabel, pairs) ->
            (predLabel, pairs |> List.map snd))
        |> Map.ofList

    // Group float phis by predecessor: Map<pred_label, List<(dest_freg, src_freg)>>
    // Float registers don't go through allocation - FVirtual maps directly to D regs in CodeGen
    let predecessorFloatMoves : Map<LIR.Label, (LIR.FReg * LIR.FReg) list> =
        floatPhiInfo
        |> List.collect (fun (dest, sources) ->
            sources |> List.map (fun (src, predLabel) ->
                (predLabel, (dest, src))))
        |> List.groupBy fst
        |> List.map (fun (predLabel, pairs) ->
            (predLabel, pairs |> List.map snd))
        |> Map.ofList

    // Generate move instructions for phi resolution using parallel move resolution
    // across both register and stack destinations (handles reg<->stack cycles).
    let generateIntMoveInstrs (moves: (Allocation * LIRSymbolic.Operand) list) : LIRSymbolic.Instr list =
        let getSrcAllocation (op: LIRSymbolic.Operand) : Allocation option =
            match op with
            | LIRSymbolic.Reg (LIR.Physical p) -> Some (PhysReg p)
            | LIRSymbolic.StackSlot offset -> Some (StackSlot offset)
            | _ -> None

        let actions = ParallelMoves.resolve moves getSrcAllocation

        let saveToTemp (loc: Allocation) : LIRSymbolic.Instr list =
            match loc with
            | PhysReg r -> [LIRSymbolic.Mov (LIR.Physical LIR.X16, LIRSymbolic.Reg (LIR.Physical r))]
            | StackSlot offset -> [LIRSymbolic.Mov (LIR.Physical LIR.X16, LIRSymbolic.StackSlot offset)]

        let moveFromTemp (loc: Allocation) : LIRSymbolic.Instr list =
            match loc with
            | PhysReg r -> [LIRSymbolic.Mov (LIR.Physical r, LIRSymbolic.Reg (LIR.Physical LIR.X16))]
            | StackSlot offset -> [LIRSymbolic.Store (offset, LIR.Physical LIR.X16)]

        let moveToDest (dest: Allocation) (src: LIRSymbolic.Operand) : LIRSymbolic.Instr list =
            match dest with
            | PhysReg r -> [LIRSymbolic.Mov (LIR.Physical r, src)]
            | StackSlot offset ->
                match src with
                | LIRSymbolic.Reg (LIR.Physical r) ->
                    [LIRSymbolic.Store (offset, LIR.Physical r)]
                | _ ->
                    [LIRSymbolic.Mov (LIR.Physical LIR.X16, src)
                     LIRSymbolic.Store (offset, LIR.Physical LIR.X16)]

        actions
        |> List.collect (fun action ->
            match action with
            | ParallelMoves.SaveToTemp loc -> saveToTemp loc
            | ParallelMoves.Move (dest, src) -> moveToDest dest src
            | ParallelMoves.MoveFromTemp dest -> moveFromTemp dest)

    // Add moves to predecessor blocks
    let mutable updatedBlocks = cfg.Blocks

    // Add int phi moves
    for kvp in predecessorIntMoves do
        let predLabel = kvp.Key
        let moves = kvp.Value
        match Map.tryFind predLabel updatedBlocks with
        | Some predBlock ->
            let moveInstrs = generateIntMoveInstrs moves
            let updatedBlock = { predBlock with Instrs = predBlock.Instrs @ moveInstrs }
            updatedBlocks <- Map.add predLabel updatedBlock updatedBlocks
        | None -> ()

    // Add float phi moves
    // IMPORTANT: For tail call blocks, the phi resolution is ALREADY handled by:
    // 1. FArgMoves: puts new values in D0-D7
    // 2. TailCall: jumps back to function entry
    // 3. Param copy at entry: copies D0-D7 to phi destination registers
    // So we should SKIP phi resolution for tail call backedges - it's redundant and incorrect.
    //
    // For non-tail-call predecessors, append moves at the end as usual.
    for kvp in predecessorFloatMoves do
        let predLabel = kvp.Key
        let moves = kvp.Value
        match Map.tryFind predLabel updatedBlocks with
        | Some predBlock ->
            // Add phi moves at end of predecessor block
            let moveInstrs = generateFloatMoveInstrsWithAllocation moves floatAllocation
            let updatedBlock = { predBlock with Instrs = predBlock.Instrs @ moveInstrs }
            updatedBlocks <- Map.add predLabel updatedBlock updatedBlocks
        | None ->
            ()

    // Remove phi and fphi nodes from all blocks
    updatedBlocks <-
        updatedBlocks
        |> Map.map (fun _ block ->
            let filteredInstrs =
                block.Instrs
                |> List.filter (fun instr ->
                    match instr with
                    | LIRSymbolic.Phi _ -> false
                    | LIRSymbolic.FPhi _ -> false
                    | _ -> true)
            { block with Instrs = filteredInstrs })

    { cfg with Blocks = updatedBlocks }

// ============================================================================
// Main Entry Point
// ============================================================================

/// Parameter registers per ARM64 calling convention (X0-X7 for ints, D0-D7 for floats)
let parameterRegs = [LIR.X0; LIR.X1; LIR.X2; LIR.X3; LIR.X4; LIR.X5; LIR.X6; LIR.X7]
let floatParamRegs = [LIR.D0; LIR.D1; LIR.D2; LIR.D3; LIR.D4; LIR.D5; LIR.D6; LIR.D7]

/// Allocate registers for a function
let allocateRegisters (func: LIRSymbolic.Function) : LIRSymbolic.Function =
    // Step 1: Compute liveness
    let liveness = computeLiveness func.CFG

    // Precompute parameter info with separate int/float counters (AAPCS64)
    // Needed for entry defs and float allocation.
    let paramsWithTypes = func.TypedParams |> List.map (fun tp -> (tp.Reg, tp.Type))
    let _, _, intParams, floatParams =
        paramsWithTypes
        |> List.fold (fun (intIdx, floatIdx, intAcc, floatAcc) (reg, typ) ->
            if typ = AST.TFloat64 then
                // Float parameter - uses D registers
                (intIdx, floatIdx + 1, intAcc, (reg, floatIdx) :: floatAcc)
            else
                // Int/other parameter - uses X registers
                (intIdx + 1, floatIdx, (reg, intIdx) :: intAcc, floatAcc)
        ) (0, 0, [], [])
    let intParams = List.rev intParams
    let floatParams = List.rev floatParams

    let intParamVRegs =
        intParams
        |> List.choose (fun (reg, _) ->
            match reg with
            | LIR.Virtual id -> Some id
            | LIR.Physical _ -> None)
        |> Set.ofList

    // Extract FVirtual IDs from float params for allocation
    // Float params use Virtual register IDs that are also FVirtual IDs
    let floatParamFVirtualIds =
        floatParams
        |> List.choose (fun (reg, _) ->
            match reg with
            | LIR.Virtual id -> Some id
            | LIR.Physical _ -> None)
        |> Set.ofList

    // Step 2: Build interference graph
    let graph = buildInterferenceGraph func.CFG liveness intParamVRegs

    // Step 2b: Collect coalescing preferences and move pairs
    let preferences = collectPhiPreferences func.CFG
    let movePairs =
        let moves = collectMovePairs func.CFG
        let phiPairs = collectPhiPairs func.CFG
        Set.ofList (moves @ phiPairs) |> Set.toList

    // Step 3: Run chordal graph coloring with phi coalescing
    // Use optimal register order based on calling pattern:
    // - Functions with non-tail calls: callee-saved first (save once in prologue/epilogue)
    // - Leaf functions / tail-call-only: caller-saved first (no prologue overhead)
    let regs = getAllocatableRegs func.CFG
    let colorResult = chordalGraphColor graph Map.empty (List.length regs) preferences movePairs
    let result = coloringToAllocation colorResult regs

    // Step 3b: Parameter info already computed (needed for float allocation and param moves)

    // Step 3c: Run float register allocation
    // Include float param FVirtuals so they get allocated even if not used in CFG
    let floatAllocation = chordalFloatAllocation func.CFG floatParamFVirtualIds

    // Step 5: Build mapping that copies INT parameters from X0-X7
    // to wherever chordal graph coloring allocated them.
    // IMPORTANT: Use proper parallel move resolution to handle cycles!
    // (e.g., X1→X2 and X2→X1 require a temp register)
    let intParamMoves =
        intParams
        |> List.choose (fun (reg, paramIdx) ->
            match reg with
            | LIR.Virtual id ->
                let paramReg = List.item paramIdx parameterRegs
                match Map.tryFind id result.Mapping with
                | Some (PhysReg allocatedReg) when allocatedReg <> paramReg ->
                    // Need to copy from paramReg to allocatedReg
                    Some (allocatedReg, LIRSymbolic.Reg (LIR.Physical paramReg))
                | Some (StackSlot _offset) ->
                    // Store to stack - not a register move, handle separately
                    None // We'll handle stack stores separately
                | _ -> None // Same register or not in mapping
            | LIR.Physical _ -> None)

    // Collect stack stores separately (they don't conflict with register moves)
    let intParamStackStores =
        intParams
        |> List.choose (fun (reg, paramIdx) ->
            match reg with
            | LIR.Virtual id ->
                let paramReg = List.item paramIdx parameterRegs
                match Map.tryFind id result.Mapping with
                | Some (StackSlot offset) ->
                    Some (LIRSymbolic.Store (offset, LIR.Physical paramReg))
                | _ -> None
            | LIR.Physical _ -> None)

    // Use parallel move resolution for register-to-register moves
    let getSrcReg (op: LIRSymbolic.Operand) : LIR.PhysReg option =
        match op with
        | LIRSymbolic.Reg (LIR.Physical r) -> Some r
        | _ -> None

    let moveActions = ParallelMoves.resolve intParamMoves getSrcReg

    // Convert move actions to LIR instructions using X16 as temp register
    let regMoveInstrs =
        moveActions
        |> List.collect (fun action ->
            match action with
            | ParallelMoves.SaveToTemp reg ->
                [LIRSymbolic.Mov (LIR.Physical LIR.X16, LIRSymbolic.Reg (LIR.Physical reg))]
            | ParallelMoves.Move (dest, src) ->
                [LIRSymbolic.Mov (LIR.Physical dest, src)]
            | ParallelMoves.MoveFromTemp dest ->
                [LIRSymbolic.Mov (LIR.Physical dest, LIRSymbolic.Reg (LIR.Physical LIR.X16))])

    // Combine register moves and stack stores
    let intParamCopyInstrs = regMoveInstrs @ intParamStackStores

    // Step 6: Build mapping that copies FLOAT parameters from D0-D7
    // Float parameters use FVirtual registers (same ID as Virtual)
    // and don't go through linear scan - they map directly in CodeGen
    // IMPORTANT: Use parallel move resolution to handle cases where destination
    // registers collide with source registers (e.g., when FVirtual id maps to D0
    // which is also a source register for other params)
    let floatParamMoves =
        floatParams
        |> List.choose (fun (reg, paramIdx) ->
            match reg with
            | LIR.Virtual id ->
                // Float param comes in D0/D1/etc, needs to be in FVirtual id
                let srcDReg = List.item paramIdx floatParamRegs
                let destFVirtual = LIR.FVirtual id
                Some (destFVirtual, LIR.FPhysical srcDReg)
            | LIR.Physical _ -> None)

    let floatParamCopyInstrs = generateFloatMoveInstrsWithAllocation floatParamMoves floatAllocation

    // Step 6b: Extract entry-edge phi moves for float phis
    // For phis at the entry block, we need to add moves from entry-edge sources
    // to phi destinations. These moves don't get added by resolvePhiNodes because
    // there's no predecessor block for "before function entry".
    let entryBlockBeforeResolution = Map.find func.CFG.Entry func.CFG.Blocks

    let entryEdgeFloatPhiMoves =
        entryBlockBeforeResolution.Instrs
        |> List.choose (fun instr ->
            match instr with
            | LIRSymbolic.FPhi (dest, sources) ->
                // Find sources where the predecessor label doesn't exist in the CFG
                // (these are entry-edge sources)
                let entryEdgeSources =
                    sources
                    |> List.filter (fun (_, predLabel) ->
                        not (Map.containsKey predLabel func.CFG.Blocks))
                // Generate moves for entry-edge sources
                entryEdgeSources
                |> List.map (fun (src, _) -> (dest, src))
                |> Some
            | _ -> None)
        |> List.concat

    // Generate FMov instructions for entry-edge phi resolution
    // Use parallel move resolution to handle potential register conflicts
    let entryEdgePhiInstrs = generateFloatMoveInstrsWithAllocation entryEdgeFloatPhiMoves floatAllocation

    // Step 7: Resolve phi nodes (convert to moves at predecessor exits)
    // This must happen BEFORE applying allocation since we need to know where each
    // value is allocated to generate the correct moves
    let cfgWithPhiResolved = resolvePhiNodes func.CFG result.Mapping floatAllocation

    // Step 8: Apply allocation to CFG with liveness info for SaveRegs/RestoreRegs population
    let floatLiveness = computeFloatLiveness func.CFG
    let allocatedCFG = applyToCFGWithLiveness result.Mapping floatAllocation cfgWithPhiResolved liveness floatLiveness

    // Step 8b: Apply float allocation to CFG - convert FVirtual to FPhysical
    let allocatedCFG = applyFloatAllocationToCFG floatAllocation allocatedCFG

    // Step 9: Insert parameter copy instructions at the start of the entry block
    // Float param copies go first (they use separate register bank)
    // Entry-edge phi moves come after param copies (they copy from param FVirtual to phi dest FVirtual)
    // IMPORTANT: Apply float allocation to param copy instructions since they were generated
    // before applyFloatAllocationToCFG ran and still contain FVirtual registers
    let allocatedFloatParamCopyInstrs =
        floatParamCopyInstrs |> List.map (applyFloatAllocationToInstr floatAllocation)
    let allocatedEntryEdgePhiInstrs =
        entryEdgePhiInstrs |> List.map (applyFloatAllocationToInstr floatAllocation)

    let entryBlock = Map.find allocatedCFG.Entry allocatedCFG.Blocks
    let entryBlockWithCopies = {
        entryBlock with
            Instrs = allocatedFloatParamCopyInstrs @ allocatedEntryEdgePhiInstrs @ intParamCopyInstrs @ entryBlock.Instrs
    }

    let updatedBlocks = Map.add allocatedCFG.Entry entryBlockWithCopies allocatedCFG.Blocks
    let cfgWithParamCopies = { allocatedCFG with Blocks = updatedBlocks }

    // Step 10: Set parameters to calling convention registers
    // Create TypedLIRParams with physical registers
    let allocatedTypedParams : LIR.TypedLIRParam list =
        func.TypedParams
        |> List.mapi (fun i tp -> { Reg = LIR.Physical (List.item i parameterRegs); Type = tp.Type })

    { Name = func.Name
      TypedParams = allocatedTypedParams
      CFG = cfgWithParamCopies
      StackSize = result.StackSize
      UsedCalleeSaved = result.UsedCalleeSaved }
