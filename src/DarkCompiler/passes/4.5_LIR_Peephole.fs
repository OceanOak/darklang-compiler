// 4.5_LIR_Peephole.fs - LIR Peephole Optimizations
//
// Performs low-level optimizations on LIR:
// - Remove identity operations (add x, y, 0 → mov x, y)
// - Remove self-moves (mov x, x → remove)
// - Constant multiplication optimizations (mul x, y, 0 → mov x, 0)
// - Dead move elimination
//
// These optimizations work on individual instructions or small sequences.

module LIR_Peephole

open LIRSymbolic

(* Plan:
   1) Mirror MIR's loop discovery in LIR: build predecessor/successor maps and dominators.
   2) Identify natural loops with a single preheader that jumps to the header.
   3) Hoist loop-invariant Mov(Imm ...) for virtual regs into the preheader and remove from loop blocks.
   4) Run LICM alongside existing peephole passes in optimizeCFGOnce. *)

/// Check if an operand is an immediate with value 0
let isZero (op: Operand) : bool =
    match op with
    | Imm 0L -> true
    | _ -> false

/// Check if an operand is an immediate with value 1
let isOne (op: Operand) : bool =
    match op with
    | Imm 1L -> true
    | _ -> false

/// Check if two registers are the same
let sameReg (r1: Reg) (r2: Reg) : bool =
    match r1, r2 with
    | LIR.Physical p1, LIR.Physical p2 -> p1 = p2
    | LIR.Virtual v1, LIR.Virtual v2 -> v1 = v2
    | _ -> false

/// Get successor labels from a terminator
let getSuccessors (term: Terminator) : Label list =
    match term with
    | Ret -> []
    | Jump label -> [label]
    | Branch (_, trueLabel, falseLabel) -> [trueLabel; falseLabel]
    | BranchZero (_, zeroLabel, nonZeroLabel) -> [zeroLabel; nonZeroLabel]
    | BranchBitZero (_, _, zeroLabel, nonZeroLabel) -> [zeroLabel; nonZeroLabel]
    | BranchBitNonZero (_, _, nonZeroLabel, zeroLabel) -> [nonZeroLabel; zeroLabel]
    | CondBranch (_, trueLabel, falseLabel) -> [trueLabel; falseLabel]

/// Build predecessor map for the CFG
let buildPredecessors (cfg: CFG) : Map<Label, Label list> =
    let emptyPreds =
        cfg.Blocks
        |> Map.toList
        |> List.map (fun (label, _) -> (label, []))
        |> Map.ofList

    cfg.Blocks
    |> Map.fold (fun preds label block ->
        getSuccessors block.Terminator
        |> List.fold (fun acc succ ->
            let existing = Map.tryFind succ acc |> Option.defaultValue []
            Map.add succ (label :: existing) acc
        ) preds
    ) emptyPreds

/// Build successor map for the CFG
let buildSuccessors (cfg: CFG) : Map<Label, Label list> =
    cfg.Blocks |> Map.map (fun _ block -> getSuccessors block.Terminator)

/// Compute dominator sets for each block
let computeDominators (cfg: CFG) (preds: Map<Label, Label list>) : Map<Label, Set<Label>> =
    let labels = cfg.Blocks |> Map.toList |> List.map fst |> Set.ofList
    let entry = cfg.Entry

    let initial =
        labels
        |> Set.fold (fun acc label ->
            let doms =
                if label = entry then Set.singleton label
                else labels
            Map.add label doms acc
        ) Map.empty

    let rec loop doms =
        let updated =
            labels
            |> Set.fold (fun acc label ->
                if label = entry then
                    Map.add label (Set.singleton label) acc
                else
                    let predSets =
                        Map.tryFind label preds
                        |> Option.defaultValue []
                        |> List.choose (fun pred -> Map.tryFind pred doms)
                    let intersect =
                        match predSets with
                        | [] -> Set.singleton label
                        | first :: rest -> rest |> List.fold Set.intersect first |> Set.add label
                    Map.add label intersect acc
            ) Map.empty
        if updated = doms then updated else loop updated
    loop initial

/// Identify natural loops via backedges (header dominates source)
let findNaturalLoops (cfg: CFG) : Map<Label, Set<Label>> =
    let preds = buildPredecessors cfg
    let doms = computeDominators cfg preds
    let succs = buildSuccessors cfg

    let dominates (dominator: Label) (node: Label) : bool =
        Map.tryFind node doms
        |> Option.map (Set.contains dominator)
        |> Option.defaultValue false

    let backedges =
        succs
        |> Map.fold (fun acc from successors ->
            successors
            |> List.fold (fun acc' succ ->
                if dominates succ from then
                    let existing = Map.tryFind succ acc' |> Option.defaultValue []
                    Map.add succ (from :: existing) acc'
                else
                    acc'
            ) acc
        ) Map.empty

    backedges
    |> Map.fold (fun loops header sources ->
        let loopBlocks =
            sources
            |> List.fold (fun acc source ->
                let initial = Set.ofList [header; source]
                let rec grow work loopSet =
                    match work with
                    | [] -> loopSet
                    | node :: rest ->
                        let nodePreds = Map.tryFind node preds |> Option.defaultValue []
                        let (loopSet', work') =
                            nodePreds
                            |> List.fold (fun (setAcc, workAcc) pred ->
                                if Set.contains pred setAcc then
                                    (setAcc, workAcc)
                                elif dominates header pred then
                                    (Set.add pred setAcc, pred :: workAcc)
                                else
                                    (setAcc, workAcc)
                            ) (loopSet, rest)
                        grow work' loopSet'
                Set.union acc (grow [source] initial)
            ) Set.empty
        if Set.isEmpty loopBlocks then loops else Map.add header loopBlocks loops
    ) Map.empty

/// Check whether an instruction is a hoistable constant move
let isHoistableConstMove (instr: Instr) : Reg option =
    match instr with
    | Mov (dest, Imm _) ->
        match dest with
        | LIR.Virtual _ -> Some dest
        | _ -> None
    | _ -> None

/// Check whether an instruction represents a call (affects register saving)
let isCallInstr (instr: Instr) : bool =
    match instr with
    | Call _
    | TailCall _
    | IndirectCall _
    | IndirectTailCall _
    | ClosureCall _
    | ClosureTailCall _ -> true
    | _ -> false

/// Check whether an instruction is pure arithmetic/logic for LICM safety
let isPureLoopInstr (instr: Instr) : bool =
    match instr with
    | Mov _
    | Phi _
    | FPhi _
    | Add _
    | Sub _
    | Mul _
    | Sdiv _
    | Msub _
    | Madd _
    | Cmp _
    | Cset _
    | And _
    | And_imm _
    | Orr _
    | Eor _
    | Lsl _
    | Lsr _
    | Lsl_imm _
    | Lsr_imm _
    | Mvn _
    | Sxtb _
    | Sxth _
    | Sxtw _
    | Uxtb _
    | Uxth _
    | Uxtw _
    | FMov _
    | FLoad _
    | FAdd _
    | FSub _
    | FMul _
    | FDiv _
    | FNeg _
    | FAbs _
    | FSqrt _
    | FCmp _
    | IntToFloat _
    | FloatToInt _
    | GpToFp _
    | FpToGp _ -> true
    | _ -> false

/// Hoist loop-invariant Mov(Imm ...) into simple preheaders
let applyLoopInvariantConstHoist (cfg: CFG) : CFG * bool =
    let loops = findNaturalLoops cfg
    let preds = buildPredecessors cfg
    let labelName (LIR.Label name) = name

    loops
    |> Map.fold (fun (cfgAcc, changedAcc) header loopBlocks ->
        let outsidePreds =
            Map.tryFind header preds
            |> Option.defaultValue []
            |> List.filter (fun pred -> not (Set.contains pred loopBlocks))

        let tryGetPreheader =
            match outsidePreds with
            | [preheader] ->
                match Map.tryFind preheader cfgAcc.Blocks with
                | Some block ->
                    match block.Terminator with
                    | Jump target when target = header -> Some preheader
                    | _ -> None
                | None -> None
            | _ -> None

        match tryGetPreheader with
        | None -> (cfgAcc, changedAcc)
        | Some preheader ->
            let loopHasCall =
                loopBlocks
                |> Set.exists (fun label ->
                    match Map.tryFind label cfgAcc.Blocks with
                    | None -> false
                    | Some block -> block.Instrs |> List.exists isCallInstr
                )

            let loopIsPure =
                loopBlocks
                |> Set.forall (fun label ->
                    match Map.tryFind label cfgAcc.Blocks with
                    | None -> true
                    | Some block -> block.Instrs |> List.forall isPureLoopInstr
                )

            if loopHasCall || not loopIsPure then
                (cfgAcc, changedAcc)
            else
            let blockOrder =
                header :: (loopBlocks |> Set.remove header |> Set.toList |> List.sortBy labelName)

            let (hoistedRev, hoistedDests) =
                blockOrder
                |> List.fold (fun (moves, dests) label ->
                    match Map.tryFind label cfgAcc.Blocks with
                    | None -> (moves, dests)
                    | Some block ->
                        block.Instrs
                        |> List.fold (fun (movesAcc, destsAcc) instr ->
                            match isHoistableConstMove instr with
                            | Some dest when not (Set.contains dest destsAcc) ->
                                (instr :: movesAcc, Set.add dest destsAcc)
                            | _ -> (movesAcc, destsAcc)
                        ) (moves, dests)
                ) ([], Set.empty)

            let hoistedMoves = List.rev hoistedRev
            if List.isEmpty hoistedMoves then
                (cfgAcc, changedAcc)
            else
                let blocks' =
                    cfgAcc.Blocks
                    |> Map.map (fun label block ->
                        if label = preheader then
                            { block with Instrs = block.Instrs @ hoistedMoves }
                        elif Set.contains label loopBlocks then
                            let instrs' =
                                block.Instrs
                                |> List.filter (fun instr ->
                                    match isHoistableConstMove instr with
                                    | Some dest -> not (Set.contains dest hoistedDests)
                                    | None -> true
                                )
                            { block with Instrs = instrs' }
                        else
                            block
                    )
                ({ cfgAcc with Blocks = blocks' }, true)
    ) (cfg, false)

/// Optimize a single instruction (returns None to remove, Some to replace)
let optimizeInstr (instr: Instr) : Instr option =
    match instr with
    // Remove self-moves: mov x, x → remove
    | Mov (dest, Reg src) when sameReg dest src ->
        None

    // Add with zero: add x, y, 0 → mov x, y (if x != y) or remove (if x == y)
    | Add (dest, left, Imm 0L) ->
        if sameReg dest left then None
        else Some (Mov (dest, Reg left))

    // Add with zero on left: we don't have this form in LIR

    // Sub with zero: sub x, y, 0 → mov x, y or remove
    | Sub (dest, left, Imm 0L) ->
        if sameReg dest left then None
        else Some (Mov (dest, Reg left))

    // Multiply by zero: mul x, y, z where z is zero → mov x, 0
    // This requires both operands to be registers in LIR, so we can't detect 0

    // Multiply by one: would require one operand to be immediate, but Mul takes two regs

    // For now, keep the instruction as-is
    | _ -> Some instr

/// Optimize a list of instructions (single-pass peephole)
let optimizeInstrs (instrs: Instr list) : Instr list =
    instrs
    |> List.choose optimizeInstr

/// Check if a register is used in any instruction (for dead code detection)
let isRegUsedInInstrs (reg: Reg) (instrs: Instr list) : bool =
    instrs |> List.exists (fun instr ->
        match instr with
        | Mov (_, Reg r) -> sameReg r reg
        | Mov (_, _) -> false
        | Add (_, left, Reg right) -> sameReg left reg || sameReg right reg
        | Add (_, left, _) -> sameReg left reg
        | Sub (_, left, Reg right) -> sameReg left reg || sameReg right reg
        | Sub (_, left, _) -> sameReg left reg
        | Mul (_, left, right) -> sameReg left reg || sameReg right reg
        | Cmp (left, Reg right) -> sameReg left reg || sameReg right reg
        | Cmp (left, _) -> sameReg left reg
        | Cset _ -> false  // Cset only writes, doesn't read
        | _ -> false  // Conservative: assume not used for other instructions
    )

/// Try to fuse MUL + ADD into MADD (multiply-add)
/// Pattern: MUL temp, a, b; ADD dest, temp, Reg c → MADD dest, a, b, c
/// Or:      MUL temp, a, b; ADD dest, Reg c, temp → MADD dest, a, b, c (commutative)
let tryFuseMulAdd (instrs: Instr list) : Instr list =
    let rec loop acc remaining =
        match remaining with
        | [] -> List.rev acc
        | [single] -> List.rev (single :: acc)
        | Mul (mulDest, mulLeft, mulRight) :: Add (addDest, addLeft, Reg addRight) :: rest
            when sameReg mulDest addLeft && not (sameReg mulDest addRight) ->
            // MUL temp, a, b; ADD dest, temp, c → MADD dest, a, b, c
            // Check that temp is not used later (dead after the ADD)
            if not (isRegUsedInInstrs mulDest rest) then
                loop (Madd (addDest, mulLeft, mulRight, addRight) :: acc) rest
            else
                loop (Mul (mulDest, mulLeft, mulRight) :: acc) (Add (addDest, addLeft, Reg addRight) :: rest)
        | Mul (mulDest, mulLeft, mulRight) :: Add (addDest, addLeft, Reg addRight) :: rest
            when sameReg mulDest addRight && not (sameReg mulDest addLeft) ->
            // MUL temp, a, b; ADD dest, c, temp → MADD dest, a, b, c (commutative)
            if not (isRegUsedInInstrs mulDest rest) then
                loop (Madd (addDest, mulLeft, mulRight, addLeft) :: acc) rest
            else
                loop (Mul (mulDest, mulLeft, mulRight) :: acc) (Add (addDest, addLeft, Reg addRight) :: rest)
        | instr :: rest ->
            loop (instr :: acc) rest
    loop [] instrs

/// Try to fuse Cset + Branch into CondBranch
/// Pattern: last instruction is Cset dest, cond; terminator is Branch dest, trueL, falseL
/// Result: remove Cset, replace Branch with CondBranch cond, trueL, falseL
let tryFuseCondBranch (instrs: Instr list) (terminator: Terminator) : (Instr list * Terminator) option =
    match terminator with
    | Branch (condReg, trueLabel, falseLabel) ->
        // Check if last instruction is Cset writing to condReg
        match List.tryLast instrs with
        | Some (Cset (dest, cond)) when sameReg dest condReg ->
            // Check that condReg is not used elsewhere in the block (except the Cset and Branch)
            let otherInstrs = instrs |> List.take (List.length instrs - 1)
            if not (isRegUsedInInstrs condReg otherInstrs) then
                // Fuse: remove Cset and replace Branch with CondBranch
                Some (otherInstrs, CondBranch (cond, trueLabel, falseLabel))
            else
                None
        | _ -> None
    | _ -> None

/// Check if a value is a power of 2 (exactly one bit set)
let isPowerOf2 (n: int64) : bool =
    n > 0L && (n &&& (n - 1L)) = 0L

/// Get the bit position of a power-of-2 value (log2)
let bitPosition (n: int64) : int =
    let rec loop pos x =
        if x = 1L then pos
        else loop (pos + 1) (x >>> 1)
    loop 0 n

/// Try to fuse AND_imm (power-of-2 mask) + BranchZero/Branch into BranchBitZero/BranchBitNonZero
/// Pattern: last instruction is AND_imm dest, src, mask where mask is power of 2
///          terminator is BranchZero(dest, ...) or Branch(dest, ...)
/// Result: BranchBitZero(src, bitNum, ...) or BranchBitNonZero(src, bitNum, ...)
/// This uses TBZ/TBNZ instructions which test a single bit
let tryFuseAndBitBranch (instrs: Instr list) (terminator: Terminator) : (Instr list * Terminator) option =
    match List.tryLast instrs with
    | Some (And_imm (andDest, andSrc, mask)) when isPowerOf2 mask ->
        let bit = bitPosition mask
        let otherInstrs = instrs |> List.take (List.length instrs - 1)
        // Check that andDest is not used in the remaining instructions
        if isRegUsedInInstrs andDest otherInstrs then
            None
        else
            match terminator with
            | BranchZero (condReg, zeroLabel, nonZeroLabel) when sameReg condReg andDest ->
                // AND_imm + CBZ → TBZ
                Some (otherInstrs, BranchBitZero (andSrc, bit, zeroLabel, nonZeroLabel))
            | Branch (condReg, nonZeroLabel, zeroLabel) when sameReg condReg andDest ->
                // AND_imm + CBNZ → TBNZ
                Some (otherInstrs, BranchBitNonZero (andSrc, bit, nonZeroLabel, zeroLabel))
            | _ -> None
    | _ -> None

/// Try to fuse CMP reg, #0 + CondBranch into Branch/BranchZero
/// Pattern: last instruction is CMP reg, #0; terminator is CondBranch(EQ/NE, ...)
/// Result:
///   - CMP reg, #0 + CondBranch(EQ, true, false) → BranchZero(reg, true, false)  [uses CBZ]
///   - CMP reg, #0 + CondBranch(NE, true, false) → Branch(reg, true, false)      [uses CBNZ]
let tryFuseCmpZeroBranch (instrs: Instr list) (terminator: Terminator) : (Instr list * Terminator) option =
    match terminator with
    | CondBranch (cond, trueLabel, falseLabel) ->
        // Check if last instruction is CMP reg, #0
        match List.tryLast instrs with
        | Some (Cmp (cmpReg, Imm 0L)) ->
            let otherInstrs = instrs |> List.take (List.length instrs - 1)
            match cond with
            | LIR.EQ ->
                // CMP reg, #0 + B.eq → CBZ reg (BranchZero)
                Some (otherInstrs, BranchZero (cmpReg, trueLabel, falseLabel))
            | LIR.NE ->
                // CMP reg, #0 + B.ne → CBNZ reg (Branch)
                Some (otherInstrs, Branch (cmpReg, trueLabel, falseLabel))
            | _ ->
                // Other conditions (LT, GT, LE, GE) can't be fused with CBZ/CBNZ
                None
        | _ -> None
    | _ -> None

/// Apply TBZ/TBNZ fusion if applicable
/// Fuses AND_imm (power-of-2 mask) + BranchZero/Branch → BranchBitZero/BranchBitNonZero
let applyAndBitBranchFusion (instrs: Instr list) (terminator: Terminator) : (Instr list * Terminator) =
    match tryFuseAndBitBranch instrs terminator with
    | Some (fusedInstrs, fusedTerminator) -> (fusedInstrs, fusedTerminator)
    | None -> (instrs, terminator)

/// Optimize a basic block (returns whether anything changed)
let optimizeBlock (block: BasicBlock) : BasicBlock * bool =
    let instrs' = optimizeInstrs block.Instrs
    // Apply MUL + ADD → MADD fusion
    let instrs'' = tryFuseMulAdd instrs'

    // Try to fuse Cset + Branch into CondBranch
    let (instrs''', terminator') =
        match tryFuseCondBranch instrs'' block.Terminator with
        | Some (fusedInstrs, fusedTerminator) ->
            // After fusing Cset + Branch → CondBranch, try to fuse CMP #0 + CondBranch → CBZ/CBNZ
            match tryFuseCmpZeroBranch fusedInstrs fusedTerminator with
            | Some (fusedInstrs2, fusedTerminator2) ->
                (fusedInstrs2, fusedTerminator2)
            | None ->
                (fusedInstrs, fusedTerminator)
        | None ->
            // Also try CMP #0 + CondBranch fusion on the original terminator
            match tryFuseCmpZeroBranch instrs'' block.Terminator with
            | Some (fusedInstrs, fusedTerminator) ->
                (fusedInstrs, fusedTerminator)
            | None ->
                (instrs'', block.Terminator)

    // Try to fuse AND_imm (power-of-2) + BranchZero/Branch → TBZ/TBNZ
    let (finalInstrs, finalTerminator) = applyAndBitBranchFusion instrs''' terminator'
    let block' = { block with Instrs = finalInstrs; Terminator = finalTerminator }
    (block', block' <> block)

/// Optimize a CFG in a single pass (returns whether anything changed)
let optimizeCFGOnce (cfg: CFG) : CFG * bool =
    let (blocks', changed) =
        cfg.Blocks
        |> Map.fold (fun (acc, ch) label block ->
            let (block', blockChanged) = optimizeBlock block
            (Map.add label block' acc, ch || blockChanged)
        ) (Map.empty, false)
    let cfg' = { cfg with Blocks = blocks' }
    let (cfg'', hoisted) = applyLoopInvariantConstHoist cfg'
    (cfg'', changed || hoisted)

/// Optimize a CFG until fixed point
let optimizeCFG (cfg: CFG) : CFG =
    let rec loop current remaining =
        if remaining <= 0 then
            current
        else
            let (next, changed) = optimizeCFGOnce current
            if changed then
                loop next (remaining - 1)
            else
                next
    loop cfg 10

/// Optimize a function
let optimizeFunction (func: Function) : Function =
    { func with CFG = optimizeCFG func.CFG }

/// Optimize a program
let optimizeProgram (program: Program) : Program =
    let (Program functions) = program
    let functions' = functions |> List.map optimizeFunction
    Program functions'
