// MIROptimizeTests.fs - Unit tests for MIR optimizer fixpoint behavior
//
// Verifies that MIR optimization re-runs to a fixed point when copy propagation
// creates new CSE opportunities.

module MIROptimizeTests

open MIR
open MIR_Optimize
open IRPrinter

type TestResult = Result<unit, string>

let testCseAfterCopyPropFixpoint () : TestResult =
    let entry = Label "entry"
    let block: BasicBlock = {
        Label = entry
        Instrs = [
            Mov (VReg 2, Register (VReg 0), Some AST.TInt64)
            BinOp (VReg 3, Add, Register (VReg 2), Register (VReg 1), AST.TInt64)
            BinOp (VReg 4, Add, Register (VReg 0), Register (VReg 1), AST.TInt64)
            BinOp (VReg 5, Add, Register (VReg 3), Register (VReg 4), AST.TInt64)
        ]
        Terminator = Ret (Register (VReg 5))
    }
    let cfg: CFG = {
        Entry = entry
        Blocks = Map.ofList [ (entry, block) ]
    }
    let func: Function = {
        Name = "fixpoint_cse"
        TypedParams = [
            { Reg = VReg 0; Type = AST.TInt64 }
            { Reg = VReg 1; Type = AST.TInt64 }
        ]
        ReturnType = AST.TInt64
        CFG = cfg
        FloatRegs = Set.empty
    }
    let program = Program ([func], Map.empty, Map.empty)

    let (Program (functions, _, _)) = optimizeProgram program
    let optimizedFunc = functions |> List.head
    let optimizedBlock = optimizedFunc.CFG.Blocks |> Map.find entry

    let expectedInstrs = [
        BinOp (VReg 3, Add, Register (VReg 0), Register (VReg 1), AST.TInt64)
        BinOp (VReg 5, Add, Register (VReg 3), Register (VReg 3), AST.TInt64)
    ]
    let expectedBlock = { block with Instrs = expectedInstrs }

    if optimizedBlock = expectedBlock then
        Ok ()
    else
        let actual = formatMIR (Program ([optimizedFunc], Map.empty, Map.empty))
        Error $"MIR optimization did not reach fixpoint.\nActual:\n{actual}"

let tests = [
    ("MIR optimize fixed point CSE after copy prop", testCseAfterCopyPropFixpoint)
]
