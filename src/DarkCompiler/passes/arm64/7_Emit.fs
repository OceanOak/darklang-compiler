// 7_Emit.fs - ARM64 Emission (Encoding + Binary Generation)
//
// Resolves symbolic data labels into literal pools, encodes ARM64 instructions,
// and produces a platform-specific binary in a single pass.

module ARM64_Emit

type EmitResult = {
    MachineCode: ARM64.MachineCode array
    Binary: byte array
}

/// Resolve label refs, encode machine code, and generate a binary for the target OS
let emitBinary
    (program: CodeGen.GeneratedProgram)
    (os: Platform.OS)
    (enableLeakCheck: bool)
    (prepareCachedChunk:
        (ARM64Symbolic.Instr list
            -> (unit -> ARM64_Encoding.PreparedChunk)
            -> ARM64_Encoding.PreparedChunk) option)
    (prepareCachedChunkGroup:
        (ARM64Symbolic.Instr list list
            -> (unit -> ARM64_Encoding.PreparedChunk)
            -> ARM64_Encoding.PreparedChunk) option)
    (phaseRecorder: (string -> float -> unit) option)
    : EmitResult =
    let startPhase () =
        phaseRecorder |> Option.map (fun _ -> System.Diagnostics.Stopwatch.StartNew())
    let recordPhase name timer =
        match phaseRecorder, timer with
        | Some record, Some (timer: System.Diagnostics.Stopwatch) ->
            timer.Stop()
            record name timer.Elapsed.TotalMilliseconds
        | _ -> ()

    let prepareTimer = startPhase ()
    let preparedChunks =
        program
        |> CodeGen.generatedProgramChunks
        |> List.map (fun chunk ->
            let preparePart instructions =
                let prepare () =
                    ARM64_Encoding.prepareSymbolicChunk instructions
                match prepareCachedChunk with
                | Some cache when chunk.ReusableAcrossCompilations ->
                    cache instructions prepare
                | _ -> prepare ()
            match chunk.InstructionParts with
            | [instructions] -> preparePart instructions
            | instructionParts ->
                let prepareGroup () =
                    instructionParts
                    |> List.map preparePart
                    |> ARM64_Encoding.combinePreparedChunks
                match prepareCachedChunkGroup with
                | Some cache when chunk.ReusableAcrossCompilations ->
                    cache instructionParts prepareGroup
                | _ -> prepareGroup ())
    recordPhase "ARM64 Emit Chunk Preparation" prepareTimer

    let poolTimer = startPhase ()
    let (stringPool, floatPool) =
        preparedChunks
        |> Seq.collect (fun chunk -> chunk.PoolLabelRefs)
        |> ARM64_Resolve.collectPoolsFromLabelRefs
    recordPhase "ARM64 Emit Pool Collection" poolTimer

    let encodingTimer = startPhase ()
    let machineCode =
        ARM64_Encoding.encodePreparedChunksWithPools
            preparedChunks
            stringPool
            floatPool
            os
            enableLeakCheck
    recordPhase "ARM64 Emit Encoding" encodingTimer

    let binaryTimer = startPhase ()
    let binary =
        match os with
        | Platform.MacOS ->
            Binary_Generation_MachO.createExecutableWithPools machineCode stringPool floatPool enableLeakCheck
        | Platform.Linux ->
            Binary_Generation_ELF.createExecutableWithPools machineCode stringPool floatPool enableLeakCheck
    recordPhase "ARM64 Emit Binary Assembly" binaryTimer
    { MachineCode = machineCode; Binary = binary }
