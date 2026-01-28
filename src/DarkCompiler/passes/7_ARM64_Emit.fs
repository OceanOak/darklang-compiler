// 7_ARM64_Emit.fs - ARM64 Emission (Encoding + Binary Generation)
//
// Resolves symbolic data labels into literal pools, encodes ARM64 instructions,
// and produces a platform-specific binary in a single pass.

module ARM64_Emit

open System.Diagnostics

type EmitResult = {
    MachineCode: ARM64.MachineCode list
    Binary: byte array
}

let private timePhase
    (recorder: (string -> float -> unit) option)
    (phase: string)
    (f: unit -> 'a)
    : 'a =
    match recorder with
    | None -> f ()
    | Some record ->
        let sw = Stopwatch.StartNew()
        let result = f ()
        let elapsedMs = sw.Elapsed.TotalMilliseconds
        record phase elapsedMs
        result

/// Resolve label refs, encode machine code, and generate a binary for the target OS
let emitBinary
    (instructions: ARM64Symbolic.Instr list)
    (os: Platform.OS)
    (enableLeakCheck: bool)
    (microTimingRecorder: (string -> float -> unit) option)
    : Result<EmitResult, string> =
    let resolved =
        timePhase microTimingRecorder "resolve" (fun () ->
            ARM64_Resolve.resolve instructions)
    let machineCode =
        timePhase microTimingRecorder "encode" (fun () ->
            ARM64_Encoding.encodeAllWithPools
                resolved.Instructions
                resolved.StringPool
                resolved.FloatPool
                os
                enableLeakCheck
                microTimingRecorder)
    let binary =
        let formatName = match os with | Platform.MacOS -> "Mach-O" | Platform.Linux -> "ELF"
        timePhase microTimingRecorder $"binary ({formatName})" (fun () ->
            match os with
            | Platform.MacOS ->
                Binary_Generation_MachO.createExecutableWithPools machineCode resolved.StringPool resolved.FloatPool enableLeakCheck microTimingRecorder
            | Platform.Linux ->
                Binary_Generation_ELF.createExecutableWithPools machineCode resolved.StringPool resolved.FloatPool enableLeakCheck microTimingRecorder)
    Ok { MachineCode = machineCode; Binary = binary }
