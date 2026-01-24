// PassTimingTests.fs - Unit tests for compiler pass timing tracing
//
// Ensures compiler pass timing records are produced during compilation.

module PassTimingTests

open System

type TestResult = Result<unit, string>

let testPassTimingsRecorded () : TestResult =
    match CompilerLibrary.buildStdlibWithCache { Enabled = false; CompilerKey = "" } None with
    | Error err -> Error $"Stdlib build failed: {err}"
    | Ok stdlib ->
        let timings = System.Collections.Generic.List<CompilerLibrary.PassTiming>()
        let record (timing: CompilerLibrary.PassTiming) : unit =
            timings.Add timing

        let request : CompilerLibrary.CompileRequest = {
            Context = CompilerLibrary.StdlibOnly stdlib
            Mode = CompilerLibrary.TestExpression
            Source = "1"
            SourceFile = "pass-timing-tests"
            AllowInternal = false
            Verbosity = 0
            Options = CompilerLibrary.defaultOptions
            PassTimingRecorder = Some record
        }

        let report = CompilerLibrary.compile request
        match report.Result with
        | Error err -> Error $"Compilation failed: {err}"
        | Ok _ ->
            let passNames =
                timings
                |> Seq.map (fun timing -> timing.Pass)
                |> Set.ofSeq

            let required =
                Set.ofList [
                    "Parse"
                    "Type Checking"
                    "AST -> ANF"
                    "ANF Optimizations"
                    "ANF Inlining"
                    "Reference Count Insertion"
                    "Print Insertion"
                    "Tail Call Detection"
                    "ANF -> MIR"
                    "SSA Construction"
                    "MIR Optimizations"
                    "MIR -> LIR"
                    "LIR Peephole"
                    "Register Allocation"
                    "Function Tree Shaking"
                    "Code Generation"
                    "ARM64 Encoding"
                    "Binary Generation"
                ]

            let missing =
                Set.difference required passNames
                |> Set.toList

            match missing with
            | [] -> Ok ()
            | _ ->
                let details = String.concat ", " missing
                Error $"Missing pass timings: {details}"

let tests = [
    ("pass timings recorded", testPassTimingsRecorded)
]
