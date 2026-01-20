// Trace.fs - Lightweight tracing helpers for compiler/test instrumentation
//
// Emits trace events when enabled via an environment variable.

module Trace

open System
open Output

let private traceEnvVar = "DARK_TRACE_COMPILER"

let isEnabled () : bool =
    match Environment.GetEnvironmentVariable traceEnvVar with
    | null -> false
    | value -> value.Trim() <> "" && value <> "0"

let private formatFields (fields: (string * string) list) : string =
    fields
    |> List.map (fun (key, value) -> $"{key}={value}")
    |> String.concat " "

let formatEvent (name: string) (fields: (string * string) list) : string =
    let fieldText = formatFields fields
    if fieldText = "" then
        $"TRACE event={name}"
    else
        $"TRACE event={name} {fieldText}"

let emit (name: string) (fields: (string * string) list) : unit =
    if isEnabled () then
        eprintln (formatEvent name fields)
