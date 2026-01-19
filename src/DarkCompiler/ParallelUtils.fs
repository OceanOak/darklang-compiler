// ParallelUtils.fs - Sequential helpers for list/result transforms
//
// Provides order-preserving sequential mapping helpers for compiler passes.

module ParallelUtils

/// Map over a list sequentially
let mapListSequential (items: 'a list) (f: 'a -> 'b) : 'b list =
    items |> List.map f

/// Map over a list sequentially, returning first error
let mapResultsParallel (f: 'a -> Result<'b, string>) (items: 'a list) : Result<'b list, string> =
    let rec loop acc remaining =
        match remaining with
        | [] -> Ok (List.rev acc)
        | item :: rest ->
            match f item with
            | Error err -> Error err
            | Ok result -> loop (result :: acc) rest
    loop [] items
