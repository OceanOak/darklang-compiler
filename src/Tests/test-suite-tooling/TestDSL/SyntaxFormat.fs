// SyntaxFormat.fs - Canonical Dark syntax fixture parser.
module TestDSL.SyntaxFormat
open TestDSL.Common

type SyntaxTest = { Name: string; Source: string; ExpectedError: string option; ExpectedFormat: string option; Roundtrip: bool; SourceFile: string }

let private caseFromSections path (sections: Section list) =
    let values = Map.ofList sections
    let allowedSections = Set.ofList [ "NAME"; "SOURCE"; "EXPECT-ERROR"; "EXPECTED"; "ROUNDTRIP" ]
    let unknownSection =
        sections
        |> List.map fst
        |> List.tryFind (fun name -> not (Set.contains name allowedSections))
    let required name =
        match Map.tryFind name values with
        | Some value -> Ok (value.Trim())
        | None -> Error $"Missing required syntax section: {name}"
    match unknownSection, required "NAME", required "SOURCE" with
    | Some name, _, _ -> Error $"Unknown syntax section: {name}"
    | None, Ok name, Ok source ->
        let expectedError = Map.tryFind "EXPECT-ERROR" values |> Option.map (fun value -> value.Trim())
        let expectedFormat = Map.tryFind "EXPECTED" values |> Option.map normalizeLineEndings
        let roundtrip = Map.containsKey "ROUNDTRIP" values
        if expectedError.IsSome && (expectedFormat.IsSome || roundtrip) then Error "EXPECT-ERROR cannot be combined with formatting or roundtrip"
        else Ok { Name = name; Source = normalizeLineEndings source; ExpectedError = expectedError; ExpectedFormat = expectedFormat; Roundtrip = roundtrip; SourceFile = path }
    | None, Error err, _ | None, _, Error err -> Error err

let parseSyntaxFileContent path content =
    let rec groups completed current remaining =
        match remaining with
        | [] -> Ok (List.rev (if List.isEmpty current then completed else List.rev current :: completed))
        | (("NAME", _) as section) :: rest ->
            if List.isEmpty current then groups completed [section] rest else groups (List.rev current :: completed) [section] rest
        | section :: rest ->
            if List.isEmpty current then Error $"Syntax case must start with NAME, found {fst section}" else groups completed (section :: current) rest
    let sections = parseSections (normalizeLineEndings content)
    if List.isEmpty sections then Error "Syntax fixture contains no sections"
    else groups [] [] sections |> Result.bind (fun cases -> cases |> List.fold (fun state sections -> state |> Result.bind (fun parsed -> caseFromSections path sections |> Result.map (fun test -> parsed @ [test]))) (Ok []))
