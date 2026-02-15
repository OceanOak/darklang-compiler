// ASTPrettyPrinter.fs - Pretty printers for Darklang source syntaxes.
//
// Formats the shared AST into compiler syntax or interpreter syntax.

module ASTPrettyPrinter

open AST

type Syntax =
    | CompilerSyntax
    | InterpreterSyntax

let private escapeStringContent (input: string) : string =
    input
    |> String.collect (fun c ->
        match c with
        | '\\' -> "\\\\"
        | '"' -> "\\\""
        | '\n' -> "\\n"
        | '\r' -> "\\r"
        | '\t' -> "\\t"
        | '\000' -> "\\0"
        | _ -> string c)

let private escapeCharContent (input: string) : string =
    input
    |> String.collect (fun c ->
        match c with
        | '\\' -> "\\\\"
        | '\'' -> "\\'"
        | '\n' -> "\\n"
        | '\r' -> "\\r"
        | '\t' -> "\\t"
        | '\000' -> "\\0"
        | _ -> string c)

let private formatFloatLiteral (value: float) : string =
    let raw = value.ToString("R", System.Globalization.CultureInfo.InvariantCulture)
    let containsLetters = raw |> Seq.exists System.Char.IsLetter
    if containsLetters || raw.Contains(".") || raw.Contains("E") || raw.Contains("e") then
        raw
    else
        $"{raw}.0"

let rec private formatType (typ: Type) : string =
    match typ with
    | TInt8 -> "Int8"
    | TInt16 -> "Int16"
    | TInt32 -> "Int32"
    | TInt64 -> "Int64"
    | TUInt8 -> "UInt8"
    | TUInt16 -> "UInt16"
    | TUInt32 -> "UInt32"
    | TUInt64 -> "UInt64"
    | TBool -> "Bool"
    | TFloat64 -> "Float"
    | TString -> "String"
    | TBytes -> "Bytes"
    | TChar -> "Char"
    | TUnit -> "Unit"
    | TRawPtr -> "RawPtr"
    | TVar name -> name
    | TList elemType -> $"List<{formatType elemType}>"
    | TDict (keyType, valueType) -> $"Dict<{formatType keyType}, {formatType valueType}>"
    | TTuple elemTypes ->
        let elemText = elemTypes |> List.map formatType |> String.concat ", "
        $"({elemText})"
    | TRecord (name, []) -> name
    | TRecord (name, typeArgs) ->
        let argsText = typeArgs |> List.map formatType |> String.concat ", "
        $"{name}<{argsText}>"
    | TSum (name, []) -> name
    | TSum (name, typeArgs) ->
        let argsText = typeArgs |> List.map formatType |> String.concat ", "
        $"{name}<{argsText}>"
    | TFunction (paramTypes, returnType) ->
        let paramsText = paramTypes |> List.map formatType |> String.concat ", "
        $"({paramsText}) -> {formatType returnType}"

let private formatBinOp (op: BinOp) : string =
    match op with
    | Add -> "+"
    | Sub -> "-"
    | Mul -> "*"
    | Div -> "/"
    | Mod -> "%"
    | Shl -> "<<"
    | Shr -> ">>"
    | BitAnd -> "&"
    | BitOr -> "|||"
    | BitXor -> "^"
    | StringConcat -> "++"
    | Eq -> "=="
    | Neq -> "!="
    | Lt -> "<"
    | Gt -> ">"
    | Lte -> "<="
    | Gte -> ">="
    | And -> "&&"
    | Or -> "||"

let private formatUnaryOp (op: UnaryOp) : string =
    match op with
    | Neg -> "-"
    | Not -> "!"
    | BitNot -> "~~~"

let rec private isAtomicExpr (expr: Expr) : bool =
    match expr with
    | UnitLiteral
    | Int64Literal _
    | Int8Literal _
    | Int16Literal _
    | Int32Literal _
    | UInt8Literal _
    | UInt16Literal _
    | UInt32Literal _
    | UInt64Literal _
    | BoolLiteral _
    | StringLiteral _
    | CharLiteral _
    | FloatLiteral _
    | InterpolatedString _
    | Var _
    | FuncRef _
    | TupleLiteral _
    | RecordLiteral _
    | ListLiteral _
    | Constructor (_, _, None) -> true
    | TupleAccess (tupleExpr, _) -> isAtomicExpr tupleExpr
    | RecordAccess (recordExpr, _) -> isAtomicExpr recordExpr
    | _ -> false

let private parenthesizeIfNeeded (expr: Expr) (text: string) : string =
    if isAtomicExpr expr then text else $"({text})"

let private parenthesizeTupleBaseIfNeeded (expr: Expr) (text: string) : string =
    match expr with
    | TupleAccess _ -> $"({text})"
    | _ -> parenthesizeIfNeeded expr text

let rec private formatPattern (syntax: Syntax) (pattern: Pattern) : string =
    match pattern with
    | PUnit -> "()"
    | PWildcard -> "_"
    | PVar name -> name
    | PConstructor (name, None) -> name
    | PConstructor (name, Some payload) ->
        let payloadText = formatPattern syntax payload
        match syntax with
        | CompilerSyntax -> $"{name}({payloadText})"
        | InterpreterSyntax ->
            if payloadText.StartsWith "(" then $"{name} {payloadText}" else $"{name} {payloadText}"
    | PInt64 n ->
        match syntax with
        | CompilerSyntax -> $"{n}"
        | InterpreterSyntax -> $"{n}L"
    | PInt8Literal n ->
        match syntax with
        | CompilerSyntax -> $"{n}y"
        | InterpreterSyntax -> Crash.crash "Interpreter syntax does not support Int8 literals"
    | PInt16Literal n ->
        match syntax with
        | CompilerSyntax -> $"{n}s"
        | InterpreterSyntax -> Crash.crash "Interpreter syntax does not support Int16 literals"
    | PInt32Literal n ->
        match syntax with
        | CompilerSyntax -> $"{n}l"
        | InterpreterSyntax -> Crash.crash "Interpreter syntax does not support Int32 literals"
    | PUInt8Literal n ->
        match syntax with
        | CompilerSyntax -> $"{n}uy"
        | InterpreterSyntax -> Crash.crash "Interpreter syntax does not support UInt8 literals"
    | PUInt16Literal n ->
        match syntax with
        | CompilerSyntax -> $"{n}us"
        | InterpreterSyntax -> Crash.crash "Interpreter syntax does not support UInt16 literals"
    | PUInt32Literal n ->
        match syntax with
        | CompilerSyntax -> $"{n}ul"
        | InterpreterSyntax -> Crash.crash "Interpreter syntax does not support UInt32 literals"
    | PUInt64Literal n ->
        match syntax with
        | CompilerSyntax -> $"{n}UL"
        | InterpreterSyntax -> Crash.crash "Interpreter syntax does not support UInt64 literals"
    | PBool b -> if b then "true" else "false"
    | PString s -> $"\"{escapeStringContent s}\""
    | PFloat f -> formatFloatLiteral f
    | PTuple patterns ->
        let parts = patterns |> List.map (formatPattern syntax) |> String.concat ", "
        $"({parts})"
    | PRecord (typeName, fields) ->
        let fieldsText =
            fields
            |> List.map (fun (name, fieldPattern) -> $"{name} = {formatPattern syntax fieldPattern}")
            |> String.concat ", "
        $"{typeName} {{ {fieldsText} }}"
    | PList patterns ->
        let separator =
            match syntax with
            | CompilerSyntax -> ", "
            | InterpreterSyntax -> "; "
        let items = patterns |> List.map (formatPattern syntax) |> String.concat separator
        $"[{items}]"
    | PListCons (head, tail) ->
        let separator =
            match syntax with
            | CompilerSyntax -> ", "
            | InterpreterSyntax -> "; "
        let headText = head |> List.map (formatPattern syntax) |> String.concat separator
        if headText = "" then
            $"[...{formatPattern syntax tail}]"
        else
            $"[{headText}{separator}...{formatPattern syntax tail}]"

let rec private formatExpr (syntax: Syntax) (expr: Expr) : string =
    let formatAppArg (arg: Expr) : string =
        let argText = formatExpr syntax arg
        parenthesizeIfNeeded arg argText

    match expr with
    | UnitLiteral -> "()"
    | Int64Literal n ->
        match syntax with
        | CompilerSyntax -> $"{n}"
        | InterpreterSyntax -> $"{n}L"
    | Int8Literal n ->
        match syntax with
        | CompilerSyntax -> $"{n}y"
        | InterpreterSyntax -> Crash.crash "Interpreter syntax does not support Int8 literals"
    | Int16Literal n ->
        match syntax with
        | CompilerSyntax -> $"{n}s"
        | InterpreterSyntax -> Crash.crash "Interpreter syntax does not support Int16 literals"
    | Int32Literal n ->
        match syntax with
        | CompilerSyntax -> $"{n}l"
        | InterpreterSyntax -> Crash.crash "Interpreter syntax does not support Int32 literals"
    | UInt8Literal n ->
        match syntax with
        | CompilerSyntax -> $"{n}uy"
        | InterpreterSyntax -> Crash.crash "Interpreter syntax does not support UInt8 literals"
    | UInt16Literal n ->
        match syntax with
        | CompilerSyntax -> $"{n}us"
        | InterpreterSyntax -> Crash.crash "Interpreter syntax does not support UInt16 literals"
    | UInt32Literal n ->
        match syntax with
        | CompilerSyntax -> $"{n}ul"
        | InterpreterSyntax -> Crash.crash "Interpreter syntax does not support UInt32 literals"
    | UInt64Literal n ->
        match syntax with
        | CompilerSyntax -> $"{n}UL"
        | InterpreterSyntax -> Crash.crash "Interpreter syntax does not support UInt64 literals"
    | BoolLiteral b -> if b then "true" else "false"
    | StringLiteral s -> $"\"{escapeStringContent s}\""
    | CharLiteral c -> $"'{escapeCharContent c}'"
    | FloatLiteral f -> formatFloatLiteral f
    | InterpolatedString parts ->
        match syntax with
        | InterpreterSyntax -> Crash.crash "Interpreter syntax does not support interpolated strings"
        | CompilerSyntax ->
            let partsText =
                parts
                |> List.map (function
                    | StringText t -> escapeStringContent t
                    | StringExpr e -> $"{{{formatExpr syntax e}}}")
                |> String.concat ""
            $"$\"{partsText}\""
    | BinOp (op, left, right) ->
        let leftText = parenthesizeIfNeeded left (formatExpr syntax left)
        let rightText = parenthesizeIfNeeded right (formatExpr syntax right)
        $"{leftText} {formatBinOp op} {rightText}"
    | UnaryOp (op, inner) ->
        let innerText = parenthesizeIfNeeded inner (formatExpr syntax inner)
        $"{formatUnaryOp op}{innerText}"
    | Let (name, value, body) ->
        $"let {name} = {formatExpr syntax value} in {formatExpr syntax body}"
    | Var name -> name
    | If (cond, thenBranch, elseBranch) ->
        $"if {formatExpr syntax cond} then {formatExpr syntax thenBranch} else {formatExpr syntax elseBranch}"
    | Call (funcName, args) ->
        match syntax with
        | CompilerSyntax ->
            let argsText = args |> List.map (formatExpr syntax) |> String.concat ", "
            $"{funcName}({argsText})"
        | InterpreterSyntax ->
            if List.isEmpty args then
                funcName
            else
                let argsText = args |> List.map formatAppArg |> String.concat " "
                $"{funcName} {argsText}"
    | TypeApp (funcName, typeArgs, args) ->
        let typeArgsText = typeArgs |> List.map formatType |> String.concat ", "
        match syntax with
        | CompilerSyntax ->
            let argsText = args |> List.map (formatExpr syntax) |> String.concat ", "
            $"{funcName}<{typeArgsText}>({argsText})"
        | InterpreterSyntax ->
            let head = $"{funcName}<{typeArgsText}>"
            if List.isEmpty args then
                head
            else
                let argsText = args |> List.map formatAppArg |> String.concat " "
                $"{head} {argsText}"
    | TupleLiteral elements ->
        let elementsText = elements |> List.map (formatExpr syntax) |> String.concat ", "
        $"({elementsText})"
    | TupleAccess (tupleExpr, index) ->
        let tupleText = parenthesizeTupleBaseIfNeeded tupleExpr (formatExpr syntax tupleExpr)
        $"{tupleText}.{index}"
    | RecordLiteral (typeName, fields) ->
        let fieldsText =
            fields
            |> List.map (fun (name, value) -> $"{name} = {formatExpr syntax value}")
            |> String.concat ", "
        $"{typeName} {{ {fieldsText} }}"
    | RecordUpdate (recordExpr, updates) ->
        let recordText = formatExpr syntax recordExpr
        let updatesText =
            updates
            |> List.map (fun (name, value) -> $"{name} = {formatExpr syntax value}")
            |> String.concat ", "
        $"{{ {recordText} with {updatesText} }}"
    | RecordAccess (recordExpr, fieldName) ->
        let recordText = parenthesizeIfNeeded recordExpr (formatExpr syntax recordExpr)
        $"{recordText}.{fieldName}"
    | Constructor (typeName, variantName, payload) ->
        let fullName =
            if typeName = "" then variantName else $"{typeName}.{variantName}"
        match payload with
        | None -> fullName
        | Some payloadExpr ->
            let payloadText = formatAppArg payloadExpr
            match syntax with
            | CompilerSyntax -> $"{fullName}({formatExpr syntax payloadExpr})"
            | InterpreterSyntax -> $"{fullName} {payloadText}"
    | Match (scrutinee, cases) ->
        let scrutineeText = formatExpr syntax scrutinee
        let caseText =
            cases
            |> List.map (fun case ->
                let patternsText =
                    case.Patterns
                    |> NonEmptyList.toList
                    |> List.map (formatPattern syntax)
                    |> String.concat " | "
                let guardText =
                    match case.Guard with
                    | None -> ""
                    | Some guardExpr -> $" when {formatExpr syntax guardExpr}"
                $"| {patternsText}{guardText} -> {formatExpr syntax case.Body}")
            |> String.concat " "
        $"match {scrutineeText} with {caseText}"
    | ListLiteral elements ->
        let separator =
            match syntax with
            | CompilerSyntax -> ", "
            | InterpreterSyntax -> "; "
        let elementsText = elements |> List.map (formatExpr syntax) |> String.concat separator
        $"[{elementsText}]"
    | ListCons (head, tail) ->
        let separator =
            match syntax with
            | CompilerSyntax -> ", "
            | InterpreterSyntax -> "; "
        let headText = head |> List.map (formatExpr syntax) |> String.concat separator
        if headText = "" then
            $"[...{formatExpr syntax tail}]"
        else
            $"[{headText}{separator}...{formatExpr syntax tail}]"
    | Lambda (parameters, body) ->
        match syntax with
        | CompilerSyntax ->
            let paramsText =
                parameters
                |> List.map (fun (name, typ) -> $"{name}: {formatType typ}")
                |> String.concat ", "
            $"({paramsText}) => {formatExpr syntax body}"
        | InterpreterSyntax ->
            if List.isEmpty parameters then
                Crash.crash "Cannot render zero-argument lambda in interpreter syntax"
            else
                let paramsText = parameters |> List.map fst |> String.concat " "
                $"fun {paramsText} -> {formatExpr syntax body}"
    | Apply (funcExpr, args) ->
        match syntax with
        | CompilerSyntax ->
            let funcText = parenthesizeIfNeeded funcExpr (formatExpr syntax funcExpr)
            let argsText = args |> List.map (formatExpr syntax) |> String.concat ", "
            $"{funcText}({argsText})"
        | InterpreterSyntax ->
            let funcText = parenthesizeIfNeeded funcExpr (formatExpr syntax funcExpr)
            if List.isEmpty args then
                funcText
            else
                let argsText = args |> List.map formatAppArg |> String.concat " "
                $"{funcText} {argsText}"
    | FuncRef funcName -> funcName
    | Closure (funcName, captures) ->
        let capturesText = captures |> List.map (formatExpr syntax) |> String.concat ", "
        $"Closure({funcName}, [{capturesText}])"

let private formatFunctionDef (syntax: Syntax) (funcDef: FunctionDef) : string =
    match syntax with
    | CompilerSyntax ->
        let typeParamsText =
            if List.isEmpty funcDef.TypeParams then ""
            else
                let joined = String.concat ", " funcDef.TypeParams
                $"<{joined}>"
        let paramsText =
            funcDef.Params
            |> List.map (fun (name, typ) -> $"{name}: {formatType typ}")
            |> String.concat ", "
        $"def {funcDef.Name}{typeParamsText}({paramsText}) : {formatType funcDef.ReturnType} = {formatExpr syntax funcDef.Body}"
    | InterpreterSyntax ->
        let bodyText = formatExpr syntax funcDef.Body
        if List.isEmpty funcDef.Params then
            $"let {funcDef.Name} = {bodyText}"
        else
            let paramNames = funcDef.Params |> List.map fst
            let lambdaText =
                List.foldBack (fun name acc -> $"fun {name} -> {acc}") paramNames bodyText
            $"let {funcDef.Name} = {lambdaText}"

let private formatTypeDef (typeDef: TypeDef) : string =
    let formatTypeParams (typeParams: string list) : string =
        if List.isEmpty typeParams then ""
        else
            let joined = String.concat ", " typeParams
            $"<{joined}>"

    match typeDef with
    | RecordDef (name, typeParams, fields) ->
        let fieldsText =
            fields
            |> List.map (fun (fieldName, fieldType) -> $"{fieldName}: {formatType fieldType}")
            |> String.concat ", "
        $"type {name}{formatTypeParams typeParams} = {{ {fieldsText} }}"
    | SumTypeDef (name, typeParams, variants) ->
        let variantsText =
            variants
            |> List.map (fun variant ->
                match variant.Payload with
                | None -> variant.Name
                | Some payloadType -> $"{variant.Name} of {formatType payloadType}")
            |> String.concat " | "
        $"type {name}{formatTypeParams typeParams} = {variantsText}"
    | TypeAlias (name, typeParams, targetType) ->
        $"type {name}{formatTypeParams typeParams} = {formatType targetType}"

let private formatTopLevel (syntax: Syntax) (topLevel: TopLevel) : string =
    match topLevel with
    | FunctionDef funcDef -> formatFunctionDef syntax funcDef
    | TypeDef typeDef -> formatTypeDef typeDef
    | Expression expr -> formatExpr syntax expr

let formatProgram (syntax: Syntax) (Program items: Program) : string =
    items |> List.map (formatTopLevel syntax) |> String.concat "\n"
