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

let private isComparisonOp (op: BinOp) : bool =
    match op with
    | Eq
    | Neq
    | Lt
    | Gt
    | Lte
    | Gte -> true
    | _ -> false

let private binOpPrecedence (op: BinOp) : int =
    match op with
    | Or -> 1
    | And -> 2
    | BitOr -> 3
    | BitXor -> 4
    | BitAnd -> 5
    | Eq
    | Neq
    | Lt
    | Gt
    | Lte
    | Gte -> 6
    | Shl
    | Shr -> 7
    | Add
    | Sub
    | StringConcat -> 8
    | Mul
    | Div
    | Mod -> 9

let private shouldParenthesizeBinChild (parentOp: BinOp) (isLeftChild: bool) (childOp: BinOp) : bool =
    let parentPrec = binOpPrecedence parentOp
    let childPrec = binOpPrecedence childOp
    if childPrec < parentPrec then
        true
    elif childPrec > parentPrec then
        false
    elif isComparisonOp parentOp then
        true
    else
        // Operators are left-associative: left child can omit equal-precedence
        // parentheses, right child needs them to preserve tree shape.
        not isLeftChild

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
    | Call _
    | TypeApp _
    | Apply _
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
        | InterpreterSyntax -> $"{n}y"
    | PInt16Literal n ->
        match syntax with
        | CompilerSyntax -> $"{n}s"
        | InterpreterSyntax -> $"{n}s"
    | PInt32Literal n ->
        match syntax with
        | CompilerSyntax -> $"{n}l"
        | InterpreterSyntax -> $"{n}l"
    | PUInt8Literal n ->
        match syntax with
        | CompilerSyntax -> $"{n}uy"
        | InterpreterSyntax -> $"{n}uy"
    | PUInt16Literal n ->
        match syntax with
        | CompilerSyntax -> $"{n}us"
        | InterpreterSyntax -> $"{n}us"
    | PUInt32Literal n ->
        match syntax with
        | CompilerSyntax -> $"{n}ul"
        | InterpreterSyntax -> $"{n}ul"
    | PUInt64Literal n ->
        match syntax with
        | CompilerSyntax -> $"{n}UL"
        | InterpreterSyntax -> $"{n}UL"
    | PBool b -> if b then "true" else "false"
    | PString s -> $"\"{escapeStringContent s}\""
    | PChar c -> $"'{escapeCharContent c}'"
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
    let isNegativeNumericLiteral (arg: Expr) : bool =
        match arg with
        | Int64Literal n -> n < 0L
        | Int8Literal n -> n < 0y
        | Int16Literal n -> n < 0s
        | Int32Literal n -> n < 0l
        | FloatLiteral f ->
            // Keep -0.0 wrapped as well; it is lexically ambiguous in application position.
            System.BitConverter.DoubleToInt64Bits(f) < 0L
        | _ -> false

    let formatAppArg (arg: Expr) : string =
        let argText = formatExpr syntax arg
        match syntax with
        | CompilerSyntax ->
            parenthesizeIfNeeded arg argText
        | InterpreterSyntax ->
            match arg with
            | _ when isNegativeNumericLiteral arg -> $"({argText})"
            | Constructor (_, _, None) -> $"({argText})"
            | TupleLiteral _ -> $"({argText})"
            | Call _
            | TypeApp _
            | Apply _ -> $"({argText})"
            | _ -> parenthesizeIfNeeded arg argText

    let rec formatInterpreterAppArgs (args: Expr list) : string list =
        match args with
        | [] -> []
        | [lastArg] -> [formatAppArg lastArg]
        | currentArg :: ((UnitLiteral as nextArg) :: restArgs) ->
            // `f x ()` is ambiguous with zero-arg calls (`x()`).
            // Parenthesize the preceding argument to preserve argument boundaries.
            $"({formatAppArg currentArg})"
            :: (formatInterpreterAppArgs (nextArg :: restArgs))
        | currentArg :: ((TupleLiteral _ as nextArg) :: restArgs) ->
            // `f g (a, b)` can be reparsed as applying `g` to tuple elements.
            // Parenthesize the preceding argument to keep tuple as a separate argument.
            $"({formatAppArg currentArg})"
            :: (formatInterpreterAppArgs (nextArg :: restArgs))
        | currentArg :: restArgs ->
            formatAppArg currentArg :: formatInterpreterAppArgs restArgs

    match expr with
    | UnitLiteral -> "()"
    | Int64Literal n ->
        match syntax with
        | CompilerSyntax -> $"{n}"
        | InterpreterSyntax -> $"{n}L"
    | Int8Literal n ->
        match syntax with
        | CompilerSyntax -> $"{n}y"
        | InterpreterSyntax -> $"{n}y"
    | Int16Literal n ->
        match syntax with
        | CompilerSyntax -> $"{n}s"
        | InterpreterSyntax -> $"{n}s"
    | Int32Literal n ->
        match syntax with
        | CompilerSyntax -> $"{n}l"
        | InterpreterSyntax -> $"{n}l"
    | UInt8Literal n ->
        match syntax with
        | CompilerSyntax -> $"{n}uy"
        | InterpreterSyntax -> $"{n}uy"
    | UInt16Literal n ->
        match syntax with
        | CompilerSyntax -> $"{n}us"
        | InterpreterSyntax -> $"{n}us"
    | UInt32Literal n ->
        match syntax with
        | CompilerSyntax -> $"{n}ul"
        | InterpreterSyntax -> $"{n}ul"
    | UInt64Literal n ->
        match syntax with
        | CompilerSyntax -> $"{n}UL"
        | InterpreterSyntax -> $"{n}UL"
    | BoolLiteral b -> if b then "true" else "false"
    | StringLiteral s -> $"\"{escapeStringContent s}\""
    | CharLiteral c -> $"'{escapeCharContent c}'"
    | FloatLiteral f -> formatFloatLiteral f
    | InterpolatedString parts ->
        let partsText =
            parts
            |> List.map (function
                | StringText t -> escapeStringContent t
                | StringExpr e -> $"{{{formatExpr syntax e}}}")
            |> String.concat ""
        $"$\"{partsText}\""
    | BinOp (op, left, right) ->
        let formatChild (isLeftChild: bool) (child: Expr) : string =
            let childText = formatExpr syntax child
            match child with
            | BinOp (childOp, _, _) ->
                if shouldParenthesizeBinChild op isLeftChild childOp then
                    $"({childText})"
                else
                    childText
            | _ -> parenthesizeIfNeeded child childText
        let leftCanConsumeNegativeNumericArg (expr: Expr) : bool =
            match expr with
            | Var funcName when funcName.Contains "." -> true
            | Call _
            | TypeApp _
            | Apply _
            | Constructor (_, _, None) -> true
            | _ -> false
        let isNumericLiteralExpr (expr: Expr) : bool =
            match expr with
            | Int64Literal _
            | Int8Literal _
            | Int16Literal _
            | Int32Literal _
            | UInt8Literal _
            | UInt16Literal _
            | UInt32Literal _
            | UInt64Literal _
            | FloatLiteral _ -> true
            | _ -> false
        let leftText = formatChild true left
        let rightTextBase = formatChild false right
        let rightText =
            match syntax, op with
            | InterpreterSyntax, Sub when leftCanConsumeNegativeNumericArg left && isNumericLiteralExpr right ->
                $"({rightTextBase})"
            | _ -> rightTextBase
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
                $"{funcName}()"
            else
                let argsText = args |> formatInterpreterAppArgs |> String.concat " "
                $"{funcName} {argsText}"
    | TypeApp (funcName, typeArgs, args) ->
        let typeArgsText = typeArgs |> List.map formatType |> String.concat ", "
        match syntax with
        | CompilerSyntax ->
            let argsText = args |> List.map (formatExpr syntax) |> String.concat ", "
            $"{funcName}<{typeArgsText}>({argsText})"
        | InterpreterSyntax ->
            let head = $"{funcName}<{typeArgsText}>"
            match args with
            | [] -> $"{head}()"
            | _ ->
                let argsText = args |> List.map (formatExpr syntax) |> String.concat ", "
                $"{head}({argsText})"
    | TupleLiteral elements ->
        let elementsText = elements |> List.map (formatExpr syntax) |> String.concat ", "
        $"({elementsText})"
    | TupleAccess (tupleExpr, index) ->
        let tupleBaseText = formatExpr syntax tupleExpr
        let tupleText =
            match syntax, tupleExpr with
            | InterpreterSyntax, (Call _ | TypeApp _ | Apply _) ->
                // In interpreter syntax, call application has no mandatory wrapping.
                // Parenthesize before postfix access so `.0` binds to the call result.
                $"({tupleBaseText})"
            | _ ->
                parenthesizeTupleBaseIfNeeded tupleExpr tupleBaseText
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
        let recordBaseText = formatExpr syntax recordExpr
        let recordText =
            match syntax, recordExpr with
            | InterpreterSyntax, (Call _ | TypeApp _ | Apply _) ->
                // Same ambiguity as tuple access: ensure `.field` applies to call result.
                $"({recordBaseText})"
            | _ ->
                parenthesizeIfNeeded recordExpr recordBaseText
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
        let formatCaseBody (body: Expr) : string =
            let bodyText = formatExpr syntax body
            match body with
            // Without parens, nested match case bars get parsed as outer cases.
            | Match _
            | Let _ -> $"({bodyText})"
            | _ -> bodyText
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
                $"| {patternsText}{guardText} -> {formatCaseBody case.Body}")
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
            match parameters, body with
            | [ (paramName, TBool) ], BinOp (And, Var varName, rightArg) when paramName = "$pipe_arg" && varName = "$pipe_arg" ->
                $"(&&) {formatAppArg rightArg}"
            | [ (paramName, TBool) ], BinOp (Or, Var varName, rightArg) when paramName = "$pipe_arg" && varName = "$pipe_arg" ->
                $"(||) {formatAppArg rightArg}"
            | _ ->
                let paramsText =
                    parameters
                    |> List.map (fun (name, typ) -> $"{name}: {formatType typ}")
                    |> String.concat ", "
                $"({paramsText}) => {formatExpr syntax body}"
        | InterpreterSyntax ->
            if List.isEmpty parameters then
                Crash.crash "Cannot render zero-argument lambda in interpreter syntax"
            else
                match parameters, body with
                | [ (paramName, TBool) ], BinOp (And, Var varName, rightArg) when paramName = "$pipe_arg" && varName = "$pipe_arg" ->
                    $"(&&) {formatAppArg rightArg}"
                | [ (paramName, TBool) ], BinOp (Or, Var varName, rightArg) when paramName = "$pipe_arg" && varName = "$pipe_arg" ->
                    $"(||) {formatAppArg rightArg}"
                | _ ->
                    let paramsText =
                        parameters
                        |> List.map (fun (name, typ) ->
                            match typ with
                            | TVar typeVar when typeVar.StartsWith "__interp_lambda_" -> name
                            | _ -> $"({name}: {formatType typ})")
                        |> String.concat " "
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
                let argsText = args |> formatInterpreterAppArgs |> String.concat " "
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
        let typeParamsText =
            if List.isEmpty funcDef.TypeParams then ""
            else
                let joined = String.concat ", " funcDef.TypeParams
                $"<{joined}>"
        let paramsText =
            funcDef.Params
            |> List.map (fun (name, typ) -> $"{name}: {formatType typ}")
            |> String.concat ", "
        $"let {funcDef.Name}{typeParamsText}({paramsText}) : {formatType funcDef.ReturnType} = {formatExpr syntax funcDef.Body}"

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
    let separator =
        match syntax with
        | CompilerSyntax -> "\n"
        | InterpreterSyntax -> "\n;\n"
    items |> List.map (formatTopLevel syntax) |> String.concat separator
