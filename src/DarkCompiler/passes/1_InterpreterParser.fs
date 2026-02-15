// 1_InterpreterParser.fs - Lexer and Parser for interpreter-style syntax
//
// Transforms Darklang interpreter-style source code (string) into the shared
// compiler Abstract Syntax Tree (AST).
//
// Lexer: Converts source string into tokens
// Parser: Recursive descent parser with operator precedence
//
// Operator precedence (specific to this parser):
// - Multiplication and division bind tighter than addition and subtraction
// - Operators are left-associative: "1 + 2 + 3" parses as "(1 + 2) + 3"
// - Parentheses for explicit grouping
//
// Example:
//   "2 + 3 * 4" → BinOp(Add, Int64Literal(2), BinOp(Mul, Int64Literal(3), Int64Literal(4)))

module InterpreterParser

open AST
open System.Numerics

/// Part of an interpolated string token
type InterpPart =
    | InterpText of string       // Literal text
    | InterpTokens of Token list // Tokens for an expression (will be parsed later)

/// Token types for lexer
and Token =
    | TInt64 of int64       // Default integer (Int64)
    | TInt8 of sbyte        // 8-bit signed: 1y
    | TInt16 of int16       // 16-bit signed: 1s
    | TInt32 of int32       // 32-bit signed: 1l
    | TUInt8 of byte        // 8-bit unsigned: 1uy
    | TUInt16 of uint16     // 16-bit unsigned: 1us
    | TUInt32 of uint32     // 32-bit unsigned: 1ul
    | TUInt64 of uint64     // 64-bit unsigned: 1UL
    | TFloat of float
    | TStringLit of string  // String literal token (named to avoid conflict with AST.TString type)
    | TCharLit of string    // Char literal: 'x' (stores UTF-8 string for EGC support)
    | TInterpString of InterpPart list  // Interpolated string: $"Hello {name}!"
    | TTrue
    | TFalse
    | TPlus
    | TPlusPlus    // ++ (string concatenation)
    | TMinus
    | TStar
    | TSlash
    | TLParen
    | TRParen
    | TLet
    | TIn
    | TIf          // if
    | TThen        // then
    | TElse        // else
    | TDef         // def (function definition)
    | TType        // type (type definition)
    | TCons        // :: (list cons pattern)
    | TColon       // : (type annotation)
    | TComma       // , (parameter separator)
    | TSemicolon   // ; (interpreter-style list separator)
    | TDot         // . (tuple/record access)
    | TLBrace      // { (record literal)
    | TRBrace      // } (record literal)
    | TBar         // | (sum type variant separator / pattern separator)
    | TOf          // of (sum type payload)
    | TMatch       // match (pattern matching)
    | TWith        // with (pattern matching)
    | TFun         // fun (interpreter-style lambda)
    | TArrow       // -> (pattern matching)
    | TUnderscore  // _ (wildcard pattern)
    | TWhen        // when (guard clause in pattern matching)
    | TLBracket    // [ (list literal)
    | TRBracket    // ] (list literal)
    | TEquals      // = (assignment in let)
    | TEqEq        // == (equality comparison)
    | TNeq         // !=
    | TLt          // <
    | TGt          // >
    | TLte         // <=
    | TGte         // >=
    | TAnd         // &&
    | TOr          // ||
    | TNot         // !
    | TPipe        // |> (pipe operator)
    | TDotDotDot    // ... (rest pattern in lists)
    | TPercent     // % (modulo)
    | TShl         // << (left shift)
    | TShr         // >> (right shift)
    | TBitAnd      // & (bitwise and)
    | TBitOr       // ||| (bitwise or)
    | TBitXor      // ^ (bitwise xor)
    | TBitNot      // ~~~ (bitwise not)
    | TIdent of string
    | TEOF

/// Lexer: convert string to list of tokens
let lex (input: string) : Result<Token list, string> =
    let rec lexHelper (chars: char list) (acc: Token list) : Result<Token list, string> =
        match chars with
        | [] -> Ok (List.rev (TEOF :: acc))
        | ' ' :: rest | '\t' :: rest | '\n' :: rest | '\r' :: rest ->
            // Skip whitespace
            lexHelper rest acc
        | '+' :: '+' :: rest -> lexHelper rest (TPlusPlus :: acc)
        | '+' :: rest -> lexHelper rest (TPlus :: acc)
        | '-' :: '>' :: rest -> lexHelper rest (TArrow :: acc)
        | '-' :: rest -> lexHelper rest (TMinus :: acc)
        | '=' :: '>' :: _ -> Error "Interpreter syntax does not use '=>'; use 'fun <args> -> <body>'"
        | '*' :: rest -> lexHelper rest (TStar :: acc)
        | '/' :: '/' :: rest ->
            // Skip line comment: // ... until end of line
            let rec skipToEndOfLine (cs: char list) : char list =
                match cs with
                | [] -> []
                | '\n' :: remaining -> remaining
                | '\r' :: '\n' :: remaining -> remaining
                | '\r' :: remaining -> remaining
                | _ :: remaining -> skipToEndOfLine remaining
            lexHelper (skipToEndOfLine rest) acc
        | '/' :: rest -> lexHelper rest (TSlash :: acc)
        | '(' :: rest -> lexHelper rest (TLParen :: acc)
        | ')' :: rest -> lexHelper rest (TRParen :: acc)
        | '{' :: rest -> lexHelper rest (TLBrace :: acc)
        | '}' :: rest -> lexHelper rest (TRBrace :: acc)
        | '[' :: rest -> lexHelper rest (TLBracket :: acc)
        | ']' :: rest -> lexHelper rest (TRBracket :: acc)
        | ':' :: ':' :: rest -> lexHelper rest (TCons :: acc)
        | ':' :: rest -> lexHelper rest (TColon :: acc)
        | ',' :: rest -> lexHelper rest (TComma :: acc)
        | ';' :: rest -> lexHelper rest (TSemicolon :: acc)
        | '.' :: '.' :: '.' :: rest -> lexHelper rest (TDotDotDot :: acc)
        | '.' :: rest -> lexHelper rest (TDot :: acc)
        | '=' :: '=' :: rest -> lexHelper rest (TEqEq :: acc)
        | '=' :: rest -> lexHelper rest (TEquals :: acc)
        | '!' :: '=' :: rest -> lexHelper rest (TNeq :: acc)
        | '!' :: rest -> lexHelper rest (TNot :: acc)
        | '<' :: '<' :: rest -> lexHelper rest (TShl :: acc)
        | '<' :: '=' :: rest -> lexHelper rest (TLte :: acc)
        | '<' :: rest -> lexHelper rest (TLt :: acc)
        | '>' :: '>' :: rest -> lexHelper rest (TShr :: acc)
        | '>' :: '=' :: rest -> lexHelper rest (TGte :: acc)
        | '>' :: rest -> lexHelper rest (TGt :: acc)
        | '&' :: '&' :: rest -> lexHelper rest (TAnd :: acc)
        | '&' :: rest -> lexHelper rest (TBitAnd :: acc)
        | '^' :: rest -> lexHelper rest (TBitXor :: acc)
        | '~' :: '~' :: '~' :: rest -> lexHelper rest (TBitNot :: acc)
        | '%' :: rest -> lexHelper rest (TPercent :: acc)
        | '|' :: '|' :: '|' :: rest -> lexHelper rest (TBitOr :: acc)
        | '|' :: '|' :: rest -> lexHelper rest (TOr :: acc)
        | '|' :: '>' :: rest -> lexHelper rest (TPipe :: acc)
        | '|' :: rest -> lexHelper rest (TBar :: acc)
        | c :: _ when System.Char.IsLetter(c) || c = '_' ->
            // Parse identifier or keyword
            let rec parseIdent (cs: char list) (chars: char list) : string * char list =
                match cs with
                | c :: rest when System.Char.IsLetterOrDigit(c) || c = '_' ->
                    parseIdent rest (c :: chars)
                | _ ->
                    let ident = System.String(List.rev chars |> List.toArray)
                    (ident, cs)

            let (ident, remaining) = parseIdent chars []
            let token =
                match ident with
                | "let" -> TLet
                | "in" -> TIn
                | "if" -> TIf
                | "then" -> TThen
                | "else" -> TElse
                | "def" -> TDef
                | "type" -> TType
                | "of" -> TOf
                | "match" -> TMatch
                | "with" -> TWith
                | "fun" -> TFun
                | "when" -> TWhen
                | "true" -> TTrue
                | "false" -> TFalse
                | "_" -> TUnderscore
                | _ -> TIdent ident
            lexHelper remaining (token :: acc)
        | c :: _ when System.Char.IsDigit(c) ->
            // Parse number (integer or float)
            // First collect all digits
            let rec collectDigits (cs: char list) (acc: char list) : char list * char list =
                match cs with
                | d :: rest when System.Char.IsDigit(d) -> collectDigits rest (d :: acc)
                | _ -> (List.rev acc, cs)

            let (intDigits, afterInt) = collectDigits chars []

            // Check if this is a float (has decimal point or exponent)
            match afterInt with
            | '.' :: rest when not (List.isEmpty rest) && System.Char.IsDigit(List.head rest) ->
                // Float with decimal point: 3.14
                let (fracDigits, afterFrac) = collectDigits rest []
                // Check for exponent
                match afterFrac with
                | ('e' :: rest' | 'E' :: rest') ->
                    // Scientific notation: 3.14e10 or 3.14e-10
                    let (expSign, afterSign) =
                        match rest' with
                        | '+' :: r -> (['+'], r)
                        | '-' :: r -> (['-'], r)
                        | _ -> ([], rest')
                    let (expDigits, remaining) = collectDigits afterSign []
                    if List.isEmpty expDigits then
                        Error "Expected exponent digits after 'e'"
                    else
                        let numStr = System.String(Array.ofList (intDigits @ ['.'] @ fracDigits @ ['e'] @ expSign @ expDigits))
                        match System.Double.TryParse(numStr, System.Globalization.NumberStyles.Float, System.Globalization.CultureInfo.InvariantCulture) with
                        | (true, value) -> lexHelper remaining (TFloat value :: acc)
                        | (false, _) -> Error $"Invalid float literal: {numStr}"
                | _ ->
                    // Float without exponent: 3.14
                    let numStr = System.String(Array.ofList (intDigits @ ['.'] @ fracDigits))
                    match System.Double.TryParse(numStr, System.Globalization.NumberStyles.Float, System.Globalization.CultureInfo.InvariantCulture) with
                    | (true, value) -> lexHelper afterFrac (TFloat value :: acc)
                    | (false, _) -> Error $"Invalid float literal: {numStr}"
            | ('e' :: rest | 'E' :: rest) ->
                // Scientific notation without decimal: 1e10 or 1e-10
                let (expSign, afterSign) =
                    match rest with
                    | '+' :: r -> (['+'], r)
                    | '-' :: r -> (['-'], r)
                    | _ -> ([], rest)
                let (expDigits, remaining) = collectDigits afterSign []
                if List.isEmpty expDigits then
                    Error "Expected exponent digits after 'e'"
                else
                    let numStr = System.String(Array.ofList (intDigits @ ['e'] @ expSign @ expDigits))
                    match System.Double.TryParse(numStr, System.Globalization.NumberStyles.Float, System.Globalization.CultureInfo.InvariantCulture) with
                    | (true, value) -> lexHelper remaining (TFloat value :: acc)
                    | (false, _) -> Error $"Invalid float literal: {numStr}"
            | _ ->
                // Interpreter syntax requires Int64 literals with L suffix.
                // Exception: tuple access uses plain numeric indices (e.g. t.0).
                let numStr = System.String(List.toArray intDigits)
                let parseInt64OrError (remaining: char list) =
                    match System.Int64.TryParse(numStr) with
                    | (true, value) -> lexHelper remaining (TInt64 value :: acc)
                    | (false, _) ->
                        if numStr = "9223372036854775808" then
                            lexHelper remaining (TInt64 System.Int64.MinValue :: acc)
                        else
                            Error $"Integer literal too large: {numStr}"
                let parseSizedIntOrError typeName tryParse mkToken remaining : Result<Token list, string> =
                    match tryParse numStr with
                    | (true, value) -> lexHelper remaining (mkToken value :: acc)
                    | (false, _) -> Error $"Integer literal out of range for {typeName}: {numStr}"
                let parseInt128CompatOrError (remaining: char list) : Result<Token list, string> =
                    match BigInteger.TryParse(numStr) with
                    | (true, value) ->
                        // Keep parser compatibility for interpreter upstream tests by
                        // mapping Int128 literals into Int64 two's-complement space.
                        let two64 = BigInteger.One <<< 64
                        let normalized = ((value % two64) + two64) % two64
                        let signed =
                            if normalized > BigInteger(int64 System.Int64.MaxValue) then
                                normalized - two64
                            else
                                normalized
                        match System.Int64.TryParse(signed.ToString()) with
                        | (true, int64Value) -> lexHelper remaining (TInt64 int64Value :: acc)
                        | (false, _) -> Error $"Invalid Int128 literal: {numStr}"
                    | (false, _) ->
                        Error $"Invalid Int128 literal: {numStr}"
                let parseUInt128CompatOrError (remaining: char list) : Result<Token list, string> =
                    match BigInteger.TryParse(numStr) with
                    | (true, value) ->
                        if value < BigInteger.Zero then
                            Error $"UInt128 literal must be non-negative: {numStr}"
                        else
                            // Keep parser compatibility for interpreter upstream tests by
                            // mapping UInt128 literals into UInt64 space modulo 2^64.
                            let two64 = BigInteger.One <<< 64
                            let normalized = value % two64
                            match System.UInt64.TryParse(normalized.ToString()) with
                            | (true, uint64Value) -> lexHelper remaining (TUInt64 uint64Value :: acc)
                            | (false, _) -> Error $"Invalid UInt128 literal: {numStr}"
                    | (false, _) ->
                        Error $"Invalid UInt128 literal: {numStr}"

                match afterInt with
                | 'L' :: rest ->
                    parseInt64OrError rest
                | 'Q' :: rest ->
                    parseInt128CompatOrError rest
                | 'y' :: rest ->
                    parseSizedIntOrError "Int8" System.SByte.TryParse TInt8 rest
                | 's' :: rest ->
                    parseSizedIntOrError "Int16" System.Int16.TryParse TInt16 rest
                | 'l' :: rest ->
                    parseSizedIntOrError "Int32" System.Int32.TryParse TInt32 rest
                | 'Z' :: rest ->
                    parseUInt128CompatOrError rest
                | 'u' :: 'y' :: rest ->
                    parseSizedIntOrError "UInt8" System.Byte.TryParse TUInt8 rest
                | 'u' :: 's' :: rest ->
                    parseSizedIntOrError "UInt16" System.UInt16.TryParse TUInt16 rest
                | 'u' :: 'l' :: rest ->
                    parseSizedIntOrError "UInt32" System.UInt32.TryParse TUInt32 rest
                | 'U' :: 'L' :: rest ->
                    parseSizedIntOrError "UInt64" System.UInt64.TryParse TUInt64 rest
                | _ ->
                    match acc with
                    | TDot :: _ ->
                        // tuple index context (e.g. .0, .1)
                        parseInt64OrError afterInt
                    | _ ->
                        Error $"Interpreter syntax requires Int64 literals to end with 'L': {numStr}"
        | '$' :: '"' :: rest ->
            // Parse interpolated string: $"Hello {name}!"
            // Returns TInterpString token with parts list
            let (isTripleQuoted, contentStart) =
                match rest with
                | '"' :: '"' :: remaining -> (true, remaining)
                | _ -> (false, rest)

            // Helper to parse escape sequences (same as regular strings)
            let parseEscape (cs: char list) : Result<char * char list, string> =
                match cs with
                | 'n' :: remaining -> Ok ('\n', remaining)
                | 't' :: remaining -> Ok ('\t', remaining)
                | 'r' :: remaining -> Ok ('\r', remaining)
                | '\\' :: remaining -> Ok ('\\', remaining)
                | '"' :: remaining -> Ok ('"', remaining)
                | '0' :: remaining -> Ok ('\000', remaining)
                | '{' :: remaining -> Ok ('{', remaining)  // Escape { as \{
                | '}' :: remaining -> Ok ('}', remaining)  // Escape } as \}
                | 'x' :: h1 :: h2 :: remaining ->
                    let hexStr = System.String([| h1; h2 |])
                    match System.Int32.TryParse(hexStr, System.Globalization.NumberStyles.HexNumber, null) with
                    | (true, value) -> Ok (char value, remaining)
                    | (false, _) -> Error $"Invalid hex escape sequence: \\x{hexStr}"
                | c :: _ -> Error $"Unknown escape sequence: \\{c}"
                | [] -> Error "Unterminated escape sequence"

            // Helper to collect characters until { or closing "
            let rec collectLiteralPart (cs: char list) (chars: char list) : Result<string * char list, string> =
                match cs with
                | [] -> Error "Unterminated interpolated string"
                | '"' :: '"' :: '"' :: remaining when isTripleQuoted ->
                    let str = System.String(List.rev chars |> List.toArray)
                    Ok (str, '"' :: '"' :: '"' :: remaining)  // Put """ back for caller to detect end
                | '"' :: remaining ->
                    if isTripleQuoted then
                        collectLiteralPart remaining ('"' :: chars)
                    else
                        let str = System.String(List.rev chars |> List.toArray)
                        Ok (str, '"' :: remaining)  // Put " back for caller to detect end
                | '{' :: remaining ->
                    let str = System.String(List.rev chars |> List.toArray)
                    Ok (str, '{' :: remaining)  // Put { back for caller to detect expression
                | '\\' :: escRest when not isTripleQuoted ->
                    match parseEscape escRest with
                    | Ok (c, remaining) -> collectLiteralPart remaining (c :: chars)
                    | Error err -> Error err
                | c :: remaining ->
                    collectLiteralPart remaining (c :: chars)

            // Helper to collect expression chars until matching }
            let rec collectExprChars (cs: char list) (depth: int) (chars: char list) : Result<char list * char list, string> =
                match cs with
                | [] -> Error "Unterminated interpolated expression"
                | '}' :: remaining when depth = 0 ->
                    Ok (List.rev chars, remaining)
                | '}' :: remaining ->
                    collectExprChars remaining (depth - 1) ('}' :: chars)
                | '{' :: remaining ->
                    collectExprChars remaining (depth + 1) ('{' :: chars)
                | '"' :: remaining ->
                    // Skip strings inside the expression
                    let rec skipString (cs: char list) (acc: char list) =
                        match cs with
                        | [] -> Error "Unterminated string in interpolated expression"
                        | '"' :: rest -> Ok ('"' :: acc, rest)
                        | '\\' :: c :: rest -> skipString rest (c :: '\\' :: acc)
                        | c :: rest -> skipString rest (c :: acc)
                    match skipString remaining ('"' :: chars) with
                    | Ok (acc', rest') -> collectExprChars rest' depth acc'
                    | Error err -> Error err
                | c :: remaining ->
                    collectExprChars remaining depth (c :: chars)

            // Parse all parts and build InterpPart list
            let rec parseInterpParts (cs: char list) (parts: InterpPart list) : Result<InterpPart list * char list, string> =
                match cs with
                | '"' :: '"' :: '"' :: remaining when isTripleQuoted ->
                    // End of triple-quoted interpolated string
                    Ok (List.rev parts, remaining)
                | '"' :: remaining when not isTripleQuoted ->
                    // End of interpolated string
                    Ok (List.rev parts, remaining)
                | '{' :: remaining ->
                    // Expression part - collect chars and lex them
                    match collectExprChars remaining 0 [] with
                    | Ok (exprChars, afterExpr) ->
                        let exprStr = System.String(exprChars |> List.toArray)
                        // Lex the expression
                        match lexHelper (exprStr |> Seq.toList) [] with
                        | Ok tokens ->
                            let tokens' = tokens |> List.filter (fun t -> t <> TEOF)
                            parseInterpParts afterExpr (InterpTokens tokens' :: parts)
                        | Error err -> Error $"Error in interpolated expression: {err}"
                    | Error err -> Error err
                | _ ->
                    // Literal part
                    match collectLiteralPart cs [] with
                    | Ok (str, afterLit) ->
                        if str = "" then
                            parseInterpParts afterLit parts
                        else
                            parseInterpParts afterLit (InterpText str :: parts)
                    | Error err -> Error err

            match parseInterpParts contentStart [] with
            | Ok (parts, remaining) ->
                lexHelper remaining (TInterpString parts :: acc)
            | Error err -> Error err

        | '"' :: '"' :: '"' :: rest ->
            // Parse raw triple-quoted string literal: """..."""
            let rec parseTripleString (cs: char list) (chars: char list) : Result<string * char list, string> =
                match cs with
                | [] -> Error "Unterminated triple-quoted string literal"
                | '"' :: '"' :: '"' :: remaining ->
                    let str = System.String(List.rev chars |> List.toArray)
                    Ok (str, remaining)
                | c :: remaining ->
                    parseTripleString remaining (c :: chars)

            match parseTripleString rest [] with
            | Ok (str, remaining) -> lexHelper remaining (TStringLit str :: acc)
            | Error err -> Error err

        | '\'' :: rest ->
            // Parse char literal with escape sequences (single Extended Grapheme Cluster)
            let rec parseCharContent (cs: char list) (chars: char list) : Result<string * char list, string> =
                match cs with
                | [] -> Error "Unterminated char literal"
                | '\'' :: remaining ->
                    // End of char literal
                    let str = System.String(List.rev chars |> List.toArray)
                    if str.Length = 0 then
                        Error "Empty char literal"
                    else
                        // Validate that it's a single Extended Grapheme Cluster using .NET's StringInfo
                        let enumerator = System.Globalization.StringInfo.GetTextElementEnumerator(str)
                        if enumerator.MoveNext() then
                            if enumerator.MoveNext() then
                                Error $"Char literal contains more than one grapheme cluster: '{str}'"
                            else
                                Ok (str, remaining)
                        else
                            Error "Empty char literal"
                | '\\' :: 'n' :: remaining ->
                    parseCharContent remaining ('\n' :: chars)
                | '\\' :: 't' :: remaining ->
                    parseCharContent remaining ('\t' :: chars)
                | '\\' :: 'r' :: remaining ->
                    parseCharContent remaining ('\r' :: chars)
                | '\\' :: '\\' :: remaining ->
                    parseCharContent remaining ('\\' :: chars)
                | '\\' :: '\'' :: remaining ->
                    parseCharContent remaining ('\'' :: chars)
                | '\\' :: '0' :: remaining ->
                    parseCharContent remaining ('\000' :: chars)
                | '\\' :: 'x' :: h1 :: h2 :: remaining ->
                    // Hex escape: \xNN
                    let hexStr = System.String([| h1; h2 |])
                    match System.Int32.TryParse(hexStr, System.Globalization.NumberStyles.HexNumber, null) with
                    | (true, value) ->
                        parseCharContent remaining (char value :: chars)
                    | (false, _) ->
                        Error $"Invalid hex escape sequence: \\x{hexStr}"
                | '\\' :: c :: _ ->
                    Error $"Unknown escape sequence: \\{c}"
                | c :: remaining ->
                    parseCharContent remaining (c :: chars)

            match parseCharContent rest [] with
            | Ok (str, remaining) -> lexHelper remaining (TCharLit str :: acc)
            | Error err -> Error err

        | '"' :: rest ->
            // Parse string literal with escape sequences
            let rec parseString (cs: char list) (chars: char list) : Result<string * char list, string> =
                match cs with
                | [] -> Error "Unterminated string literal"
                | '"' :: remaining ->
                    // End of string
                    let str = System.String(List.rev chars |> List.toArray)
                    Ok (str, remaining)
                | '\\' :: 'n' :: remaining ->
                    parseString remaining ('\n' :: chars)
                | '\\' :: 't' :: remaining ->
                    parseString remaining ('\t' :: chars)
                | '\\' :: 'r' :: remaining ->
                    parseString remaining ('\r' :: chars)
                | '\\' :: '\\' :: remaining ->
                    parseString remaining ('\\' :: chars)
                | '\\' :: '"' :: remaining ->
                    parseString remaining ('"' :: chars)
                | '\\' :: '0' :: remaining ->
                    parseString remaining ('\000' :: chars)
                | '\\' :: 'x' :: h1 :: h2 :: remaining ->
                    // Hex escape: \xNN
                    let hexStr = System.String([| h1; h2 |])
                    match System.Int32.TryParse(hexStr, System.Globalization.NumberStyles.HexNumber, null) with
                    | (true, value) ->
                        parseString remaining (char value :: chars)
                    | (false, _) ->
                        Error $"Invalid hex escape sequence: \\x{hexStr}"
                | '\\' :: c :: _ ->
                    Error $"Unknown escape sequence: \\{c}"
                | c :: remaining ->
                    parseString remaining (c :: chars)

            match parseString rest [] with
            | Ok (str, remaining) -> lexHelper remaining (TStringLit str :: acc)
            | Error err -> Error err
        | c :: _ ->
            Error $"Unexpected character: {c}"

    input |> Seq.toList |> fun cs -> lexHelper cs []

/// Parse function type parameters: type, type, ...) or type * type * ...)
let rec parseFunctionTypeParams (typeParams: Set<string>) (tokens: Token list) (acc: Type list) : Result<Type list * Token list, string> =
    match tokens with
    | TRParen :: rest ->
        // End of parameter list
        Ok (List.rev acc, rest)
    | _ ->
        // Parse a type
        parseTypeBase typeParams tokens
        |> Result.bind (fun (ty, remaining) ->
            match remaining with
            | TRParen :: rest -> Ok (List.rev (ty :: acc), rest)
            | TComma :: rest -> parseFunctionTypeParams typeParams rest (ty :: acc)
            | TStar :: rest -> parseFunctionTypeParams typeParams rest (ty :: acc)
            | _ -> Error "Expected ',', '*', or ')' in function type parameters")

/// Base type parser (no function types - used to parse function type components)
and parseTypeBase (typeParams: Set<string>) (tokens: Token list) : Result<Type * Token list, string> =
    match tokens with
    | TIdent "Int8" :: rest -> Ok (AST.TInt8, rest)
    | TIdent "Int16" :: rest -> Ok (AST.TInt16, rest)
    | TIdent "Int32" :: rest -> Ok (AST.TInt32, rest)
    | TIdent "Int64" :: rest -> Ok (AST.TInt64, rest)
    | TIdent "UInt8" :: rest -> Ok (AST.TUInt8, rest)
    | TIdent "UInt16" :: rest -> Ok (AST.TUInt16, rest)
    | TIdent "UInt32" :: rest -> Ok (AST.TUInt32, rest)
    | TIdent "UInt64" :: rest -> Ok (AST.TUInt64, rest)
    | TIdent "Bool" :: rest -> Ok (AST.TBool, rest)
    | TIdent "String" :: rest -> Ok (AST.TString, rest)
    | TIdent "Bytes" :: rest -> Ok (AST.TBytes, rest)
    | TIdent "Char" :: rest -> Ok (AST.TChar, rest)
    | TIdent "Float" :: rest -> Ok (AST.TFloat64, rest)
    | TIdent "Unit" :: rest -> Ok (AST.TUnit, rest)
    | TIdent "RawPtr" :: rest -> Ok (AST.TRawPtr, rest)  // Internal raw pointer type
    | TIdent typeName :: rest when Set.contains typeName typeParams ->
        Ok (TVar typeName, rest)
    | TIdent "List" :: TLt :: rest ->
        // List type: List<ElementType>
        parseTypeWithContext typeParams rest
        |> Result.bind (fun (elemType, afterElem) ->
            match afterElem with
            | TGt :: remaining -> Ok (TList elemType, remaining)
            | TShr :: remaining -> Ok (TList elemType, TGt :: remaining)  // >> is two >'s
            | _ -> Error "Expected '>' after List element type")
    | TIdent "Dict" :: TLt :: rest ->
        // Dict type: Dict<KeyType, ValueType>
        parseTypeWithContext typeParams rest
        |> Result.bind (fun (keyType, afterKey) ->
            match afterKey with
            | TComma :: valueRest ->
                parseTypeWithContext typeParams valueRest
                |> Result.bind (fun (valueType, afterValue) ->
                    match afterValue with
                    | TGt :: remaining -> Ok (TDict (keyType, valueType), remaining)
                    | TShr :: remaining -> Ok (TDict (keyType, valueType), TGt :: remaining)  // >> is two >'s
                    | _ -> Error "Expected '>' after Dict value type")
            | _ -> Error "Expected ',' after Dict key type")
    | TIdent typeName :: rest when System.Char.IsUpper(typeName.[0]) ->
        // Could be a simple type or a qualified type like Stdlib.Option.Option
        // First parse the full qualified name
        let rec parseQualTypeName (name: string) (toks: Token list) : string * Token list =
            match toks with
            | TDot :: TIdent nextName :: remaining when System.Char.IsUpper(nextName.[0]) ->
                parseQualTypeName (name + "." + nextName) remaining
            | _ -> (name, toks)
        let (fullTypeName, afterTypeName) = parseQualTypeName typeName rest
        // Check for type arguments <...>
        match afterTypeName with
        | TLt :: typeArgsStart ->
            // Generic type: TypeName<args>
            // Need to parse type args allowing lowercase type variables
            let rec parseTypeArgsInType (toks: Token list) (acc: Type list) : Result<Type list * Token list, string> =
                parseTypeWithContext typeParams toks
                |> Result.bind (fun (ty, remaining) ->
                    match remaining with
                    | TGt :: rest -> Ok (List.rev (ty :: acc), rest)
                    | TShr :: rest -> Ok (List.rev (ty :: acc), TGt :: rest)  // >> is two >'s
                    | TComma :: rest -> parseTypeArgsInType rest (ty :: acc)
                    | _ -> Error "Expected ',' or '>' after type argument in generic type")
            parseTypeArgsInType typeArgsStart []
            |> Result.map (fun (typeArgs, remaining) ->
                // Store as TSum with type arguments - type checker will validate
                (TSum (fullTypeName, typeArgs), remaining))
        | _ ->
            // Simple type without type arguments
            Ok (TRecord (fullTypeName, []), afterTypeName)
    | TLParen :: rest ->
        // Could be a function type: (int, int) -> bool
        // Or a tuple type: (int, int)
        parseFunctionTypeParams typeParams rest []
        |> Result.bind (fun (paramTypes, afterParams) ->
            match afterParams with
            | TArrow :: returnRest ->
                // Function type: (params) -> return
                parseTypeWithContext typeParams returnRest
                |> Result.map (fun (returnType, remaining) ->
                    (TFunction (paramTypes, returnType), remaining))
            | _ ->
                // Tuple type: (type, type, ...)
                if List.length paramTypes < 2 then
                    Error "Tuple type must have at least 2 elements"
                else
                    Ok (TTuple paramTypes, afterParams))
    | _ -> Error "Expected type annotation (Int64, Bool, String, Float, TypeName, type variable, or function type)"

/// Parse a type annotation with context for type parameters in scope
and parseTypeWithContext (typeParams: Set<string>) (tokens: Token list) : Result<Type * Token list, string> =
    let rec parseTupleTail (acc: Type list) (remaining: Token list) : Result<Type * Token list, string> =
        match remaining with
        | TStar :: rest ->
            parseTypeBase typeParams rest
            |> Result.bind (fun (nextType, afterNext) ->
                parseTupleTail (nextType :: acc) afterNext)
        | _ ->
            let allTypes = List.rev acc
            match allTypes with
            | [single] -> Ok (single, remaining)
            | _ -> Ok (TTuple allTypes, remaining)
    parseTypeBase typeParams tokens
    |> Result.bind (fun (firstType, remaining) ->
        parseTupleTail [firstType] remaining)

/// Parse a type annotation (no type parameters in scope)
let parseType (tokens: Token list) : Result<Type * Token list, string> =
    parseTypeWithContext Set.empty tokens

/// Parse type parameters: <t, u, v> (names only, for function definitions)
let rec parseTypeParams (tokens: Token list) (acc: string list) : Result<string list * Token list, string> =
    match tokens with
    | TIdent name :: TGt :: rest when System.Char.IsLower(name.[0]) ->
        // Last type parameter
        Ok (List.rev (name :: acc), rest)
    | TIdent name :: TComma :: rest when System.Char.IsLower(name.[0]) ->
        // More type parameters to come
        parseTypeParams rest (name :: acc)
    | TIdent name :: _ when not (System.Char.IsLower(name.[0])) ->
        Error $"Type parameter must start with lowercase letter: {name}"
    | TGt :: rest when List.isEmpty acc ->
        // Empty type parameters: <>
        Ok ([], rest)
    | _ -> Error "Expected type parameter name (lowercase identifier)"

/// Parse type for type arguments context (allows lowercase as type variables)
/// This is used when parsing call sites like func<t>(args) where t is a type variable
let rec parseTypeArgType (tokens: Token list) : Result<Type * Token list, string> =
    match tokens with
    | TIdent "Int8" :: rest -> Ok (AST.TInt8, rest)
    | TIdent "Int16" :: rest -> Ok (AST.TInt16, rest)
    | TIdent "Int32" :: rest -> Ok (AST.TInt32, rest)
    | TIdent "Int64" :: rest -> Ok (AST.TInt64, rest)
    | TIdent "UInt8" :: rest -> Ok (AST.TUInt8, rest)
    | TIdent "UInt16" :: rest -> Ok (AST.TUInt16, rest)
    | TIdent "UInt32" :: rest -> Ok (AST.TUInt32, rest)
    | TIdent "UInt64" :: rest -> Ok (AST.TUInt64, rest)
    | TIdent "Bool" :: rest -> Ok (AST.TBool, rest)
    | TIdent "String" :: rest -> Ok (AST.TString, rest)
    | TIdent "Bytes" :: rest -> Ok (AST.TBytes, rest)
    | TIdent "Char" :: rest -> Ok (AST.TChar, rest)
    | TIdent "Float" :: rest -> Ok (AST.TFloat64, rest)
    | TIdent "Unit" :: rest -> Ok (AST.TUnit, rest)
    | TIdent "RawPtr" :: rest -> Ok (AST.TRawPtr, rest)  // Internal raw pointer type
    | TIdent "List" :: TLt :: rest ->
        // List type: List<ElementType>
        parseTypeArgType rest
        |> Result.bind (fun (elemType, afterElem) ->
            match afterElem with
            | TGt :: remaining -> Ok (TList elemType, remaining)
            | TShr :: remaining -> Ok (TList elemType, TGt :: remaining)  // >> is two >'s
            | _ -> Error "Expected '>' after List element type in type argument")
    | TIdent "Dict" :: TLt :: rest ->
        // Dict type: Dict<KeyType, ValueType>
        parseTypeArgType rest
        |> Result.bind (fun (keyType, afterKey) ->
            match afterKey with
            | TComma :: valueRest ->
                parseTypeArgType valueRest
                |> Result.bind (fun (valueType, afterValue) ->
                    match afterValue with
                    | TGt :: remaining -> Ok (TDict (keyType, valueType), remaining)
                    | TShr :: remaining -> Ok (TDict (keyType, valueType), TGt :: remaining)  // >> is two >'s
                    | _ -> Error "Expected '>' after Dict value type in type argument")
            | _ -> Error "Expected ',' after Dict key type in type argument")
    | TIdent typeName :: rest when System.Char.IsLower(typeName.[0]) ->
        // Lowercase identifier is a type variable in type argument context
        Ok (TVar typeName, rest)
    | TIdent typeName :: rest when System.Char.IsUpper(typeName.[0]) ->
        // Could be a simple type or a qualified type like Stdlib.Option.Option
        // First parse the full qualified name
        let rec parseQualTypeName (name: string) (toks: Token list) : string * Token list =
            match toks with
            | TDot :: TIdent nextName :: remaining when System.Char.IsUpper(nextName.[0]) ->
                parseQualTypeName (name + "." + nextName) remaining
            | _ -> (name, toks)
        let (fullTypeName, afterTypeName) = parseQualTypeName typeName rest
        // Check for type arguments <...>
        match afterTypeName with
        | TLt :: typeArgsStart ->
            // Generic type: TypeName<args> - recursively parse type arguments
            let rec parseNestedTypeArgs (toks: Token list) (acc: Type list) : Result<Type list * Token list, string> =
                parseTypeArgType toks
                |> Result.bind (fun (ty, remaining) ->
                    match remaining with
                    | TGt :: rest -> Ok (List.rev (ty :: acc), rest)
                    | TShr :: rest -> Ok (List.rev (ty :: acc), TGt :: rest)  // >> is two >'s
                    | TComma :: rest -> parseNestedTypeArgs rest (ty :: acc)
                    | _ -> Error "Expected ',' or '>' after type argument in generic type")
            parseNestedTypeArgs typeArgsStart []
            |> Result.map (fun (typeArgs, remaining) ->
                (TSum (fullTypeName, typeArgs), remaining))
        | _ ->
            // Simple type without type arguments
            Ok (TRecord (fullTypeName, []), afterTypeName)
    | TLParen :: rest ->
        // Tuple type or function type: (Type1, Type2, ...) or (Type1, Type2) -> RetType
        parseTypeArgTupleElements rest []
        |> Result.bind (fun (elemTypes, afterElems) ->
            match afterElems with
            | TArrow :: returnRest ->
                // Function type: (params) -> return
                parseTypeArgType returnRest
                |> Result.map (fun (returnType, remaining) ->
                    (TFunction (elemTypes, returnType), remaining))
            | _ ->
                // Tuple type: (type, type, ...)
                if List.length elemTypes < 2 then
                    Error "Tuple type must have at least 2 elements"
                else
                    Ok (TTuple elemTypes, afterElems))
    | _ -> Error "Expected type in type argument"

/// Parse tuple elements in type argument context: Type1, Type2, ... )
and parseTypeArgTupleElements (tokens: Token list) (acc: Type list) : Result<Type list * Token list, string> =
    match tokens with
    | TRParen :: rest ->
        // End of tuple/parameter list
        Ok (List.rev acc, rest)
    | _ ->
        // Parse a type
        parseTypeArgType tokens
        |> Result.bind (fun (ty, remaining) ->
            match remaining with
            | TRParen :: rest -> Ok (List.rev (ty :: acc), rest)
            | TComma :: rest -> parseTypeArgTupleElements rest (ty :: acc)
            | _ -> Error "Expected ',' or ')' in tuple type")

/// Parse type arguments: <Int64, Bool, Point, t> (concrete types or type vars, for call sites)
let rec parseTypeArgs (tokens: Token list) (acc: Type list) : Result<Type list * Token list, string> =
    parseTypeArgType tokens
    |> Result.bind (fun (ty, remaining) ->
        match remaining with
        | TGt :: rest ->
            // Last type argument
            Ok (List.rev (ty :: acc), rest)
        | TShr :: rest ->
            // >> is two >'s - last type argument, put one > back
            Ok (List.rev (ty :: acc), TGt :: rest)
        | TComma :: rest ->
            // More type arguments to come
            parseTypeArgs rest (ty :: acc)
        | _ -> Error "Expected ',' or '>' after type argument")

/// Parse a single parameter: IDENT : type (with type parameter context)
let parseParamWithContext (typeParams: Set<string>) (tokens: Token list) : Result<(string * Type) * Token list, string> =
    match tokens with
    | TIdent name :: TColon :: rest ->
        parseTypeWithContext typeParams rest
        |> Result.map (fun (ty, remaining) -> ((name, ty), remaining))
    | _ -> Error "Expected parameter (name : type)"

/// Parse parameter list: param (, param)* (with type parameter context)
let rec parseParamsWithContext (typeParams: Set<string>) (tokens: Token list) (acc: (string * Type) list) : Result<(string * Type) list * Token list, string> =
    match tokens with
    | TRParen :: _ ->
        // End of parameters
        Ok (List.rev acc, tokens)
    | _ ->
        // Parse a parameter
        parseParamWithContext typeParams tokens
        |> Result.bind (fun (param, remaining) ->
            match remaining with
            | TComma :: rest ->
                // More parameters
                parseParamsWithContext typeParams rest (param :: acc)
            | TRParen :: _ ->
                // End of parameters
                Ok (List.rev (param :: acc), remaining)
            | _ -> Error "Expected ',' or ')' after parameter")

/// Parse parameter list: param (, param)* (no type parameters in scope)
let rec parseParams (tokens: Token list) (acc: (string * Type) list) : Result<(string * Type) list * Token list, string> =
    parseParamsWithContext Set.empty tokens acc

/// Parse record fields in a type definition: { name: Type, name: Type, ... }
/// Uses parseTypeWithContext so generic record fields can reference in-scope type parameters.
let rec parseRecordFieldsWithContext
    (typeParams: Set<string>)
    (tokens: Token list)
    (acc: (string * Type) list)
    : Result<(string * Type) list * Token list, string> =
    match tokens with
    | TRBrace :: rest ->
        // End of fields
        Ok (List.rev acc, rest)
    | TIdent name :: TColon :: rest ->
        parseTypeWithContext typeParams rest
        |> Result.bind (fun (ty, remaining) ->
            match remaining with
            | TComma :: rest' ->
                // More fields
                parseRecordFieldsWithContext typeParams rest' ((name, ty) :: acc)
            | TRBrace :: rest' ->
                // End of fields
                Ok (List.rev ((name, ty) :: acc), rest')
            | _ -> Error "Expected ',' or '}' after record field")
    | _ -> Error "Expected field name in record definition"

/// Parse record fields in a type definition with no type parameters in scope.
let parseRecordFields (tokens: Token list) (acc: (string * Type) list) : Result<(string * Type) list * Token list, string> =
    parseRecordFieldsWithContext Set.empty tokens acc

/// Parse sum type variants: Variant1 | Variant2 of Type | ...
/// Returns list of variants and remaining tokens
let rec parseVariants (tokens: Token list) (acc: Variant list) : Result<Variant list * Token list, string> =
    match tokens with
    | TIdent variantName :: TOf :: rest when System.Char.IsUpper(variantName.[0]) ->
        // Variant with payload: Variant of Type
        parseType rest
        |> Result.bind (fun (payloadType, afterType) ->
            let variant = { Name = variantName; Payload = Some payloadType }
            match afterType with
            | TBar :: rest' ->
                // More variants
                parseVariants rest' (variant :: acc)
            | _ ->
                // End of variants
                Ok (List.rev (variant :: acc), afterType))
    | TIdent variantName :: rest when System.Char.IsUpper(variantName.[0]) ->
        // Simple enum variant (no payload)
        let variant = { Name = variantName; Payload = None }
        match rest with
        | TBar :: rest' ->
            // More variants
            parseVariants rest' (variant :: acc)
        | _ ->
            // End of variants (next token is not a bar)
            Ok (List.rev (variant :: acc), rest)
    | _ -> Error "Expected variant name (must start with uppercase letter)"

/// Parse sum type variants with type parameter context: Variant1 | Variant2 of t | ...
/// Uses parseTypeWithContext to resolve type parameters
let rec parseVariantsWithContext (typeParams: string list) (tokens: Token list) (acc: Variant list) : Result<Variant list * Token list, string> =
    let typeParamSet = Set.ofList typeParams
    match tokens with
    | TIdent variantName :: TOf :: rest when System.Char.IsUpper(variantName.[0]) ->
        // Variant with payload: Variant of Type
        parseTypeWithContext typeParamSet rest
        |> Result.bind (fun (payloadType, afterType) ->
            let variant = { Name = variantName; Payload = Some payloadType }
            match afterType with
            | TBar :: rest' ->
                // More variants
                parseVariantsWithContext typeParams rest' (variant :: acc)
            | _ ->
                // End of variants
                Ok (List.rev (variant :: acc), afterType))
    | TIdent variantName :: rest when System.Char.IsUpper(variantName.[0]) ->
        // Simple enum variant (no payload)
        let variant = { Name = variantName; Payload = None }
        match rest with
        | TBar :: rest' ->
            // More variants
            parseVariantsWithContext typeParams rest' (variant :: acc)
        | _ ->
            // End of variants (next token is not a bar)
            Ok (List.rev (variant :: acc), rest)
    | _ -> Error "Expected variant name (must start with uppercase letter)"

/// Parse a qualified type name: Name or Stdlib.Result.Result
let rec parseQualifiedTypeName (firstName: string) (tokens: Token list) : string * Token list =
    match tokens with
    | TDot :: TIdent nextName :: rest when System.Char.IsUpper(nextName.[0]) ->
        let (fullName, remaining) = parseQualifiedTypeName nextName rest
        (firstName + "." + fullName, remaining)
    | _ ->
        (firstName, tokens)

/// Parse a type definition: type Name = { fields } or type Name = Variant1 | Variant2 of Type | ...
/// Also supports type aliases: type Id = String, type MyList = List<Int64>
/// Supports qualified type names: type Stdlib.Result.Result = Ok of T | Error of E
/// Supports generic types: type Result<t, e> = Ok of t | Error of e
let parseTypeDef (tokens: Token list) : Result<TypeDef * Token list, string> =
    match tokens with
    | TType :: TIdent firstName :: rest when System.Char.IsUpper(firstName.[0]) ->
        // Parse potentially qualified type name
        let (typeName, afterName) = parseQualifiedTypeName firstName rest
        // Check for type parameters: <t, e>
        let parseBody typeParams afterTypeParams =
            match afterTypeParams with
            | TEquals :: TLBrace :: bodyRest ->
                // Record type: type Name = { field: Type, ... }
                let typeParamSet = Set.ofList typeParams
                parseRecordFieldsWithContext typeParamSet bodyRest []
                |> Result.map (fun (fields, remaining) ->
                    (RecordDef (typeName, typeParams, fields), remaining))
            | TEquals :: TIdent variantName :: TOf :: bodyRest when System.Char.IsUpper(variantName.[0]) ->
                // Sum type with first variant having payload: type Name = Variant of Type | ...
                let typeParamSet = Set.ofList typeParams
                parseTypeWithContext typeParamSet bodyRest
                |> Result.bind (fun (payloadType, afterType) ->
                    let firstVariant = { Name = variantName; Payload = Some payloadType }
                    match afterType with
                    | TBar :: rest' ->
                        // More variants
                        parseVariantsWithContext typeParams rest' [firstVariant]
                        |> Result.map (fun (variants, remaining) ->
                            (SumTypeDef (typeName, typeParams, variants), remaining))
                    | _ ->
                        // Single variant sum type
                        Ok (SumTypeDef (typeName, typeParams, [firstVariant]), afterType))
            | TEquals :: TIdent variantName :: TBar :: bodyRest when System.Char.IsUpper(variantName.[0]) ->
                // Sum type with multiple variants: type Name = Variant1 | Variant2 | ...
                let firstVariant = { Name = variantName; Payload = None }
                parseVariantsWithContext typeParams bodyRest [firstVariant]
                |> Result.map (fun (variants, remaining) ->
                    (SumTypeDef (typeName, typeParams, variants), remaining))
            | TEquals :: TBar :: bodyRest ->
                // Sum type where the first variant starts on the next line:
                // type Name =
                //   | Variant1
                //   | Variant2 of Type
                parseVariantsWithContext typeParams bodyRest []
                |> Result.map (fun (variants, remaining) ->
                    (SumTypeDef (typeName, typeParams, variants), remaining))
            | TEquals :: rest' ->
                // Could be a type alias or a single-variant sum type
                // Try to parse as a type first
                match rest' with
                | TBar :: variantRest ->
                    parseVariantsWithContext typeParams variantRest []
                    |> Result.map (fun (variants, remaining) ->
                        (SumTypeDef (typeName, typeParams, variants), remaining))
                | _ ->
                    let typeParamSet = Set.ofList typeParams
                    match parseTypeWithContext typeParamSet rest' with
                    | Ok (targetType, remaining) ->
                        // Decide: type alias or single-variant sum type?
                        // Rules:
                        // 1. Primitive types (Int64, String, etc.) → TYPE ALIAS
                        // 2. Generic types (List<T>, Result<T,E>) → TYPE ALIAS
                        // 3. Tuple types ((T, U)) → TYPE ALIAS
                        // 4. Function types ((T) -> U) → TYPE ALIAS
                        // 5. Simple name (TRecord):
                        //    - Same name as type being defined → SUM TYPE (recursive variant)
                        //    - End of input → SUM TYPE (backwards compat for single-variant enums)
                        //    - Otherwise → TYPE ALIAS (reference to existing type)
                        match targetType with
                        | TRecord (potentialVariant, _) when potentialVariant = typeName ->
                            // Same name as type being defined - this is a recursive variant definition
                            // e.g., type Unit2 = Unit2 defines a sum type with variant Unit2
                            let variant = { Name = potentialVariant; Payload = None }
                            Ok (SumTypeDef (typeName, typeParams, [variant]), remaining)
                        | TRecord (potentialVariant, _) when
                            // Not a primitive type and at end of input - treat as sum type for backwards compat
                            potentialVariant <> "Int64" && potentialVariant <> "Int32" && potentialVariant <> "Int16" && potentialVariant <> "Int8" &&
                            potentialVariant <> "UInt64" && potentialVariant <> "UInt32" && potentialVariant <> "UInt16" && potentialVariant <> "UInt8" &&
                            potentialVariant <> "Bool" && potentialVariant <> "String" && potentialVariant <> "Float" &&
                            (match remaining with [] -> true | _ -> false) ->
                            let variant = { Name = potentialVariant; Payload = None }
                            Ok (SumTypeDef (typeName, typeParams, [variant]), remaining)
                        | _ ->
                            // Type alias for:
                            // - Primitive types (parsed as TInt64, TString, etc. directly by parseType)
                            // - Generic types (TSum with type args, TList)
                            // - Tuple types (TTuple)
                            // - Function types (TFunction)
                            // - User types with remaining tokens (assumed to be alias to existing type)
                            Ok (TypeAlias (typeName, typeParams, targetType), remaining)
                    | Error _ ->
                        Error "Expected type expression after '=' in type alias or variant name"
            | _ -> Error "Expected '=' after type name in type definition"
        match afterName with
        | TLt :: rest' ->
            // Generic type: type Name<t, e> = ...
            parseTypeParams rest' []
            |> Result.bind (fun (typeParams, afterParams) ->
                parseBody typeParams afterParams)
        | _ ->
            // Non-generic type
            parseBody [] afterName
    | TType :: TIdent name :: _ when not (System.Char.IsUpper(name.[0])) ->
        Error $"Type name must start with uppercase letter: {name}"
    | _ -> Error "Expected type definition: type Name = { fields } or type Name = Variant1 | Variant2"

/// Parse a qualified function name: name or Stdlib.Int64.add
let rec parseQualifiedFuncName (firstName: string) (tokens: Token list) : string * Token list =
    match tokens with
    | TDot :: TIdent nextName :: rest ->
        let (fullName, remaining) = parseQualifiedFuncName nextName rest
        (firstName + "." + fullName, remaining)
    | _ ->
        (firstName, tokens)

/// Parse a function definition: def name<T, U>(params) : type = body
/// Type parameters are optional: def name(params) : type = body is also valid
/// Qualified names supported: def Stdlib.Int64.add(params) : type = body
let parseFunctionDef (tokens: Token list) (parseExpr: Token list -> Result<Expr * Token list, string>) : Result<FunctionDef * Token list, string> =
    match tokens with
    | TDef :: TIdent firstName :: rest ->
        // Parse potentially qualified function name (e.g., Stdlib.Int64.add)
        let (name, afterName) = parseQualifiedFuncName firstName rest
        match afterName with
        | TLt :: rest' ->
            // Generic function: def name<T, U>(...)
            parseTypeParams rest' []
            |> Result.bind (fun (typeParams, afterTypeParams) ->
                // Check for duplicate type parameters
                if List.length typeParams <> (typeParams |> List.distinct |> List.length) then
                    Error "Duplicate type parameter names"
                else
                let typeParamsSet = Set.ofList typeParams
                match afterTypeParams with
                | TLParen :: paramsStart ->
                    // Parse parameters with type params in scope
                    let paramsResult =
                        match paramsStart with
                        | TRParen :: _ -> Ok ([], paramsStart)
                        | _ -> parseParamsWithContext typeParamsSet paramsStart []

                    paramsResult
                    |> Result.bind (fun (parameters, remaining) ->
                        match remaining with
                        | TRParen :: TColon :: rest'' ->
                            // Parse return type with type params in scope
                            parseTypeWithContext typeParamsSet rest''
                            |> Result.bind (fun (returnType, remaining') ->
                                match remaining' with
                                | TEquals :: rest''' ->
                                    // Parse body
                                    parseExpr rest'''
                                    |> Result.map (fun (body, remaining'') ->
                                        let funcDef = {
                                            Name = name
                                            TypeParams = typeParams
                                            Params = parameters
                                            ReturnType = returnType
                                            Body = body
                                        }
                                        (funcDef, remaining''))
                                | _ -> Error "Expected '=' after function return type")
                        | _ -> Error "Expected ':' after function parameters")
                | _ -> Error "Expected '(' after type parameters")
        | TLParen :: rest' ->
            // Non-generic function: def name(...)
            let paramsResult =
                match rest' with
                | TRParen :: _ -> Ok ([], rest')
                | _ -> parseParams rest' []

            paramsResult
            |> Result.bind (fun (parameters, remaining) ->
                match remaining with
                | TRParen :: TColon :: rest'' ->
                    // Parse return type
                    parseType rest''
                    |> Result.bind (fun (returnType, remaining') ->
                        match remaining' with
                        | TEquals :: rest''' ->
                            // Parse body
                            parseExpr rest'''
                            |> Result.map (fun (body, remaining'') ->
                                let funcDef = {
                                    Name = name
                                    TypeParams = []
                                    Params = parameters
                                    ReturnType = returnType
                                    Body = body
                                }
                                (funcDef, remaining''))
                        | _ -> Error "Expected '=' after function return type")
                | _ -> Error "Expected ':' after function parameters")
        | _ -> Error $"Expected '<' or '(' after function name '{name}'"
    | _ -> Error "Expected function definition (def name(params) : type = body)"

/// Parse a pattern for pattern matching
let rec parsePattern (tokens: Token list) : Result<Pattern * Token list, string> =
    let canStartPatternPayload (toks: Token list) : bool =
        match toks with
        | TUnderscore :: _
        | TInt64 _ :: _
        | TInt8 _ :: _
        | TInt16 _ :: _
        | TInt32 _ :: _
        | TUInt8 _ :: _
        | TUInt16 _ :: _
        | TUInt32 _ :: _
        | TUInt64 _ :: _
        | TMinus :: TInt64 _ :: _
        | TMinus :: TInt8 _ :: _
        | TMinus :: TInt16 _ :: _
        | TMinus :: TInt32 _ :: _
        | TMinus :: TFloat _ :: _
        | TTrue :: _
        | TFalse :: _
        | TStringLit _ :: _
        | TCharLit _ :: _
        | TFloat _ :: _
        | TLParen :: _
        | TLBracket :: _
        | TIdent _ :: _ -> true
        | _ -> false

    let rec parsePatternBase (toks: Token list) : Result<Pattern * Token list, string> =
        match toks with
        | TUnderscore :: rest ->
            // Wildcard pattern: _
            Ok (PWildcard, rest)
        | TInt64 n :: rest ->
            // Integer literal pattern (Int64)
            Ok (PInt64 n, rest)
        | TInt8 n :: rest ->
            Ok (PInt8Literal n, rest)
        | TInt16 n :: rest ->
            Ok (PInt16Literal n, rest)
        | TInt32 n :: rest ->
            Ok (PInt32Literal n, rest)
        | TUInt8 n :: rest ->
            Ok (PUInt8Literal n, rest)
        | TUInt16 n :: rest ->
            Ok (PUInt16Literal n, rest)
        | TUInt32 n :: rest ->
            Ok (PUInt32Literal n, rest)
        | TUInt64 n :: rest ->
            Ok (PUInt64Literal n, rest)
        | TMinus :: TInt64 n :: rest ->
            // Negative integer literal pattern
            Ok (PInt64 (-n), rest)
        | TMinus :: TInt8 n :: rest ->
            Ok (PInt8Literal (sbyte (-int n)), rest)
        | TMinus :: TInt16 n :: rest ->
            Ok (PInt16Literal (int16 (-int n)), rest)
        | TMinus :: TInt32 n :: rest ->
            Ok (PInt32Literal (-n), rest)
        | TMinus :: TFloat f :: rest ->
            Ok (PFloat (-f), rest)
        | TTrue :: rest ->
            // Boolean true pattern
            Ok (PBool true, rest)
        | TFalse :: rest ->
            // Boolean false pattern
            Ok (PBool false, rest)
        | TStringLit s :: rest ->
            // String literal pattern
            Ok (PString s, rest)
        | TCharLit s :: rest ->
            // Char patterns share the same runtime representation as string literals.
            Ok (PString s, rest)
        | TFloat f :: rest ->
            // Float literal pattern
            Ok (PFloat f, rest)
        | TLParen :: TRParen :: rest ->
            // Unit pattern: ()
            Ok (PUnit, rest)
        | TLParen :: rest ->
            // Parenthesized pattern or tuple pattern: (p) / (a, b, c)
            parseTuplePattern rest []
        | TLBrace :: _ ->
            // Anonymous record pattern is no longer supported
            Error "Record pattern requires type name: use 'TypeName { field = pattern, ... }'"
        | TLBracket :: rest ->
            // List pattern: [a, b, c] or []
            parseListPattern rest []
        | TIdent typeName :: TLBrace :: rest when System.Char.IsUpper(typeName.[0]) ->
            // Record pattern with type name: Point { x = a, y = b }
            parseRecordPatternWithTypeName typeName rest []
        | TIdent name :: rest when System.Char.IsUpper(name.[0]) ->
            // Constructor pattern, optionally with interpreter-style payload: Some x
            if canStartPatternPayload rest then
                parsePattern rest
                |> Result.map (fun (payloadPattern, remaining) ->
                    (PConstructor (name, Some payloadPattern), remaining))
            else
                Ok (PConstructor (name, None), rest)
        | TIdent name :: rest ->
            // Variable pattern: x (binds the value)
            Ok (PVar name, rest)
        | _ -> Error "Expected pattern (_, variable, literal, or constructor)"

    let rec parseConsTail (headPattern: Pattern) (remaining: Token list) : Result<Pattern * Token list, string> =
        let rec normalizeConsPattern (headPatterns: Pattern list) (tailPattern: Pattern) : Pattern =
            match tailPattern with
            | PList listTail ->
                // a :: b :: [c; d]  ==>  [a; b; c; d]
                PList (headPatterns @ listTail)
            | PListCons (moreHeads, tail) ->
                // Flatten chained cons heads while preserving order.
                normalizeConsPattern (headPatterns @ moreHeads) tail
            | _ ->
                PListCons (headPatterns, tailPattern)

        match remaining with
        | TCons :: rest ->
            parsePattern rest
            |> Result.map (fun (tailPattern, rest') ->
                match tailPattern with
                | PListCons (tailHead, tailRest) ->
                    (normalizeConsPattern (headPattern :: tailHead) tailRest, rest')
                | _ ->
                    (normalizeConsPattern [headPattern] tailPattern, rest'))
        | _ ->
            Ok (headPattern, remaining)

    parsePatternBase tokens
    |> Result.bind (fun (headPattern, remaining) ->
        parseConsTail headPattern remaining)

and parseTuplePattern (tokens: Token list) (acc: Pattern list) : Result<Pattern * Token list, string> =
    parsePattern tokens
    |> Result.bind (fun (pat, remaining) ->
        match remaining with
        | TRParen :: rest ->
            // End of parenthesized / tuple pattern
            let patterns = List.rev (pat :: acc)
            match patterns with
            | [single] -> Ok (single, rest)
            | _ -> Ok (PTuple patterns, rest)
        | TComma :: rest ->
            // More elements
            parseTuplePattern rest (pat :: acc)
        | _ -> Error "Expected ',' or ')' in tuple pattern")

and parseRecordPatternWithTypeName (typeName: string) (tokens: Token list) (acc: (string * Pattern) list) : Result<Pattern * Token list, string> =
    // Parse record pattern with explicit type name: TypeName { field = pattern, ... }
    match tokens with
    | TRBrace :: rest ->
        // Empty record or end of fields
        let fields = List.rev acc
        Ok (PRecord (typeName, fields), rest)
    | TIdent fieldName :: TEquals :: rest ->
        parsePattern rest
        |> Result.bind (fun (pat, remaining) ->
            let field = (fieldName, pat)
            match remaining with
            | TRBrace :: rest' ->
                // End of record pattern
                let fields = List.rev (field :: acc)
                Ok (PRecord (typeName, fields), rest')
            | TComma :: rest' ->
                // More fields
                parseRecordPatternWithTypeName typeName rest' (field :: acc)
            | _ -> Error "Expected ',' or '}' in record pattern")
    | _ -> Error "Expected field name in record pattern"

and parseListPattern (tokens: Token list) (acc: Pattern list) : Result<Pattern * Token list, string> =
    match tokens with
    | TRBracket :: rest ->
        // Empty list or end of list pattern
        Ok (PList (List.rev acc), rest)
    | TDotDotDot :: rest ->
        // Rest pattern at start: [...t]
        parsePattern rest
        |> Result.bind (fun (tailPat, remaining) ->
            match remaining with
            | TRBracket :: rest' ->
                Ok (PListCons (List.rev acc, tailPat), rest')
            | _ -> Error "Expected ']' after rest pattern")
    | _ ->
        parsePattern tokens
        |> Result.bind (fun (pat, remaining) ->
            match remaining with
            | TRBracket :: rest ->
                // End of list pattern
                Ok (PList (List.rev (pat :: acc)), rest)
            | TSemicolon :: TDotDotDot :: rest ->
                // Rest pattern after element: [a, b, ...t]
                parsePattern rest
                |> Result.bind (fun (tailPat, remaining') ->
                    match remaining' with
                    | TRBracket :: rest' ->
                        Ok (PListCons (List.rev (pat :: acc), tailPat), rest')
                    | _ -> Error "Expected ']' after rest pattern")
            | TSemicolon :: rest ->
                // More elements
                parseListPattern rest (pat :: acc)
            | _ -> Error "Expected ';' or ']' in list pattern")

/// Parse a single case: | pat1 | pat2 when guard -> expr
/// Supports multiple patterns (pattern grouping) and optional guard clause
let parseCase (tokens: Token list) (parseExprFn: Token list -> Result<Expr * Token list, string>) : Result<MatchCase * Token list, string> =
    // Parse patterns until we see TWhen or TArrow
    let rec parsePatterns (toks: Token list) (acc: Pattern list) : Result<Pattern list * Token list, string> =
        match toks with
        | TBar :: rest ->
            parsePattern rest
            |> Result.bind (fun (pattern, remaining) ->
                // Check what comes next
                match remaining with
                | TBar :: _ ->
                    // Another pattern in the group
                    parsePatterns remaining (pattern :: acc)
                | TWhen :: _ | TArrow :: _ ->
                    // End of patterns, followed by guard or body
                    Ok (List.rev (pattern :: acc), remaining)
                | _ -> Error "Expected '|', 'when', or '->' after pattern")
        | _ -> Error "Expected '|' before pattern"

    parsePatterns tokens []
    |> Result.bind (fun (patterns, remaining) ->
        // Convert patterns list to NonEmptyList (safe since parsePatterns ensures at least one pattern)
        let patternsNel = NonEmptyList.fromList patterns
        // Parse optional guard
        match remaining with
        | TWhen :: rest' ->
            // Parse guard expression
            parseExprFn rest'
            |> Result.bind (fun (guard, remaining') ->
                match remaining' with
                | TArrow :: rest'' ->
                    // Parse body
                    parseExprFn rest''
                    |> Result.map (fun (body, remaining''') ->
                        ({ Patterns = patternsNel; Guard = Some guard; Body = body }, remaining'''))
                | _ -> Error "Expected '->' after guard expression")
        | TArrow :: rest' ->
            // No guard, parse body directly
            parseExprFn rest'
            |> Result.map (fun (body, remaining') ->
                ({ Patterns = patternsNel; Guard = None; Body = body }, remaining'))
        | _ -> Error "Expected 'when' or '->' after pattern")

/// Parser: convert tokens to AST
let parse (tokens: Token list) : Result<Program, string> =
    // Recursive descent parser with operator precedence
    // Precedence (low to high): or < and < comparison < +/- < */ < unary

    let startsWithNegativeNumericLiteral (toks: Token list) : bool =
        match toks with
        | TMinus :: TInt64 _ :: _
        | TMinus :: TInt8 _ :: _
        | TMinus :: TInt16 _ :: _
        | TMinus :: TInt32 _ :: _
        | TMinus :: TFloat _ :: _ -> true
        | _ -> false

    let canStartApplicationArg (toks: Token list) : bool =
        match toks with
        | TInt64 _ :: _
        | TInt8 _ :: _
        | TInt16 _ :: _
        | TInt32 _ :: _
        | TUInt8 _ :: _
        | TUInt16 _ :: _
        | TUInt32 _ :: _
        | TUInt64 _ :: _
        | TFloat _ :: _
        | TStringLit _ :: _
        | TCharLit _ :: _
        | TTrue :: _
        | TFalse :: _
        | TIdent _ :: _
        | TLParen :: _
        | TLBrace :: _
        | TLBracket :: _
        | TFun :: _ -> true
        | _ -> false

    let canStartNegativeNumericApplicationArg (callee: Expr) (toks: Token list) : bool =
        if not (startsWithNegativeNumericLiteral toks) then
            false
        else
            match callee with
            // Keep subtraction precedence for bare variables (`x - 1L`),
            // but allow negative numeric literals as call args in contexts
            // that are clearly call-like in interpreter syntax.
            | Var funcName when funcName.Contains "." -> true
            | Call _ | TypeApp _ | Apply _ | Constructor (_, _, None) -> true
            | _ -> false

    let appendCallArg (callee: Expr) (argExpr: Expr) : Expr =
        match callee with
        | Var funcName -> Call (funcName, [argExpr])
        | Call (funcName, args) -> Call (funcName, args @ [argExpr])
        | TypeApp (funcName, typeArgs, args) -> TypeApp (funcName, typeArgs, args @ [argExpr])
        | Constructor (typeName, variantName, None) -> Constructor (typeName, variantName, Some argExpr)
        | Apply (funcExpr, existingArgs) ->
            match funcExpr with
            // Preserve uncurried lambda applications as a single Apply node
            // while still allowing curried chains to remain left-associated.
            | Lambda (parameters, _) when List.length existingArgs < List.length parameters ->
                Apply (funcExpr, existingArgs @ [argExpr])
            | _ ->
                Apply (callee, [argExpr])
        | _ -> Apply (callee, [argExpr])

    /// Parse multiple cases for pattern matching: | p1 -> e1 | p2 -> e2 ...
    let rec parseCases (toks: Token list) (acc: MatchCase list) : Result<MatchCase list * Token list, string> =
        match toks with
        | TBar :: _ ->
            // Another case
            parseCase toks parseExpr
            |> Result.bind (fun (case, remaining) ->
                parseCases remaining (case :: acc))
        | _ ->
            // End of cases
            if List.isEmpty acc then
                Error "Match expression must have at least one case"
            else
                Ok (List.rev acc, toks)

    and parseExpr (toks: Token list) : Result<Expr * Token list, string> =
        match toks with
        | TLet :: rest ->
            // Parse: let pattern = value in body
            // Supports simple let (let x = ...) and pattern matching (let (a, b) = ...)
            parsePattern rest
            |> Result.bind (fun (pattern, remaining) ->
                let buildLetExpression (value: Expr) (body: Expr) (remaining'': Token list) =
                    match pattern with
                    | PVar name -> (Let (name, value, body), remaining'')
                    | _ -> (Match (value, [{ Patterns = NonEmptyList.singleton pattern; Guard = None; Body = body }]), remaining'')
                match remaining with
                | TEquals :: rest' ->
                    parseExpr rest'
                    |> Result.bind (fun (value, remaining') ->
                        match remaining' with
                        | TIn :: rest'' ->
                            parseExpr rest''
                            |> Result.map (fun (body, remaining'') ->
                                buildLetExpression value body remaining'')
                        | _ ->
                            // Upstream interpreter tests allow newline-separated `let` bodies
                            // without an explicit `in` token.
                            parseExpr remaining'
                            |> Result.map (fun (body, remaining'') ->
                                buildLetExpression value body remaining''))
                | _ -> Error "Expected '=' after let binding pattern")
        | TIf :: rest ->
            // Parse: if cond then thenBranch [else elseBranch]
            // When else is omitted, synthesize unit: if cond then expr  ==>  if cond then expr else ()
            parseExpr rest
            |> Result.bind (fun (cond, remaining) ->
                match remaining with
                | TThen :: rest' ->
                    parseExpr rest'
                    |> Result.bind (fun (thenBranch, remaining') ->
                        match remaining' with
                        | TElse :: rest'' ->
                            parseExpr rest''
                            |> Result.map (fun (elseBranch, remaining'') ->
                                (If (cond, thenBranch, elseBranch), remaining''))
                        | _ ->
                            Ok (If (cond, thenBranch, UnitLiteral), remaining'))
                | _ -> Error "Expected 'then' after if condition")
        | TMatch :: rest ->
            // Parse: match scrutinee with | p1 -> e1 | p2 -> e2
            parseExpr rest
            |> Result.bind (fun (scrutinee, remaining) ->
                match remaining with
                | TWith :: rest' ->
                    parseCases rest' []
                    |> Result.map (fun (cases, remaining') ->
                        (Match (scrutinee, cases), remaining'))
                | _ -> Error "Expected 'with' after match scrutinee")
        | _ ->
            parsePipe toks

    and parsePipe (toks: Token list) : Result<Expr * Token list, string> =
        // Pipe operator |> has lowest precedence, left-associative
        // x |> f desugars to f(x) - Call if f is a name, Apply if f is an expression
        parseOr toks
        |> Result.bind (fun (left, remaining) ->
            let rec parsePipeRest (leftExpr: Expr) (toks: Token list) : Result<Expr * Token list, string> =
                match toks with
                | TPipe :: rest ->
                    parseOr rest
                    |> Result.bind (fun (right, remaining') ->
                        // Desugar: left |> right
                        // Pipe passes left as the FIRST argument to right
                        // This matches Dark/Darklang convention where data comes first
                        let pipedExpr =
                            match right with
                            | Var funcName ->
                                // Simple function reference: f becomes f(left)
                                Call (funcName, [leftExpr])
                            | Call (funcName, args) ->
                                // Partial application: f(a) becomes f(left, a)
                                Call (funcName, leftExpr :: args)
                            | TypeApp (funcName, typeArgs, args) ->
                                // Generic partial application: f<T>(a) becomes f<T>(left, a)
                                TypeApp (funcName, typeArgs, leftExpr :: args)
                            | _ ->
                                // Lambda or other expression: apply left to it
                                Apply (right, [leftExpr])
                        parsePipeRest pipedExpr remaining')
                | _ -> Ok (leftExpr, toks)
            parsePipeRest left remaining)

    and parseOr (toks: Token list) : Result<Expr * Token list, string> =
        parseAnd toks
        |> Result.bind (fun (left, remaining) ->
            let rec parseOrRest (leftExpr: Expr) (toks: Token list) : Result<Expr * Token list, string> =
                match toks with
                | TOr :: rest ->
                    parseAnd rest
                    |> Result.bind (fun (right, remaining') ->
                        parseOrRest (BinOp (Or, leftExpr, right)) remaining')
                | _ -> Ok (leftExpr, toks)
            parseOrRest left remaining)

    and parseAnd (toks: Token list) : Result<Expr * Token list, string> =
        parseBitOr toks
        |> Result.bind (fun (left, remaining) ->
            let rec parseAndRest (leftExpr: Expr) (toks: Token list) : Result<Expr * Token list, string> =
                match toks with
                | TAnd :: rest ->
                    parseBitOr rest
                    |> Result.bind (fun (right, remaining') ->
                        parseAndRest (BinOp (And, leftExpr, right)) remaining')
                | _ -> Ok (leftExpr, toks)
            parseAndRest left remaining)

    and parseBitOr (toks: Token list) : Result<Expr * Token list, string> =
        parseBitXor toks
        |> Result.bind (fun (left, remaining) ->
            let rec parseBitOrRest (leftExpr: Expr) (toks: Token list) : Result<Expr * Token list, string> =
                match toks with
                | TBitOr :: rest ->
                    parseBitXor rest
                    |> Result.bind (fun (right, remaining') ->
                        parseBitOrRest (BinOp (BitOr, leftExpr, right)) remaining')
                | _ -> Ok (leftExpr, toks)
            parseBitOrRest left remaining)

    and parseBitXor (toks: Token list) : Result<Expr * Token list, string> =
        parseBitAnd toks
        |> Result.bind (fun (left, remaining) ->
            let rec parseBitXorRest (leftExpr: Expr) (toks: Token list) : Result<Expr * Token list, string> =
                match toks with
                | TBitXor :: rest ->
                    parseBitAnd rest
                    |> Result.bind (fun (right, remaining') ->
                        parseBitXorRest (BinOp (BitXor, leftExpr, right)) remaining')
                | _ -> Ok (leftExpr, toks)
            parseBitXorRest left remaining)

    and parseBitAnd (toks: Token list) : Result<Expr * Token list, string> =
        parseComparison toks
        |> Result.bind (fun (left, remaining) ->
            let rec parseBitAndRest (leftExpr: Expr) (toks: Token list) : Result<Expr * Token list, string> =
                match toks with
                | TBitAnd :: rest ->
                    parseComparison rest
                    |> Result.bind (fun (right, remaining') ->
                        parseBitAndRest (BinOp (BitAnd, leftExpr, right)) remaining')
                | _ -> Ok (leftExpr, toks)
            parseBitAndRest left remaining)

    and parseComparison (toks: Token list) : Result<Expr * Token list, string> =
        parseShift toks
        |> Result.bind (fun (left, remaining) ->
            // Comparison operators are non-associative (no chaining)
            match remaining with
            | TEqEq :: rest ->
                parseShift rest
                |> Result.map (fun (right, remaining') ->
                    (BinOp (Eq, left, right), remaining'))
            | TNeq :: rest ->
                parseShift rest
                |> Result.map (fun (right, remaining') ->
                    (BinOp (Neq, left, right), remaining'))
            | TLt :: rest ->
                parseShift rest
                |> Result.map (fun (right, remaining') ->
                    (BinOp (Lt, left, right), remaining'))
            | TGt :: rest ->
                parseShift rest
                |> Result.map (fun (right, remaining') ->
                    (BinOp (Gt, left, right), remaining'))
            | TLte :: rest ->
                parseShift rest
                |> Result.map (fun (right, remaining') ->
                    (BinOp (Lte, left, right), remaining'))
            | TGte :: rest ->
                parseShift rest
                |> Result.map (fun (right, remaining') ->
                    (BinOp (Gte, left, right), remaining'))
            | _ -> Ok (left, remaining))

    and parseShift (toks: Token list) : Result<Expr * Token list, string> =
        parseAdditive toks
        |> Result.bind (fun (left, remaining) ->
            let rec parseShiftRest (leftExpr: Expr) (toks: Token list) : Result<Expr * Token list, string> =
                match toks with
                | TShl :: rest ->
                    parseAdditive rest
                    |> Result.bind (fun (right, remaining') ->
                        parseShiftRest (BinOp (Shl, leftExpr, right)) remaining')
                | TShr :: rest ->
                    parseAdditive rest
                    |> Result.bind (fun (right, remaining') ->
                        parseShiftRest (BinOp (Shr, leftExpr, right)) remaining')
                | _ -> Ok (leftExpr, toks)
            parseShiftRest left remaining)

    and parseAdditive (toks: Token list) : Result<Expr * Token list, string> =
        parseMultiplicative toks
        |> Result.bind (fun (left, remaining) ->
            let rec parseAdditiveRest (leftExpr: Expr) (toks: Token list) : Result<Expr * Token list, string> =
                match toks with
                | TPlus :: rest ->
                    parseMultiplicative rest
                    |> Result.bind (fun (right, remaining') ->
                        parseAdditiveRest (BinOp (Add, leftExpr, right)) remaining')
                | TMinus :: rest ->
                    parseMultiplicative rest
                    |> Result.bind (fun (right, remaining') ->
                        parseAdditiveRest (BinOp (Sub, leftExpr, right)) remaining')
                | TPlusPlus :: rest ->
                    parseMultiplicative rest
                    |> Result.bind (fun (right, remaining') ->
                        parseAdditiveRest (BinOp (StringConcat, leftExpr, right)) remaining')
                | _ -> Ok (leftExpr, toks)
            parseAdditiveRest left remaining)

    and parseMultiplicative (toks: Token list) : Result<Expr * Token list, string> =
        parseUnary toks
        |> Result.bind (fun (left, remaining) ->
            let rec parseMultiplicativeRest (leftExpr: Expr) (toks: Token list) : Result<Expr * Token list, string> =
                match toks with
                | TStar :: rest ->
                    parseUnary rest
                    |> Result.bind (fun (right, remaining') ->
                        parseMultiplicativeRest (BinOp (Mul, leftExpr, right)) remaining')
                | TSlash :: rest ->
                    parseUnary rest
                    |> Result.bind (fun (right, remaining') ->
                        parseMultiplicativeRest (BinOp (Div, leftExpr, right)) remaining')
                | TPercent :: rest ->
                    parseUnary rest
                    |> Result.bind (fun (right, remaining') ->
                        parseMultiplicativeRest (BinOp (Mod, leftExpr, right)) remaining')
                | _ -> Ok (leftExpr, toks)
            parseMultiplicativeRest left remaining)

    and parseUnary (toks: Token list) : Result<Expr * Token list, string> =
        match toks with
        // Negative integer literals - parse directly as negative values
        | TMinus :: TInt64 n :: rest -> Ok (Int64Literal (-n), rest)
        | TMinus :: TInt8 n :: rest -> Ok (Int8Literal (-n), rest)
        | TMinus :: TInt16 n :: rest -> Ok (Int16Literal (-n), rest)
        | TMinus :: TInt32 n :: rest -> Ok (Int32Literal (-n), rest)
        | TMinus :: TFloat f :: rest -> Ok (FloatLiteral (-f), rest)
        | TMinus :: rest ->
            // For non-literal expressions, use UnaryOp
            parseUnary rest
            |> Result.map (fun (expr, remaining) -> (UnaryOp (Neg, expr), remaining))
        | TNot :: rest ->
            parseUnary rest
            |> Result.map (fun (expr, remaining) -> (UnaryOp (Not, expr), remaining))
        | TBitNot :: rest ->
            parseUnary rest
            |> Result.map (fun (expr, remaining) -> (UnaryOp (BitNot, expr), remaining))
        | _ ->
            parsePrimary toks

    and parsePrimary (toks: Token list) : Result<Expr * Token list, string> =
        // Parse a primary expression, then handle postfix operations and
        // interpreter-style space application: f x y
        parsePrimaryBase toks
        |> Result.bind (fun (expr, remaining) ->
            parsePostfix expr remaining
            |> Result.bind (fun (postfixExpr, remaining') ->
                parseApplication postfixExpr remaining'))

    and parseApplication (callee: Expr) (toks: Token list) : Result<Expr * Token list, string> =
        let negativeNumericArg = canStartNegativeNumericApplicationArg callee toks
        if negativeNumericArg || canStartApplicationArg toks then
            // Parse exactly one argument, then continue left-associatively.
            // Negative numeric literals are tokenized as `-` + literal.
            // Parse those with unary parsing so expressions like `f -1.0` become
            // function application rather than subtraction from `f`.
            let parseOneArg () : Result<Expr * Token list, string> =
                if negativeNumericArg then
                    parseUnary toks
                else
                    parsePrimaryBase toks
                    |> Result.bind (fun (argBaseExpr, afterArgBase) ->
                        parsePostfix argBaseExpr afterArgBase)

            parseOneArg ()
            |> Result.bind (fun (argExpr, afterArg) ->
                let applied = appendCallArg callee argExpr
                parseApplication applied afterArg)
        else
            Ok (callee, toks)

    // Parse a qualified identifier chain: Stdlib.Int64.add
    // Returns the full qualified name and remaining tokens
    and parseQualifiedIdent (firstName: string) (toks: Token list) : string * Token list =
        match toks with
        | TDot :: TIdent nextName :: rest ->
            // Continue the chain: firstName.nextName...
            let (fullName, remaining) = parseQualifiedIdent nextName rest
            (firstName + "." + fullName, remaining)
        | _ ->
            // End of chain
            (firstName, toks)

    and parsePrimaryBase (toks: Token list) : Result<Expr * Token list, string> =
        match toks with
        | TInt64 n :: rest -> Ok (Int64Literal n, rest)
        | TInt8 n :: rest -> Ok (Int8Literal n, rest)
        | TInt16 n :: rest -> Ok (Int16Literal n, rest)
        | TInt32 n :: rest -> Ok (Int32Literal n, rest)
        | TUInt8 n :: rest -> Ok (UInt8Literal n, rest)
        | TUInt16 n :: rest -> Ok (UInt16Literal n, rest)
        | TUInt32 n :: rest -> Ok (UInt32Literal n, rest)
        | TUInt64 n :: rest -> Ok (UInt64Literal n, rest)
        | TFloat f :: rest -> Ok (FloatLiteral f, rest)
        | TStringLit s :: rest -> Ok (StringLiteral s, rest)
        | TCharLit s :: rest -> Ok (CharLiteral s, rest)
        | TInterpString parts :: rest ->
            // Parse interpolated string into AST.InterpolatedString
            let rec parseInterpParts (parts: InterpPart list) (acc: AST.StringPart list) : Result<AST.StringPart list, string> =
                match parts with
                | [] -> Ok (List.rev acc)
                | InterpText s :: remaining ->
                    parseInterpParts remaining (AST.StringText s :: acc)
                | InterpTokens tokens :: remaining ->
                    // Parse the tokens as an expression
                    match parseExpr (tokens @ [TEOF]) with
                    | Ok (expr, [TEOF]) ->
                        parseInterpParts remaining (AST.StringExpr expr :: acc)
                    | Ok (_, leftover) ->
                        Error $"Unexpected tokens after interpolated expression: {leftover}"
                    | Error err ->
                        Error $"Error parsing interpolated expression: {err}"
            match parseInterpParts parts [] with
            | Ok astParts -> Ok (InterpolatedString astParts, rest)
            | Error err -> Error err
        | TTrue :: rest -> Ok (BoolLiteral true, rest)
        | TFalse :: rest -> Ok (BoolLiteral false, rest)
        | TFun :: rest ->
            // Interpreter lambda syntax: fun x y -> body
            let lambdaSeed = List.length rest

            let rec parseFunParameters
                (toks: Token list)
                (paramIndex: int)
                (acc: (string * Type) list)
                : Result<(string * Type) list * Token list, string> =
                match toks with
                | TArrow :: remaining when not (List.isEmpty acc) ->
                    Ok (List.rev acc, remaining)
                | TIdent name :: remaining when not (System.Char.IsUpper(name.[0])) ->
                    let typeVarName = $"__interp_lambda_{lambdaSeed}_{paramIndex}_{name}"
                    parseFunParameters
                        remaining
                        (paramIndex + 1)
                        ((name, TVar typeVarName) :: acc)
                | TLParen :: TIdent name :: TColon :: remaining when not (System.Char.IsUpper(name.[0])) ->
                    parseType remaining
                    |> Result.bind (fun (paramType, afterType) ->
                        match afterType with
                        | TRParen :: remaining' ->
                            parseFunParameters
                                remaining'
                                (paramIndex + 1)
                                ((name, paramType) :: acc)
                        | _ -> Error "Expected ')' after typed lambda parameter")
                | _ -> Error "Expected one or more parameters before '->' in fun expression"

            parseFunParameters rest 0 []
            |> Result.bind (fun (parameters, bodyStart) ->
                parseExpr bodyStart
                |> Result.map (fun (body, remaining) ->
                    (Lambda (parameters, body), remaining)))
        // Qualified identifier: Stdlib.Int64.add, Module.func, or Stdlib.Result.Result.Ok
        | TIdent name :: TDot :: TIdent nextName :: rest when System.Char.IsUpper(name.[0]) ->
            // Parse the full qualified name
            let (qualifiedTail, afterQualified) = parseQualifiedIdent nextName rest
            let fullName = name + "." + qualifiedTail
            // Check if the last segment is uppercase (constructor) or lowercase (function)
            let lastDotIdx = fullName.LastIndexOf('.')
            let lastSegment = if lastDotIdx >= 0 then fullName.Substring(lastDotIdx + 1) else fullName
            let isConstructor = System.Char.IsUpper(lastSegment.[0])
            // Check what follows - function call, constructor, or variable reference
            match afterQualified with
            | _ when isConstructor ->
                let typeName = fullName.Substring(0, lastDotIdx)
                let variantName = lastSegment
                if canStartApplicationArg afterQualified then
                    // Qualified constructor with interpreter-style payload:
                    // Stdlib.Option.Option.Some 5L
                    parsePrimaryBase afterQualified
                    |> Result.bind (fun (payloadBase, afterPayloadBase) ->
                        parsePostfix payloadBase afterPayloadBase
                        |> Result.map (fun (payloadExpr, remaining) ->
                            (Constructor (typeName, variantName, Some payloadExpr), remaining)))
                else
                    // Qualified constructor without payload: Stdlib.Color.Red
                    Ok (Constructor (typeName, variantName, None), afterQualified)
            | TLParen :: TRParen :: rest ->
                // Qualified zero-arg call: Stdlib.Module.fn()
                Ok (Call (fullName, []), rest)
            | TLt :: typeArgsStart ->
                // Qualified generic function call: Stdlib.List.length<t>(args)
                // Accept whenever we can successfully parse a type argument list.
                let looksLikeTypeArgs tokens =
                    match parseTypeArgs tokens [] with
                    | Ok _ -> true
                    | Error _ -> false
                if looksLikeTypeArgs typeArgsStart then
                    parseTypeArgs typeArgsStart []
                    |> Result.map (fun (typeArgs, afterTypes) ->
                        // Interpreter parser always expects argument application to be
                        // space-based (handled by parseApplication).
                        (TypeApp (fullName, typeArgs, []), afterTypes))
                else
                    // Not type args, treat as variable reference and leave < for comparison
                    Ok (Var fullName, TLt :: typeArgsStart)
            | _ ->
                // Qualified variable reference (function as value)
                Ok (Var fullName, afterQualified)
        | TIdent name :: TLParen :: TRParen :: rest when not (System.Char.IsUpper(name.[0])) ->
            // Zero-arg call: fn()
            Ok (Call (name, []), rest)
        | TIdent name :: TLt :: rest when not (System.Char.IsUpper(name.[0])) ->
            // Could be generic function call: name<type, ...>(args)
            // Or could be comparison: name < expr
            // Disambiguate by checking whether a full type argument list parses.
            let looksLikeGenericCall tokens =
                match parseTypeArgs tokens [] with
                | Ok _ -> true
                | Error _ -> false
            if looksLikeGenericCall rest then
                // Parse as generic call
                parseTypeArgs rest []
                |> Result.map (fun (typeArgs, afterTypes) ->
                    // Interpreter parser always expects argument application to be
                    // space-based (handled by parseApplication).
                    (TypeApp (name, typeArgs, []), afterTypes))
            else
                // Not a type application, treat name as variable and let comparison parsing handle <
                Ok (Var name, TLt :: rest)
        | TIdent typeName :: TLBrace :: rest when System.Char.IsUpper(typeName.[0]) ->
            // Record literal with type name: Point { x = 1, y = 2 }
            parseRecordLiteralFieldsWithTypeName typeName rest []
        | TIdent name :: rest when System.Char.IsUpper(name.[0]) ->
            // Constructor, optionally with interpreter-style payload: Some 5L
            if canStartApplicationArg rest then
                parsePrimaryBase rest
                |> Result.bind (fun (payloadBaseExpr, afterPayloadBase) ->
                    parsePostfix payloadBaseExpr afterPayloadBase
                    |> Result.map (fun (payloadExpr, remaining) ->
                        (Constructor ("", name, Some payloadExpr), remaining)))
            else
                Ok (Constructor ("", name, None), rest)
        | TIdent name :: rest ->
            // Variable reference (lowercase identifier)
            Ok (Var name, rest)
        | TLParen :: TRParen :: rest ->
            // Unit literal: ()
            Ok (UnitLiteral, rest)
        | TLParen :: rest ->
            // Could be parenthesized expression, tuple literal, or operator section
            // Check for operator section: (&&), (||), (+), (-), (*), (/), etc.
            match rest with
            | TAnd :: TRParen :: afterOp ->
                // (&&) - operator section, parse the right operand
                parseUnary afterOp
                |> Result.map (fun (rightArg, remaining) ->
                    // Create lambda: \$x -> $x && rightArg
                    let lambda = Lambda ([("$pipe_arg", TBool)], BinOp (And, Var "$pipe_arg", rightArg))
                    (lambda, remaining))
            | TOr :: TRParen :: afterOp ->
                // (||) - operator section
                parseUnary afterOp
                |> Result.map (fun (rightArg, remaining) ->
                    let lambda = Lambda ([("$pipe_arg", TBool)], BinOp (Or, Var "$pipe_arg", rightArg))
                    (lambda, remaining))
            | _ ->
                parseExpr rest
                |> Result.bind (fun (firstExpr, remaining) ->
                    match remaining with
                    | TRParen :: rest' ->
                        // Parenthesized expression (single element)
                        Ok (firstExpr, rest')
                    | TComma :: rest' ->
                        // Tuple literal: (expr, expr, ...)
                        parseTupleElements rest' [firstExpr]
                    | _ -> Error "Expected ')' or ',' in tuple/parenthesized expression")
        | TLBrace :: rest ->
            // Check for record update syntax: { record with field = value, ... }
            // First, try to parse an expression, then check for 'with'
            parseExpr rest
            |> Result.bind (fun (recordExpr, afterExpr) ->
                match afterExpr with
                | TWith :: afterWith ->
                    // Record update: { record with field = value, ... }
                    parseRecordUpdateFields afterWith []
                    |> Result.map (fun (updates, remaining) ->
                        (RecordUpdate (recordExpr, updates), remaining))
                | _ -> Error "Record update requires 'with' keyword: use '{ record with field = value, ... }'")
        | TLBracket :: rest ->
            // List literal: [1L; 2L; 3L] or []
            parseListLiteralElements rest []
        | _ -> Error "Expected expression"

    and parseTupleElements (toks: Token list) (acc: Expr list) : Result<Expr * Token list, string> =
        // Parse remaining tuple elements after the first comma
        parseExpr toks
        |> Result.bind (fun (expr, remaining) ->
            match remaining with
            | TComma :: rest ->
                // More elements
                parseTupleElements rest (expr :: acc)
            | TRParen :: rest ->
                // End of tuple
                let elements = List.rev (expr :: acc)
                Ok (TupleLiteral elements, rest)
            | _ -> Error "Expected ',' or ')' in tuple literal")

    and parseRecordLiteralFieldsWithTypeName (typeName: string) (toks: Token list) (acc: (string * Expr) list) : Result<Expr * Token list, string> =
        // Parse record literal fields with explicit type name: TypeName { name = expr, ... }
        match toks with
        | TRBrace :: rest ->
            // Empty record or end of fields
            Ok (RecordLiteral (typeName, List.rev acc), rest)
        | TIdent fieldName :: TEquals :: rest ->
            parseExpr rest
            |> Result.bind (fun (value, remaining) ->
                match remaining with
                | TComma :: rest' ->
                    // More fields
                    parseRecordLiteralFieldsWithTypeName typeName rest' ((fieldName, value) :: acc)
                | TRBrace :: rest' ->
                    // End of record
                    Ok (RecordLiteral (typeName, List.rev ((fieldName, value) :: acc)), rest')
                | _ -> Error "Expected ',' or '}' after record field value")
        | _ -> Error "Expected field name in record literal"

    and parseRecordUpdateFields (toks: Token list) (acc: (string * Expr) list) : Result<(string * Expr) list * Token list, string> =
        // Parse record update fields: field = expr, field = expr, ... }
        match toks with
        | TRBrace :: rest ->
            // End of fields
            Ok (List.rev acc, rest)
        | TIdent fieldName :: TEquals :: rest ->
            parseExpr rest
            |> Result.bind (fun (value, remaining) ->
                match remaining with
                | TComma :: rest' ->
                    // More fields
                    parseRecordUpdateFields rest' ((fieldName, value) :: acc)
                | TRBrace :: rest' ->
                    // End of record update
                    Ok (List.rev ((fieldName, value) :: acc), rest')
                | _ -> Error "Expected ',' or '}' after record update field value")
        | _ -> Error "Expected field name in record update"

    and parseListLiteralElements (toks: Token list) (acc: Expr list) : Result<Expr * Token list, string> =
        // Parse list literal elements:
        // [expr; expr; ...] or [] or [a; b; ...rest]
        match toks with
        | TRBracket :: rest ->
            // Empty list or end of list
            Ok (ListLiteral (List.rev acc), rest)
        | TDotDotDot :: rest ->
            // Spread at start: [...tail]
            parseExpr rest
            |> Result.bind (fun (tailExpr, remaining) ->
                match remaining with
                | TRBracket :: rest' ->
                    Ok (ListCons (List.rev acc, tailExpr), rest')
                | _ -> Error "Expected ']' after spread expression")
        | _ ->
            parseExpr toks
            |> Result.bind (fun (expr, remaining) ->
                match remaining with
                | TSemicolon :: TDotDotDot :: rest ->
                    // Spread after elements: [a, b, ...tail]
                    parseExpr rest
                    |> Result.bind (fun (tailExpr, remaining') ->
                        match remaining' with
                        | TRBracket :: rest' ->
                            Ok (ListCons (List.rev (expr :: acc), tailExpr), rest')
                        | _ -> Error "Expected ']' after spread expression")
                | TSemicolon :: rest ->
                    // More elements
                    parseListLiteralElements rest (expr :: acc)
                | TRBracket :: rest ->
                    // End of list
                    Ok (ListLiteral (List.rev (expr :: acc)), rest)
                | _ -> Error "Expected ';' or ']' in list literal")

    and parsePostfix (expr: Expr) (toks: Token list) : Result<Expr * Token list, string> =
        // Handle postfix operations: tuple access (.0, .1), field access (.fieldName),
        // and optional parenthesized call arguments.
        match toks with
        | TDot :: TInt64 index :: rest ->
            if index < 0L then
                Error "Tuple index cannot be negative"
            else
                let accessExpr = TupleAccess (expr, int index)
                parsePostfix accessExpr rest
        | TDot :: TIdent fieldName :: rest ->
            // Record field access
            let accessExpr = RecordAccess (expr, fieldName)
            parsePostfix accessExpr rest
        | TLParen :: rest ->
            // Optional call syntax for interpreter compatibility:
            // f(a, b), g(), (fun x -> x)(1)
            // Keep existing `f (x)` behavior for single parenthesized args.
            let rec hasTopLevelComma (depth: int) (ts: Token list) : bool =
                match ts with
                | [] -> false
                | TComma :: _ when depth = 0 -> true
                | TLParen :: more -> hasTopLevelComma (depth + 1) more
                | TRParen :: _ when depth = 0 -> false
                | TRParen :: more -> hasTopLevelComma (depth - 1) more
                | _ :: more -> hasTopLevelComma depth more
            let shouldUseCallSyntax =
                match rest with
                | TRParen :: _ -> true
                | _ -> hasTopLevelComma 0 rest
            if shouldUseCallSyntax then
                match expr with
                | Var _ ->
                    // Keep `f (x)` as application with a parenthesized argument.
                    Ok (expr, toks)
                | _ ->
                    parseCallArgs rest []
                    |> Result.bind (fun (args, remaining) ->
                        let appliedExpr =
                            match expr with
                            | Call (funcName, existingArgs) ->
                                Call (funcName, existingArgs @ args)
                            | TypeApp (funcName, typeArgs, existingArgs) ->
                                TypeApp (funcName, typeArgs, existingArgs @ args)
                            | Constructor (typeName, variantName, None) ->
                                match args with
                                | [singleArg] ->
                                    Constructor (typeName, variantName, Some singleArg)
                                | _ ->
                                    Apply (expr, args)
                            | _ ->
                                Apply (expr, args)
                        parsePostfix appliedExpr remaining)
            else
                Ok (expr, toks)
        | _ -> Ok (expr, toks)

    and parseCallArgs (toks: Token list) (acc: Expr list) : Result<Expr list * Token list, string> =
        match toks with
        | TRParen :: rest ->
            // End of argument list (including zero-arg calls).
            Ok (List.rev acc, rest)
        | _ ->
            parseExpr toks
            |> Result.bind (fun (argExpr, remaining) ->
                match remaining with
                | TComma :: rest ->
                    parseCallArgs rest (argExpr :: acc)
                | TRParen :: rest ->
                    Ok (List.rev (argExpr :: acc), rest)
                | _ -> Error "Expected ',' or ')' after function argument")

    // Parse top-level elements (functions or expressions)
    let rec parseTopLevels (toks: Token list) (acc: TopLevel list) : Result<Program, string> =
        match toks with
        | TSemicolon :: rest ->
            // Optional top-level separator in interpreter pretty-printed programs.
            parseTopLevels rest acc
        | TEOF :: [] ->
            // End of input
            if List.isEmpty acc then
                Error "Empty program"
            else
                Ok (Program (List.rev acc))

        | TDef :: _ ->
            // Parse function definition
            parseFunctionDef toks parseExpr
            |> Result.bind (fun (funcDef, remaining) ->
                parseTopLevels remaining (FunctionDef funcDef :: acc))

        | TLet :: TIdent firstName :: TLParen :: rest ->
            // Interpreter-style top-level function definition:
            // let name(args) : ReturnType = body
            parseFunctionDef (TDef :: TIdent firstName :: TLParen :: rest) parseExpr
            |> Result.bind (fun (funcDef, remaining) ->
                parseTopLevels remaining (FunctionDef funcDef :: acc))

        | TLet :: TIdent firstName :: TLt :: rest ->
            // Interpreter-style generic top-level function definition:
            // let name<t>(args) : ReturnType = body
            parseFunctionDef (TDef :: TIdent firstName :: TLt :: rest) parseExpr
            |> Result.bind (fun (funcDef, remaining) ->
                parseTopLevels remaining (FunctionDef funcDef :: acc))

        | TType :: _ ->
            // Parse type definition
            parseTypeDef toks
            |> Result.bind (fun (typeDef, remaining) ->
                parseTopLevels remaining (TypeDef typeDef :: acc))

        | _ ->
            // Parse expression
            parseExpr toks
            |> Result.bind (fun (expr, remaining) ->
                match remaining with
                | TEOF :: [] ->
                    // Single expression program
                    Ok (Program (List.rev (Expression expr :: acc)))
                | _ ->
                    // More top-level definitions after expression not allowed for now
                    Error "Unexpected tokens after expression (only function definitions can be followed by more definitions)")

    parseTopLevels tokens []

let private isInternalIdentifier (name: string) : bool =
    name.StartsWith("__") || name.Contains(".__")

let private validateNoInternalIdentifier (name: string) : Result<unit, string> =
    if isInternalIdentifier name then
        Error $"Internal identifier not allowed in user code: {name}"
    else
        Ok ()

let rec private validatePattern (pattern: Pattern) : Result<unit, string> =
    match pattern with
    | PVar name -> validateNoInternalIdentifier name
    | PConstructor (_, payload) ->
        match payload with
        | None -> Ok ()
        | Some inner -> validatePattern inner
    | PTuple patterns ->
        patterns
        |> List.fold (fun acc p -> Result.bind (fun () -> validatePattern p) acc) (Ok ())
    | PRecord (_, fields) ->
        fields
        |> List.fold (fun acc (_, p) -> Result.bind (fun () -> validatePattern p) acc) (Ok ())
    | PList patterns ->
        patterns
        |> List.fold (fun acc p -> Result.bind (fun () -> validatePattern p) acc) (Ok ())
    | PListCons (head, tail) ->
        let headResult =
            head |> List.fold (fun acc p -> Result.bind (fun () -> validatePattern p) acc) (Ok ())
        Result.bind (fun () -> validatePattern tail) headResult
    | PUnit
    | PWildcard
    | PInt64 _
    | PInt8Literal _
    | PInt16Literal _
    | PInt32Literal _
    | PUInt8Literal _
    | PUInt16Literal _
    | PUInt32Literal _
    | PUInt64Literal _
    | PBool _
    | PString _
    | PFloat _ -> Ok ()

let rec private validateExpr (expr: Expr) : Result<unit, string> =
    match expr with
    | Let (name, value, body) ->
        validateNoInternalIdentifier name
        |> Result.bind (fun () -> validateExpr value)
        |> Result.bind (fun () -> validateExpr body)
    | Var name -> validateNoInternalIdentifier name
    | Call (funcName, args) ->
        validateNoInternalIdentifier funcName
        |> Result.bind (fun () ->
            args |> List.fold (fun acc arg -> Result.bind (fun () -> validateExpr arg) acc) (Ok ()))
    | TypeApp (funcName, _, args) ->
        validateNoInternalIdentifier funcName
        |> Result.bind (fun () ->
            args |> List.fold (fun acc arg -> Result.bind (fun () -> validateExpr arg) acc) (Ok ()))
    | InterpolatedString parts ->
        parts
        |> List.fold (fun acc part ->
            match part with
            | StringText _ -> acc
            | StringExpr inner -> Result.bind (fun () -> validateExpr inner) acc) (Ok ())
    | BinOp (_, left, right) ->
        validateExpr left |> Result.bind (fun () -> validateExpr right)
    | UnaryOp (_, inner) -> validateExpr inner
    | If (cond, thenBranch, elseBranch) ->
        validateExpr cond
        |> Result.bind (fun () -> validateExpr thenBranch)
        |> Result.bind (fun () -> validateExpr elseBranch)
    | TupleLiteral elems ->
        elems |> List.fold (fun acc e -> Result.bind (fun () -> validateExpr e) acc) (Ok ())
    | TupleAccess (tupleExpr, _) -> validateExpr tupleExpr
    | RecordLiteral (_, fields) ->
        fields |> List.fold (fun acc (_, e) -> Result.bind (fun () -> validateExpr e) acc) (Ok ())
    | RecordUpdate (recordExpr, updates) ->
        validateExpr recordExpr
        |> Result.bind (fun () ->
            updates |> List.fold (fun acc (_, e) -> Result.bind (fun () -> validateExpr e) acc) (Ok ()))
    | RecordAccess (recordExpr, _) -> validateExpr recordExpr
    | Constructor (_, _, payload) ->
        match payload with
        | None -> Ok ()
        | Some inner -> validateExpr inner
    | Match (scrutinee, cases) ->
        let validateCase (case: MatchCase) : Result<unit, string> =
            let patternsResult =
                case.Patterns
                |> NonEmptyList.toList
                |> List.fold (fun acc p -> Result.bind (fun () -> validatePattern p) acc) (Ok ())
            patternsResult
            |> Result.bind (fun () ->
                match case.Guard with
                | None -> Ok ()
                | Some guardExpr -> validateExpr guardExpr)
            |> Result.bind (fun () -> validateExpr case.Body)
        validateExpr scrutinee
        |> Result.bind (fun () ->
            cases |> List.fold (fun acc case -> Result.bind (fun () -> validateCase case) acc) (Ok ()))
    | ListLiteral elems ->
        elems |> List.fold (fun acc e -> Result.bind (fun () -> validateExpr e) acc) (Ok ())
    | ListCons (head, tail) ->
        let headResult = head |> List.fold (fun acc e -> Result.bind (fun () -> validateExpr e) acc) (Ok ())
        Result.bind (fun () -> validateExpr tail) headResult
    | Lambda (parameters, body) ->
        parameters
        |> List.fold (fun acc (name, _) -> Result.bind (fun () -> validateNoInternalIdentifier name) acc) (Ok ())
        |> Result.bind (fun () -> validateExpr body)
    | Apply (funcExpr, args) ->
        validateExpr funcExpr
        |> Result.bind (fun () ->
            args |> List.fold (fun acc e -> Result.bind (fun () -> validateExpr e) acc) (Ok ()))
    | FuncRef funcName -> validateNoInternalIdentifier funcName
    | Closure (funcName, captures) ->
        validateNoInternalIdentifier funcName
        |> Result.bind (fun () ->
            captures |> List.fold (fun acc e -> Result.bind (fun () -> validateExpr e) acc) (Ok ()))
    | UnitLiteral | Int64Literal _ | Int8Literal _ | Int16Literal _ | Int32Literal _
    | UInt8Literal _ | UInt16Literal _ | UInt32Literal _ | UInt64Literal _
    | BoolLiteral _ | StringLiteral _ | CharLiteral _ | FloatLiteral _ -> Ok ()

let private validateNoInternalIdentifiers (Program items) : Result<Program, string> =
    let validateTopLevel (item: TopLevel) : Result<unit, string> =
        match item with
        | FunctionDef def ->
            validateNoInternalIdentifier def.Name
            |> Result.bind (fun () ->
                def.Params
                |> List.fold (fun acc (name, _) -> Result.bind (fun () -> validateNoInternalIdentifier name) acc) (Ok ()))
            |> Result.bind (fun () -> validateExpr def.Body)
        | TypeDef _ -> Ok ()
        | Expression expr -> validateExpr expr
    items
    |> List.fold (fun acc item -> Result.bind (fun () -> validateTopLevel item) acc) (Ok ())
    |> Result.map (fun () -> Program items)

/// Parse a string directly to AST
let parseString (allowInternal: bool) (input: string) : Result<Program, string> =
    lex input
    |> Result.bind parse
    |> Result.bind (fun program ->
        if allowInternal then Ok program
        else validateNoInternalIdentifiers program)
