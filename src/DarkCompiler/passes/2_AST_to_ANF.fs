// 2_AST_to_ANF.fs - ANF Transformation (Pass 2)
//
// Transforms AST into A-Normal Form (ANF).
//
// Algorithm:
// - Recursively processes nested expressions
// - Converts complex operands to atoms (literals or variables)
// - Introduces let-bindings for intermediate computations
// - Uses VarGen for generating fresh temporary variable names
//
// Example:
//   BinOp(Add, Int64Literal(2), BinOp(Mul, Int64Literal(3), Int64Literal(4)))
//   →
//   let tmp0 = 3; let tmp1 = 4; let tmp2 = tmp0 * tmp1;
//   let tmp3 = 2; let tmp4 = tmp3 + tmp2; return tmp4

module AST_to_ANF

open ANF
open Output

/// Convert AST.Type to a string for specialization keys
let rec typeToString (ty: AST.Type) : string =
    match ty with
    | AST.TInt64 -> "i64"
    | AST.TInt32 -> "i32"
    | AST.TInt16 -> "i16"
    | AST.TInt8 -> "i8"
    | AST.TUInt64 -> "u64"
    | AST.TUInt32 -> "u32"
    | AST.TUInt16 -> "u16"
    | AST.TUInt8 -> "u8"
    | AST.TBool -> "bool"
    | AST.TString -> "str"
    | AST.TBytes -> "bytes"
    | AST.TChar -> "char"
    | AST.TFloat64 -> "f64"
    | AST.TUnit -> "unit"
    | AST.TRawPtr -> "ptr"
    | AST.TVar name -> name
    | AST.TRecord (name, args) -> name + (if List.isEmpty args then "" else "<" + (args |> List.map typeToString |> String.concat ",") + ">")
    | AST.TSum (name, args) -> name + "<" + (args |> List.map typeToString |> String.concat ",") + ">"
    | AST.TList elemType -> "List<" + typeToString elemType + ">"
    | AST.TDict (keyType, valueType) -> "Dict<" + typeToString keyType + "," + typeToString valueType + ">"
    | AST.TFunction (paramTypes, retType) ->
        "(" + (paramTypes |> List.map typeToString |> String.concat ",") + ")->" + typeToString retType
    | AST.TTuple types -> "(" + (types |> List.map typeToString |> String.concat "*") + ")"

/// Convert a literal pattern into an ANF sized integer
let patternLiteralToSizedInt (pattern: AST.Pattern) : ANF.SizedInt option =
    match pattern with
    | AST.PInt64 n -> Some (ANF.Int64 n)
    | AST.PInt8Literal n -> Some (ANF.Int8 n)
    | AST.PInt16Literal n -> Some (ANF.Int16 n)
    | AST.PInt32Literal n -> Some (ANF.Int32 n)
    | AST.PUInt8Literal n -> Some (ANF.UInt8 n)
    | AST.PUInt16Literal n -> Some (ANF.UInt16 n)
    | AST.PUInt32Literal n -> Some (ANF.UInt32 n)
    | AST.PUInt64Literal n -> Some (ANF.UInt64 n)
    | _ -> None

/// Try to convert a function call to a file I/O intrinsic CExpr
/// Returns Some CExpr if it's a file intrinsic, None otherwise
let tryFileIntrinsic (funcName: string) (args: ANF.Atom list) : ANF.CExpr option =
    match funcName, args with
    | "Stdlib.File.readText", [pathAtom] ->
        Some (ANF.FileReadText pathAtom)
    | "Stdlib.File.exists", [pathAtom] ->
        Some (ANF.FileExists pathAtom)
    | "Stdlib.File.writeText", [pathAtom; contentAtom] ->
        Some (ANF.FileWriteText (pathAtom, contentAtom))
    | "Stdlib.File.appendText", [pathAtom; contentAtom] ->
        Some (ANF.FileAppendText (pathAtom, contentAtom))
    | "Stdlib.File.delete", [pathAtom] ->
        Some (ANF.FileDelete pathAtom)
    | "Stdlib.File.setExecutable", [pathAtom] ->
        Some (ANF.FileSetExecutable pathAtom)
    | "Stdlib.File.writeFromPtr", [pathAtom; ptrAtom; lengthAtom] ->
        Some (ANF.FileWriteFromPtr (pathAtom, ptrAtom, lengthAtom))
    | _ -> None

/// Try to convert a function call to a Float intrinsic CExpr
/// Returns Some CExpr if it's a Float intrinsic, None otherwise
let tryFloatIntrinsic (funcName: string) (args: ANF.Atom list) : ANF.CExpr option =
    match funcName, args with
    | "Stdlib.Float.sqrt", [xAtom] ->
        Some (ANF.FloatSqrt xAtom)
    | "Stdlib.Float.abs", [xAtom] ->
        Some (ANF.FloatAbs xAtom)
    | "Stdlib.Float.negate", [xAtom] ->
        Some (ANF.FloatNeg xAtom)
    | "Stdlib.Float.toInt", [xAtom] ->
        Some (ANF.FloatToInt64 xAtom)
    | "Stdlib.Int64.toFloat", [xAtom] ->
        Some (ANF.Int64ToFloat xAtom)
    // NOTE: Float.toString is now implemented in Dark, not as an intrinsic
    | "Stdlib.Float.toBits", [xAtom] ->
        Some (ANF.FloatToBits xAtom)
    | _ -> None

/// Try to constant-fold platform/path intrinsics at compile time
/// Returns Some CExpr if it's a constant-foldable intrinsic, None otherwise
let tryConstantFoldIntrinsic (funcName: string) (args: ANF.Atom list) : ANF.CExpr option =
    match funcName, args with
    | "Stdlib.Platform.isMacOS", [] ->
        // Constant-fold based on target platform using .NET runtime detection
        let isMac = System.Runtime.InteropServices.RuntimeInformation.IsOSPlatform(
            System.Runtime.InteropServices.OSPlatform.OSX)
        Some (ANF.Atom (ANF.BoolLiteral isMac))
    | "Stdlib.Platform.isLinux", [] ->
        let isLinux = System.Runtime.InteropServices.RuntimeInformation.IsOSPlatform(
            System.Runtime.InteropServices.OSPlatform.Linux)
        Some (ANF.Atom (ANF.BoolLiteral isLinux))
    | "Stdlib.Path.tempDir", [] ->
        // Both macOS and Linux use /tmp
        Some (ANF.Atom (ANF.StringLiteral "/tmp"))
    | _ -> None

/// Try to convert a function call to a raw memory intrinsic CExpr
/// These are internal-only functions for implementing HAMT data structures
/// Returns Some CExpr if it's a raw memory intrinsic, None otherwise
/// Note: __raw_get and __raw_set are generic and become monomorphized names like
/// __raw_get_i64, __raw_get_str, __raw_set_i64, etc.
let tryRawMemoryIntrinsic (funcName: string) (args: ANF.Atom list) : ANF.CExpr option =
    match funcName, args with
    | "__raw_alloc", [numBytesAtom] ->
        Some (ANF.RawAlloc numBytesAtom)
    | "__raw_free", [ptrAtom] ->
        Some (ANF.RawFree ptrAtom)
    | "__raw_get_byte", [ptrAtom; offsetAtom] ->
        // Read single byte at offset, returns Int64 (zero-extended)
        // IMPORTANT: Must come before the generic __raw_get_* pattern
        Some (ANF.RawGetByte (ptrAtom, offsetAtom))
    | name, [ptrAtom; offsetAtom] when name = "__raw_get" || name.StartsWith("__raw_get_") ->
        // Generic __raw_get<v> monomorphizes to __raw_get_i64, __raw_get_str, __raw_get_f64, etc.
        // Track Float type so we can convert GP bits to FP register
        // IMPORTANT: Only detect _f64 when it's the ENTIRE suffix (not list_f64 or other containers)
        let valueType = if name = "__raw_get_f64" then Some AST.TFloat64 else None
        Some (ANF.RawGet (ptrAtom, offsetAtom, valueType))
    | "__raw_set_byte", [ptrAtom; offsetAtom; valueAtom] ->
        // Write single byte at offset
        // IMPORTANT: Must come before the generic __raw_set_* pattern
        Some (ANF.RawSetByte (ptrAtom, offsetAtom, valueAtom))
    | name, [ptrAtom; offsetAtom; valueAtom] when name = "__raw_set" || name.StartsWith("__raw_set_") ->
        // Generic __raw_set<v> monomorphizes to __raw_set_i64, __raw_set_str, __raw_set_f64, etc.
        // Track Float type so we can convert FP register to GP bits for storage
        // IMPORTANT: Only detect _f64 when it's the ENTIRE suffix (not list_f64 or other containers)
        let valueType = if name = "__raw_set_f64" then Some AST.TFloat64 else None
        Some (ANF.RawSet (ptrAtom, offsetAtom, valueAtom, valueType))
    // Cast operations are no-ops at runtime - just pass through the value
    | "__rawptr_to_int64", [ptrAtom] ->
        Some (ANF.Atom ptrAtom)
    | "__int64_to_rawptr", [intAtom] ->
        Some (ANF.Atom intAtom)
    // String refcount intrinsics (for Dict with string keys)
    | "__refcount_inc_string", [strAtom] ->
        Some (ANF.RefCountIncString strAtom)
    | "__refcount_dec_string", [strAtom] ->
        Some (ANF.RefCountDecString strAtom)
    // String pointer cast operations are no-ops at runtime - just pass through the value
    | "__string_to_int64", [strAtom] ->
        Some (ANF.Atom strAtom)
    | "__int64_to_string", [intAtom] ->
        Some (ANF.Atom intAtom)

    // Bytes pointer cast operations are no-ops at runtime - just pass through the value
    | "__bytes_to_int64", [bytesAtom] ->
        Some (ANF.Atom bytesAtom)
    | "__int64_to_bytes", [intAtom] ->
        Some (ANF.Atom intAtom)

    // Dict intrinsics - for type-safe Dict<k, v> operations
    // __empty_dict<k, v> returns 0 (null pointer)
    | name, [] when name = "__empty_dict" || name.StartsWith("__empty_dict_") ->
        Some (ANF.Atom (ANF.IntLiteral (ANF.Int64 0L)))
    // __dict_is_null<k, v> checks if pointer is 0
    | name, [dictAtom] when name = "__dict_is_null" || name.StartsWith("__dict_is_null_") ->
        Some (ANF.Prim (ANF.Eq, dictAtom, ANF.IntLiteral (ANF.Int64 0L)))
    // __dict_get_tag<k, v> extracts low 2 bits (dict & 3)
    | name, [dictAtom] when name = "__dict_get_tag" || name.StartsWith("__dict_get_tag_") ->
        Some (ANF.Prim (ANF.BitAnd, dictAtom, ANF.IntLiteral (ANF.Int64 3L)))
    // __dict_to_rawptr<k, v> clears tag bits (dict & -4)
    | name, [dictAtom] when name = "__dict_to_rawptr" || name.StartsWith("__dict_to_rawptr_") ->
        Some (ANF.Prim (ANF.BitAnd, dictAtom, ANF.IntLiteral (ANF.Int64 -4L)))
    // __rawptr_to_dict<k, v> combines pointer + tag (ptr | tag)
    | name, [ptrAtom; tagAtom] when name = "__rawptr_to_dict" || name.StartsWith("__rawptr_to_dict_") ->
        Some (ANF.Prim (ANF.BitOr, ptrAtom, tagAtom))

    // List intrinsics - for Finger Tree implementation
    // __list_is_null<a> checks if list pointer is 0 (empty)
    | name, [listAtom] when name = "__list_is_null" || name.StartsWith("__list_is_null_") ->
        Some (ANF.Prim (ANF.Eq, listAtom, ANF.IntLiteral (ANF.Int64 0L)))
    // __list_get_tag<a> extracts low 3 bits (list & 7) for Finger Tree tags (0-4)
    | name, [listAtom] when name = "__list_get_tag" || name.StartsWith("__list_get_tag_") ->
        Some (ANF.Prim (ANF.BitAnd, listAtom, ANF.IntLiteral (ANF.Int64 7L)))
    // __list_to_rawptr<a> clears tag bits (list & -8) to get raw pointer
    | name, [listAtom] when name = "__list_to_rawptr" || name.StartsWith("__list_to_rawptr_") ->
        Some (ANF.Prim (ANF.BitAnd, listAtom, ANF.IntLiteral (ANF.Int64 -8L)))
    // __rawptr_to_list<a> combines pointer + tag (ptr | tag) to create tagged list
    | name, [ptrAtom; tagAtom] when name = "__rawptr_to_list" || name.StartsWith("__rawptr_to_list_") ->
        Some (ANF.Prim (ANF.BitOr, ptrAtom, tagAtom))
    // __list_empty<a> returns 0 (null pointer = empty finger tree)
    | name, [] when name = "__list_empty" || name.StartsWith("__list_empty_") ->
        Some (ANF.Atom (ANF.IntLiteral (ANF.Int64 0L)))

    // __hash_unknown/__key_eq_unknown crash at runtime when type args are unresolved
    | "__hash_unknown", [_] ->
        Some (ANF.RawGet (ANF.IntLiteral (ANF.Int64 0L), ANF.IntLiteral (ANF.Int64 0L), None))
    | "__key_eq_unknown", [_; _] ->
        Some (ANF.RawGet (ANF.IntLiteral (ANF.Int64 0L), ANF.IntLiteral (ANF.Int64 0L), None))

    | _ -> None

/// Try to convert a function call to a random intrinsic CExpr
/// Returns Some CExpr if it's a random intrinsic, None otherwise
let tryRandomIntrinsic (funcName: string) (args: ANF.Atom list) : ANF.CExpr option =
    match funcName, args with
    | "Stdlib.Random.int64", [] ->
        Some ANF.RandomInt64
    | _ -> None

/// Try to convert a function call to a date intrinsic CExpr
/// Returns Some CExpr if it's a date intrinsic, None otherwise
let tryDateIntrinsic (funcName: string) (args: ANF.Atom list) : ANF.CExpr option =
    match funcName, args with
    | "Stdlib.Date.now", [] ->
        Some ANF.DateNow
    | _ -> None

let isBuiltinUnwrapName (funcName: string) : bool =
    funcName = "Builtin.unwrap" || funcName = "Stdlib.Builtin.unwrap"

let private unwrapErrorPayloadToString (expr: AST.Expr) : string option =
    match expr with
    | AST.UnitLiteral -> Some "()"
    | AST.Int64Literal n -> Some $"{n}"
    | AST.Int8Literal n -> Some $"{n}"
    | AST.Int16Literal n -> Some $"{n}"
    | AST.Int32Literal n -> Some $"{n}"
    | AST.UInt8Literal n -> Some $"{n}"
    | AST.UInt16Literal n -> Some $"{n}"
    | AST.UInt32Literal n -> Some $"{n}"
    | AST.UInt64Literal n -> Some $"{n}"
    | AST.BoolLiteral true -> Some "true"
    | AST.BoolLiteral false -> Some "false"
    | AST.FloatLiteral f -> Some $"{f}"
    | AST.StringLiteral s -> Some s
    | AST.CharLiteral s -> Some s
    | _ -> None

/// Type registry - maps record type names to their field definitions
type TypeRegistry = Map<string, (string * AST.Type) list>

/// Variant lookup - maps variant names to (type name, type params, tag index, payload type)
type VariantLookup = Map<string, (string * string list * int * AST.Type option)>

/// Function registry - maps function names to their FULL function types (TFunction)
type FunctionRegistry = Map<string, AST.Type>

/// Alias registry - maps type alias names to their type params and target types
/// For simple record aliases: "Vec" -> ([], TRecord "Point")
type AliasRegistry = Map<string, string list * AST.Type>

/// Resolve a type name through the alias registry
/// If the name is an alias for a record type, returns the resolved record name
/// Otherwise returns the original name
let rec resolveRecordTypeName (aliasReg: AliasRegistry) (typeName: string) : string =
    match Map.tryFind typeName aliasReg with
    | Some ([], AST.TRecord (targetName, _)) -> resolveRecordTypeName aliasReg targetName
    | Some ([], AST.TSum (targetName, _)) -> resolveRecordTypeName aliasReg targetName
    | _ -> typeName

/// Expand a type registry to include alias entries
/// If "Vec" aliases to "Point" and "Point" has fields [x, y], then "Vec" also gets [x, y]
let expandTypeRegWithAliases (typeReg: TypeRegistry) (aliasReg: AliasRegistry) : TypeRegistry =
    aliasReg
    |> Map.fold (fun accReg aliasName (typeParams, targetType) ->
        match typeParams, targetType with
        | [], AST.TRecord (targetName, _) ->
            let resolvedName = resolveRecordTypeName aliasReg targetName
            match Map.tryFind resolvedName typeReg with
            | Some fields -> Map.add aliasName fields accReg
            | None -> accReg  // Target not found, skip
        | _ -> accReg  // Not a non-generic record alias, skip
    ) typeReg

/// Variable environment - maps variable names to their TempIds and types
/// The type information is used for type-directed field lookup in record access
type VarEnv = Map<string, ANF.TempId * AST.Type>

/// Extract just the type environment from VarEnv for use with inferType
let typeEnvFromVarEnv (varEnv: VarEnv) : Map<string, AST.Type> =
    varEnv |> Map.map (fun _ (_, t) -> t)

// ============================================================================
// Monomorphization Support for Generic Functions
// ============================================================================
//
// The Dark compiler uses monomorphization to handle generics - each generic
// function instantiation becomes a separate specialized function with a
// mangled name (e.g., identity<Int64> → identity_i64).
//
// Algorithm:
// 1. Collect all generic function definitions (functions with TypeParams)
// 2. Scan for TypeApp expressions (calls to generic functions with type args)
// 3. For each unique (funcName, [typeArgs]) pair:
//    - Substitute type parameters with concrete types in the function body
//    - Generate a specialized function with mangled name
// 4. Replace all TypeApp calls with regular Calls to mangled names
// 5. Iterate until fixed-point (new specializations may contain more TypeApps)
//
// Key design decisions:
// - No runtime type info: all types resolved at compile time
// - Name mangling encodes types: identity_i64, swap_str_bool
// - Iterative: handles nested generics like List<Option<T>>
//
// See docs/features/generics.md for detailed documentation.
// ============================================================================

/// Generic function registry - maps generic function names to their definitions
type GenericFuncDefs = Map<string, AST.FunctionDef>

/// Specialization key - a generic function instantiated with specific types
type SpecKey = string * AST.Type list  // (funcName, typeArgs)

/// Specialization registry - tracks which specializations are needed
/// Maps (funcName, typeArgs) -> specialized name
type SpecRegistry = Map<SpecKey, string>

/// Result of specializing generic functions from a spec set
type SpecializationResult = {
    SpecializedFuncs: AST.FunctionDef list
    SpecRegistry: SpecRegistry
    ExternalSpecs: Set<SpecKey>
}

/// Extract generic function definitions (functions with type parameters)
/// from a program. Used for on-demand monomorphization of stdlib generics.
let extractGenericFuncDefs (program: AST.Program) : GenericFuncDefs =
    let (AST.Program topLevels) = program
    topLevels
    |> List.choose (function
        | AST.FunctionDef f when not (List.isEmpty f.TypeParams) -> Some (f.Name, f)
        | _ -> None)
    |> Map.ofList

/// Convert a type to a string for name mangling
let rec typeToMangledName (t: AST.Type) : string =
    match t with
    | AST.TInt8 -> "i8"
    | AST.TInt16 -> "i16"
    | AST.TInt32 -> "i32"
    | AST.TInt64 -> "i64"
    | AST.TUInt8 -> "u8"
    | AST.TUInt16 -> "u16"
    | AST.TUInt32 -> "u32"
    | AST.TUInt64 -> "u64"
    | AST.TBool -> "bool"
    | AST.TFloat64 -> "f64"
    | AST.TString -> "str"
    | AST.TBytes -> "bytes"
    | AST.TChar -> "char"
    | AST.TUnit -> "unit"
    | AST.TFunction (paramTypes, retType) ->
        let paramStr = paramTypes |> List.map typeToMangledName |> String.concat "_"
        let retStr = typeToMangledName retType
        $"fn_{paramStr}_to_{retStr}"
    | AST.TTuple elemTypes ->
        let elemsStr = elemTypes |> List.map typeToMangledName |> String.concat "_"
        $"tup_{elemsStr}"
    | AST.TRecord (name, []) -> name
    | AST.TRecord (name, typeArgs) ->
        let argsStr = typeArgs |> List.map typeToMangledName |> String.concat "_"
        $"{name}_{argsStr}"
    | AST.TSum (name, []) -> name
    | AST.TSum (name, typeArgs) ->
        let argsStr = typeArgs |> List.map typeToMangledName |> String.concat "_"
        $"{name}_{argsStr}"
    | AST.TList elemType -> $"list_{typeToMangledName elemType}"
    | AST.TDict (keyType, valueType) -> $"dict_{typeToMangledName keyType}_{typeToMangledName valueType}"
    | AST.TVar name -> name  // Should not appear after monomorphization
    | AST.TRawPtr -> "rawptr"  // Internal raw pointer type

/// Check if a type contains any type variables
let rec containsTypeVar (t: AST.Type) : bool =
    match t with
    | AST.TVar _ -> true
    | AST.TFunction (paramTypes, retType) ->
        List.exists containsTypeVar paramTypes || containsTypeVar retType
    | AST.TTuple elemTypes -> List.exists containsTypeVar elemTypes
    | AST.TRecord (_, typeArgs) -> List.exists containsTypeVar typeArgs
    | AST.TSum (_, typeArgs) -> List.exists containsTypeVar typeArgs
    | AST.TList elemType -> containsTypeVar elemType
    | AST.TDict (keyType, valueType) -> containsTypeVar keyType || containsTypeVar valueType
    | _ -> false

/// Parse a mangled type name (from typeToMangledName) into an AST type.
/// Returns Error if the mangled form is ambiguous or unsupported.
let tryParseMangledType (variantLookup: VariantLookup) (mangled: string) : Result<AST.Type, string> =
    let tokens = mangled.Split('_') |> Array.toList
    let sumTypeNames =
        variantLookup
        |> Map.toList
        |> List.map (fun (_, (typeName, _, _, _)) -> typeName)
        |> Set.ofList

    let mkNamedType (name: string) (args: AST.Type list) : AST.Type =
        if Set.contains name sumTypeNames then AST.TSum (name, args) else AST.TRecord (name, args)

    let tryPrimitive (tok: string) : AST.Type option =
        match tok with
        | "i8" -> Some AST.TInt8
        | "i16" -> Some AST.TInt16
        | "i32" -> Some AST.TInt32
        | "i64" -> Some AST.TInt64
        | "u8" -> Some AST.TUInt8
        | "u16" -> Some AST.TUInt16
        | "u32" -> Some AST.TUInt32
        | "u64" -> Some AST.TUInt64
        | "bool" -> Some AST.TBool
        | "f64" -> Some AST.TFloat64
        | "str" -> Some AST.TString
        | "bytes" -> Some AST.TBytes
        | "char" -> Some AST.TChar
        | "unit" -> Some AST.TUnit
        | "rawptr" -> Some AST.TRawPtr
        | _ -> None

    let rec parseType (toks: string list) : (AST.Type * string list) list =
        match toks with
        | [] -> []
        | tok :: rest ->
            match tok with
            | "list" ->
                parseType rest |> List.map (fun (elemT, rem) -> (AST.TList elemT, rem))
            | "dict" ->
                parseType rest
                |> List.collect (fun (keyT, rem1) ->
                    parseType rem1 |> List.map (fun (valueT, rem2) -> (AST.TDict (keyT, valueT), rem2)))
            | "tup" ->
                parseTupleElems rest |> List.map (fun (elems, rem) -> (AST.TTuple elems, rem))
            | "fn" ->
                parseFunction rest
            | _ ->
                match tryPrimitive tok with
                | Some prim -> [ (prim, rest) ]
                | None ->
                    // Parse as named type with optional type arguments.
                    let baseType = (mkNamedType tok [], rest)
                    let withArgs =
                        parseTupleElems rest
                        |> List.map (fun (args, rem) -> (mkNamedType tok args, rem))
                    baseType :: withArgs

    and parseTupleElems (toks: string list) : (AST.Type list * string list) list =
        parseType toks
        |> List.collect (fun (firstT, rem1) ->
            let single = ([firstT], rem1)
            let more =
                parseTupleElems rem1
                |> List.map (fun (restTs, rem2) -> (firstT :: restTs, rem2))
            single :: more)

    and parseFunction (toks: string list) : (AST.Type * string list) list =
        let rec splitParams (acc: string list) (remaining: string list) =
            match remaining with
            | [] -> None
            | "to" :: rest -> Some (List.rev acc, rest)
            | tok :: rest -> splitParams (tok :: acc) rest
        match splitParams [] toks with
        | None -> []
        | Some (paramTokens, retTokens) ->
            let paramParses =
                parseTupleElems paramTokens
                |> List.filter (fun (_, rem) -> rem = [])
                |> List.map fst
            let retParses =
                parseType retTokens
                |> List.filter (fun (_, rem) -> rem = [])
                |> List.map fst
            paramParses
            |> List.collect (fun paramTypes ->
                retParses |> List.map (fun ret -> (AST.TFunction (paramTypes, ret), [])))

    match parseType tokens |> List.filter (fun (_, rem) -> rem = []) with
    | [ (typ, _) ] -> Ok typ
    | [] -> Error $"Could not parse mangled type: {mangled}"
    | _ -> Error $"Ambiguous mangled type: {mangled}"

/// Generate a specialized function name
let specName (funcName: string) (typeArgs: AST.Type list) : string =
    if List.isEmpty typeArgs then
        funcName
    elif funcName = "__hash" && List.exists containsTypeVar typeArgs then
        "__hash_unknown"
    elif funcName = "__key_eq" && List.exists containsTypeVar typeArgs then
        "__key_eq_unknown"
    else
        let typeStr = typeArgs |> List.map typeToMangledName |> String.concat "_"
        $"{funcName}_{typeStr}"

/// Type substitution - maps type variable names to concrete types
type Substitution = Map<string, AST.Type>

/// Apply a substitution to a type, replacing type variables with concrete types
let rec applySubstToType (subst: Substitution) (typ: AST.Type) : AST.Type =
    match typ with
    | AST.TVar name ->
        match Map.tryFind name subst with
        | Some concreteType -> concreteType
        | None -> typ  // Unbound type variable remains as-is
    | AST.TFunction (paramTypes, returnType) ->
        AST.TFunction (List.map (applySubstToType subst) paramTypes, applySubstToType subst returnType)
    | AST.TTuple elemTypes ->
        AST.TTuple (List.map (applySubstToType subst) elemTypes)
    | AST.TList elemType ->
        AST.TList (applySubstToType subst elemType)
    | AST.TDict (keyType, valueType) ->
        AST.TDict (applySubstToType subst keyType, applySubstToType subst valueType)
    | AST.TSum (name, typeArgs) ->
        AST.TSum (name, List.map (applySubstToType subst) typeArgs)
    | AST.TRecord (name, typeArgs) ->
        AST.TRecord (name, List.map (applySubstToType subst) typeArgs)
    | AST.TInt8 | AST.TInt16 | AST.TInt32 | AST.TInt64
    | AST.TUInt8 | AST.TUInt16 | AST.TUInt32 | AST.TUInt64
    | AST.TBool | AST.TFloat64 | AST.TString | AST.TBytes | AST.TChar | AST.TUnit | AST.TRawPtr ->
        typ  // Concrete types are unchanged

/// Apply a substitution to an expression, replacing type variables in type annotations
let rec applySubstToExpr (subst: Substitution) (expr: AST.Expr) : AST.Expr =
    match expr with
    | AST.UnitLiteral | AST.Int64Literal _ | AST.Int8Literal _ | AST.Int16Literal _ | AST.Int32Literal _
    | AST.UInt8Literal _ | AST.UInt16Literal _ | AST.UInt32Literal _ | AST.UInt64Literal _
    | AST.BoolLiteral _ | AST.StringLiteral _ | AST.CharLiteral _ | AST.FloatLiteral _ | AST.Var _ | AST.FuncRef _ | AST.Closure _ ->
        expr  // No types to substitute in literals, variables, function references, and closures
    | AST.BinOp (op, left, right) ->
        AST.BinOp (op, applySubstToExpr subst left, applySubstToExpr subst right)
    | AST.UnaryOp (op, inner) ->
        AST.UnaryOp (op, applySubstToExpr subst inner)
    | AST.Let (name, value, body) ->
        AST.Let (name, applySubstToExpr subst value, applySubstToExpr subst body)
    | AST.If (cond, thenBranch, elseBranch) ->
        AST.If (applySubstToExpr subst cond, applySubstToExpr subst thenBranch, applySubstToExpr subst elseBranch)
    | AST.Call (funcName, args) ->
        AST.Call (funcName, List.map (applySubstToExpr subst) args)
    | AST.TypeApp (funcName, typeArgs, args) ->
        // Substitute in type arguments and value arguments
        AST.TypeApp (funcName, List.map (applySubstToType subst) typeArgs, List.map (applySubstToExpr subst) args)
    | AST.TupleLiteral elements ->
        AST.TupleLiteral (List.map (applySubstToExpr subst) elements)
    | AST.TupleAccess (tuple, index) ->
        AST.TupleAccess (applySubstToExpr subst tuple, index)
    | AST.RecordLiteral (typeName, fields) ->
        AST.RecordLiteral (typeName, List.map (fun (n, e) -> (n, applySubstToExpr subst e)) fields)
    | AST.RecordUpdate (record, updates) ->
        AST.RecordUpdate (applySubstToExpr subst record, List.map (fun (n, e) -> (n, applySubstToExpr subst e)) updates)
    | AST.RecordAccess (record, fieldName) ->
        AST.RecordAccess (applySubstToExpr subst record, fieldName)
    | AST.Constructor (typeName, variantName, payload) ->
        AST.Constructor (typeName, variantName, Option.map (applySubstToExpr subst) payload)
    | AST.Match (scrutinee, cases) ->
        AST.Match (applySubstToExpr subst scrutinee,
                   cases |> List.map (fun mc -> { mc with Guard = mc.Guard |> Option.map (applySubstToExpr subst); Body = applySubstToExpr subst mc.Body }))
    | AST.ListLiteral elements ->
        AST.ListLiteral (List.map (applySubstToExpr subst) elements)
    | AST.ListCons (headElements, tail) ->
        AST.ListCons (List.map (applySubstToExpr subst) headElements, applySubstToExpr subst tail)
    | AST.Lambda (parameters, body) ->
        // Substitute types in parameter annotations and body
        let substParams = parameters |> List.map (fun (name, ty) -> (name, applySubstToType subst ty))
        AST.Lambda (substParams, applySubstToExpr subst body)
    | AST.Apply (func, args) ->
        AST.Apply (applySubstToExpr subst func, List.map (applySubstToExpr subst) args)
    | AST.InterpolatedString parts ->
        let substPart part =
            match part with
            | AST.StringText s -> AST.StringText s
            | AST.StringExpr e -> AST.StringExpr (applySubstToExpr subst e)
        AST.InterpolatedString (List.map substPart parts)

/// Resolve type aliases to their target types
let rec resolveAliasType (aliasReg: AliasRegistry) (typ: AST.Type) : AST.Type =
    match typ with
    | AST.TRecord (name, []) ->
        match Map.tryFind name aliasReg with
        | Some ([], targetType) -> resolveAliasType aliasReg targetType
        | Some (_, _) -> typ
        | None -> typ
    | AST.TSum (name, []) ->
        match Map.tryFind name aliasReg with
        | Some ([], targetType) -> resolveAliasType aliasReg targetType
        | Some (_, _) -> typ
        | None -> typ
    | AST.TSum (name, args) ->
        match Map.tryFind name aliasReg with
        | Some (typeParams, targetType) ->
            if List.length typeParams <> List.length args then
                typ
            else
                let subst = List.zip typeParams args |> Map.ofList
                let substituted = applySubstToType subst targetType
                resolveAliasType aliasReg substituted
        | None ->
            AST.TSum (name, List.map (resolveAliasType aliasReg) args)
    | AST.TRecord (name, args) ->
        AST.TRecord (name, List.map (resolveAliasType aliasReg) args)
    | AST.TTuple elems ->
        AST.TTuple (List.map (resolveAliasType aliasReg) elems)
    | AST.TList elem ->
        AST.TList (resolveAliasType aliasReg elem)
    | AST.TDict (k, v) ->
        AST.TDict (resolveAliasType aliasReg k, resolveAliasType aliasReg v)
    | AST.TFunction (args, ret) ->
        AST.TFunction (List.map (resolveAliasType aliasReg) args, resolveAliasType aliasReg ret)
    | _ -> typ

/// Resolve type aliases within function signatures
let resolveAliasesInFunction (aliasReg: AliasRegistry) (funcDef: AST.FunctionDef) : AST.FunctionDef =
    let resolvedParams = funcDef.Params |> List.map (fun (name, typ) -> (name, resolveAliasType aliasReg typ))
    let resolvedReturnType = resolveAliasType aliasReg funcDef.ReturnType
    { funcDef with Params = resolvedParams; ReturnType = resolvedReturnType }

/// Specialize a generic function definition with specific type arguments
let specializeFunction (funcDef: AST.FunctionDef) (typeArgs: AST.Type list) : AST.FunctionDef =
    // Build substitution from type parameters to type args
    let subst = List.zip funcDef.TypeParams typeArgs |> Map.ofList
    // Generate specialized name
    let specializedName = specName funcDef.Name typeArgs
    // Apply substitution to parameters, return type, and body
    let specializedParams = funcDef.Params |> List.map (fun (name, ty) -> (name, applySubstToType subst ty))
    let specializedReturnType = applySubstToType subst funcDef.ReturnType
    let specializedBody = applySubstToExpr subst funcDef.Body
    { Name = specializedName
      TypeParams = []  // Specialized function has no type parameters
      Params = specializedParams
      ReturnType = specializedReturnType
      Body = specializedBody }

/// Collect all TypeApp call sites from an expression
let rec collectTypeApps (expr: AST.Expr) : Set<SpecKey> =
    match expr with
    | AST.UnitLiteral | AST.Int64Literal _ | AST.Int8Literal _ | AST.Int16Literal _ | AST.Int32Literal _
    | AST.UInt8Literal _ | AST.UInt16Literal _ | AST.UInt32Literal _ | AST.UInt64Literal _
    | AST.BoolLiteral _ | AST.StringLiteral _ | AST.CharLiteral _ | AST.FloatLiteral _ | AST.Var _ | AST.FuncRef _ | AST.Closure _ ->
        Set.empty
    | AST.BinOp (_, left, right) ->
        Set.union (collectTypeApps left) (collectTypeApps right)
    | AST.UnaryOp (_, inner) ->
        collectTypeApps inner
    | AST.Let (_, value, body) ->
        Set.union (collectTypeApps value) (collectTypeApps body)
    | AST.If (cond, thenBranch, elseBranch) ->
        Set.union (collectTypeApps cond) (Set.union (collectTypeApps thenBranch) (collectTypeApps elseBranch))
    | AST.Call (_, args) ->
        args |> List.map collectTypeApps |> List.fold Set.union Set.empty
    | AST.TypeApp (funcName, typeArgs, args) ->
        // This is a generic call - collect this specialization plus any in args
        let argSpecs = args |> List.map collectTypeApps |> List.fold Set.union Set.empty
        let hasTypeVars = List.exists containsTypeVar typeArgs
        if hasTypeVars && (funcName = "__hash" || funcName = "__key_eq") then
            argSpecs
        elif (funcName = "Stdlib.Dict.fromList" || funcName = "Dict.fromList")
             && args = [AST.ListLiteral []]
             && not hasTypeVars then
            // Optimization: avoid building a Dict from an empty list when types are concrete.
            Set.add ("Stdlib.Dict.empty", typeArgs) argSpecs
        else
            Set.add (funcName, typeArgs) argSpecs
    | AST.TupleLiteral elements ->
        elements |> List.map collectTypeApps |> List.fold Set.union Set.empty
    | AST.TupleAccess (tuple, _) ->
        collectTypeApps tuple
    | AST.RecordLiteral (_, fields) ->
        fields |> List.map (snd >> collectTypeApps) |> List.fold Set.union Set.empty
    | AST.RecordUpdate (record, updates) ->
        let recordSpecs = collectTypeApps record
        let updatesSpecs = updates |> List.map (snd >> collectTypeApps) |> List.fold Set.union Set.empty
        Set.union recordSpecs updatesSpecs
    | AST.RecordAccess (record, _) ->
        collectTypeApps record
    | AST.Constructor (_, _, payload) ->
        payload |> Option.map collectTypeApps |> Option.defaultValue Set.empty
    | AST.Match (scrutinee, cases) ->
        let scrutineeSpecs = collectTypeApps scrutinee
        let caseSpecs = cases |> List.map (fun mc ->
            let guardSpecs = mc.Guard |> Option.map collectTypeApps |> Option.defaultValue Set.empty
            Set.union guardSpecs (collectTypeApps mc.Body)) |> List.fold Set.union Set.empty
        Set.union scrutineeSpecs caseSpecs
    | AST.ListLiteral elements ->
        elements |> List.map collectTypeApps |> List.fold Set.union Set.empty
    | AST.ListCons (headElements, tail) ->
        let headsSpecs = headElements |> List.map collectTypeApps |> List.fold Set.union Set.empty
        Set.union headsSpecs (collectTypeApps tail)
    | AST.Lambda (_, body) ->
        collectTypeApps body
    | AST.Apply (func, args) ->
        let funcSpecs = collectTypeApps func
        let argsSpecs = args |> List.map collectTypeApps |> List.fold Set.union Set.empty
        Set.union funcSpecs argsSpecs
    | AST.InterpolatedString parts ->
        parts |> List.choose (fun part ->
            match part with
            | AST.StringText _ -> None
            | AST.StringExpr e -> Some (collectTypeApps e))
        |> List.fold Set.union Set.empty

/// Collect TypeApps from a function definition
let collectTypeAppsFromFunc (funcDef: AST.FunctionDef) : Set<SpecKey> =
    collectTypeApps funcDef.Body

/// Specialize only the requested generic specs, returning new functions and a registry
let specializeFromSpecs (genericFuncDefs: GenericFuncDefs) (initialSpecs: Set<SpecKey>) : SpecializationResult =
    let rec iterate
        (pendingSpecs: Set<SpecKey>)
        (processedSpecs: Set<SpecKey>)
        (accFuncs: AST.FunctionDef list)
        (specRegistry: SpecRegistry)
        (externalSpecs: Set<SpecKey>)
        : SpecializationResult =
        let newSpecs = Set.difference pendingSpecs processedSpecs
        if Set.isEmpty newSpecs then
            { SpecializedFuncs = accFuncs
              SpecRegistry = specRegistry
              ExternalSpecs = externalSpecs }
        else
            let (newFuncs, newPendingSpecs, newRegistry, newExternal) =
                newSpecs
                |> Set.toList
                |> List.fold
                    (fun (funcs, pending, registry, external) (funcName, typeArgs) ->
                        match Map.tryFind funcName genericFuncDefs with
                        | Some funcDef ->
                            let specialized = specializeFunction funcDef typeArgs
                            let registry' = Map.add (funcName, typeArgs) specialized.Name registry
                            let bodySpecs = collectTypeAppsFromFunc specialized
                            (specialized :: funcs, Set.union pending bodySpecs, registry', external)
                        | None ->
                            (funcs, pending, registry, Set.add (funcName, typeArgs) external))
                    ([], Set.empty, specRegistry, externalSpecs)

            iterate
                newPendingSpecs
                (Set.union processedSpecs newSpecs)
                (newFuncs @ accFuncs)
                newRegistry
                newExternal

    iterate initialSpecs Set.empty [] Map.empty Set.empty

/// Replace TypeApp with Call using specialized name in an expression
let rec replaceTypeApps (expr: AST.Expr) : AST.Expr =
    match expr with
    | AST.UnitLiteral | AST.Int64Literal _ | AST.Int8Literal _ | AST.Int16Literal _ | AST.Int32Literal _
    | AST.UInt8Literal _ | AST.UInt16Literal _ | AST.UInt32Literal _ | AST.UInt64Literal _
    | AST.BoolLiteral _ | AST.StringLiteral _ | AST.CharLiteral _ | AST.FloatLiteral _ | AST.Var _ | AST.FuncRef _ | AST.Closure _ ->
        expr
    | AST.BinOp (op, left, right) ->
        AST.BinOp (op, replaceTypeApps left, replaceTypeApps right)
    | AST.UnaryOp (op, inner) ->
        AST.UnaryOp (op, replaceTypeApps inner)
    | AST.Let (name, value, body) ->
        AST.Let (name, replaceTypeApps value, replaceTypeApps body)
    | AST.If (cond, thenBranch, elseBranch) ->
        AST.If (replaceTypeApps cond, replaceTypeApps thenBranch, replaceTypeApps elseBranch)
    | AST.Call (funcName, args) ->
        AST.Call (funcName, List.map replaceTypeApps args)
    | AST.TypeApp (funcName, typeArgs, args) ->
        // Replace with a regular Call to the specialized name
        let hasTypeVars = List.exists containsTypeVar typeArgs
        if (funcName = "Stdlib.Dict.fromList" || funcName = "Dict.fromList")
           && args = [AST.ListLiteral []]
           && not hasTypeVars then
            // Optimization: avoid building a Dict from an empty list when types are concrete.
            let specializedName = specName "Stdlib.Dict.empty" typeArgs
            AST.Call (specializedName, [])
        else
            let specializedName = specName funcName typeArgs
            AST.Call (specializedName, List.map replaceTypeApps args)
    | AST.TupleLiteral elements ->
        AST.TupleLiteral (List.map replaceTypeApps elements)
    | AST.TupleAccess (tuple, index) ->
        AST.TupleAccess (replaceTypeApps tuple, index)
    | AST.RecordLiteral (typeName, fields) ->
        AST.RecordLiteral (typeName, List.map (fun (n, e) -> (n, replaceTypeApps e)) fields)
    | AST.RecordUpdate (record, updates) ->
        AST.RecordUpdate (replaceTypeApps record, List.map (fun (n, e) -> (n, replaceTypeApps e)) updates)
    | AST.RecordAccess (record, fieldName) ->
        AST.RecordAccess (replaceTypeApps record, fieldName)
    | AST.Constructor (typeName, variantName, payload) ->
        AST.Constructor (typeName, variantName, Option.map replaceTypeApps payload)
    | AST.Match (scrutinee, cases) ->
        AST.Match (replaceTypeApps scrutinee,
                   cases |> List.map (fun mc -> { mc with Guard = mc.Guard |> Option.map replaceTypeApps; Body = replaceTypeApps mc.Body }))
    | AST.ListLiteral elements ->
        AST.ListLiteral (List.map replaceTypeApps elements)
    | AST.ListCons (headElements, tail) ->
        AST.ListCons (List.map replaceTypeApps headElements, replaceTypeApps tail)
    | AST.Lambda (parameters, body) ->
        AST.Lambda (parameters, replaceTypeApps body)
    | AST.Apply (func, args) ->
        AST.Apply (replaceTypeApps func, List.map replaceTypeApps args)
    | AST.InterpolatedString parts ->
        let replacePart part =
            match part with
            | AST.StringText s -> AST.StringText s
            | AST.StringExpr e -> AST.StringExpr (replaceTypeApps e)
        AST.InterpolatedString (List.map replacePart parts)

let private isIntrinsicTypeAppName (funcName: string) : bool =
    match funcName with
    | "__raw_get"
    | "__raw_set"
    | "__empty_dict"
    | "__dict_is_null"
    | "__dict_get_tag"
    | "__dict_to_rawptr"
    | "__rawptr_to_dict"
    | "__list_empty"
    | "__list_is_null"
    | "__list_get_tag"
    | "__list_to_rawptr"
    | "__rawptr_to_list" -> true
    | _ -> false

let private missingSpecMessage (funcName: string) (typeArgs: AST.Type list) : string =
    let typeArgText =
        typeArgs
        |> List.map typeToMangledName
        |> String.concat ", "
    $"Missing specialization for {funcName}<{typeArgText}>"

/// Replace TypeApp with Call using a precomputed specialization registry
let replaceTypeAppsWithRegistry (specRegistry: SpecRegistry) (expr: AST.Expr) : Result<AST.Expr, string> =
    let rec mapResult (f: 'a -> Result<'b, string>) (items: 'a list) : Result<'b list, string> =
        match items with
        | [] -> Ok []
        | x :: xs ->
            f x
            |> Result.bind (fun x' ->
                mapResult f xs
                |> Result.map (fun xs' -> x' :: xs'))

    let rec replace (expr': AST.Expr) : Result<AST.Expr, string> =
        match expr' with
        | AST.UnitLiteral
        | AST.Int64Literal _
        | AST.Int8Literal _
        | AST.Int16Literal _
        | AST.Int32Literal _
        | AST.UInt8Literal _
        | AST.UInt16Literal _
        | AST.UInt32Literal _
        | AST.UInt64Literal _
        | AST.BoolLiteral _
        | AST.StringLiteral _
        | AST.CharLiteral _
        | AST.FloatLiteral _
        | AST.Var _
        | AST.FuncRef _
        | AST.Closure _ -> Ok expr'
        | AST.BinOp (op, left, right) ->
            replace left
            |> Result.bind (fun left' ->
                replace right
                |> Result.map (fun right' -> AST.BinOp (op, left', right')))
        | AST.UnaryOp (op, inner) ->
            replace inner |> Result.map (fun inner' -> AST.UnaryOp (op, inner'))
        | AST.Let (name, value, body) ->
            replace value
            |> Result.bind (fun value' ->
                replace body |> Result.map (fun body' -> AST.Let (name, value', body')))
        | AST.If (cond, thenBranch, elseBranch) ->
            replace cond
            |> Result.bind (fun cond' ->
                replace thenBranch
                |> Result.bind (fun thenBranch' ->
                    replace elseBranch
                    |> Result.map (fun elseBranch' -> AST.If (cond', thenBranch', elseBranch'))))
        | AST.Call (funcName, args) ->
            mapResult replace args
            |> Result.map (fun args' -> AST.Call (funcName, args'))
        | AST.TypeApp (funcName, typeArgs, args) ->
            let hasTypeVars = List.exists containsTypeVar typeArgs
            let emptyDictSpec = (funcName = "Stdlib.Dict.fromList" || funcName = "Dict.fromList")
                                && args = [AST.ListLiteral []]
                                && not hasTypeVars
            let resolvedNameResult =
                if funcName = "__hash" || funcName = "__key_eq" then
                    Ok (specName funcName typeArgs)
                elif isIntrinsicTypeAppName funcName then
                    Ok (specName funcName typeArgs)
                elif emptyDictSpec then
                    let key = ("Stdlib.Dict.empty", typeArgs)
                    match Map.tryFind key specRegistry with
                    | Some name -> Ok name
                    | None -> Error (missingSpecMessage "Stdlib.Dict.empty" typeArgs)
                else
                    match Map.tryFind (funcName, typeArgs) specRegistry with
                    | Some name -> Ok name
                    | None -> Error (missingSpecMessage funcName typeArgs)

            resolvedNameResult
            |> Result.bind (fun resolvedName ->
                if emptyDictSpec then
                    Ok (AST.Call (resolvedName, []))
                else
                    mapResult replace args
                    |> Result.map (fun args' -> AST.Call (resolvedName, args')))
        | AST.TupleLiteral elements ->
            mapResult replace elements
            |> Result.map AST.TupleLiteral
        | AST.TupleAccess (tuple, index) ->
            replace tuple |> Result.map (fun tuple' -> AST.TupleAccess (tuple', index))
        | AST.RecordLiteral (typeName, fields) ->
            fields
            |> mapResult (fun (name, value) ->
                replace value |> Result.map (fun value' -> (name, value')))
            |> Result.map (fun fields' -> AST.RecordLiteral (typeName, fields'))
        | AST.RecordUpdate (record, updates) ->
            replace record
            |> Result.bind (fun record' ->
                updates
                |> mapResult (fun (name, value) ->
                    replace value |> Result.map (fun value' -> (name, value')))
                |> Result.map (fun updates' -> AST.RecordUpdate (record', updates')))
        | AST.RecordAccess (record, fieldName) ->
            replace record |> Result.map (fun record' -> AST.RecordAccess (record', fieldName))
        | AST.Constructor (typeName, variantName, payload) ->
            match payload with
            | None -> Ok (AST.Constructor (typeName, variantName, None))
            | Some payloadExpr ->
                replace payloadExpr
                |> Result.map (fun payload' -> AST.Constructor (typeName, variantName, Some payload'))
        | AST.Match (scrutinee, cases) ->
            replace scrutinee
            |> Result.bind (fun scrutinee' ->
                cases
                |> mapResult (fun mc ->
                    let guardResult =
                        match mc.Guard with
                        | None -> Ok None
                        | Some guardExpr -> replace guardExpr |> Result.map Some
                    guardResult
                    |> Result.bind (fun guard' ->
                        replace mc.Body
                        |> Result.map (fun body' -> { mc with Guard = guard'; Body = body' })))
                |> Result.map (fun cases' -> AST.Match (scrutinee', cases')))
        | AST.ListLiteral elements ->
            mapResult replace elements |> Result.map AST.ListLiteral
        | AST.ListCons (headElements, tail) ->
            mapResult replace headElements
            |> Result.bind (fun heads' ->
                replace tail |> Result.map (fun tail' -> AST.ListCons (heads', tail')))
        | AST.Lambda (parameters, body) ->
            replace body |> Result.map (fun body' -> AST.Lambda (parameters, body'))
        | AST.Apply (func, args) ->
            replace func
            |> Result.bind (fun func' ->
                mapResult replace args |> Result.map (fun args' -> AST.Apply (func', args')))
        | AST.InterpolatedString parts ->
            parts
            |> mapResult (function
                | AST.StringText s -> Ok (AST.StringText s)
                | AST.StringExpr e -> replace e |> Result.map AST.StringExpr)
            |> Result.map AST.InterpolatedString

    replace expr

/// Replace TypeApp with Call in a function definition
let replaceTypeAppsInFunc (funcDef: AST.FunctionDef) : AST.FunctionDef =
    { funcDef with Body = replaceTypeApps funcDef.Body }

/// Replace TypeApp with Call in a function definition using a registry
let replaceTypeAppsInFuncWithRegistry (specRegistry: SpecRegistry) (funcDef: AST.FunctionDef) : Result<AST.FunctionDef, string> =
    replaceTypeAppsWithRegistry specRegistry funcDef.Body
    |> Result.map (fun body' -> { funcDef with Body = body' })

/// Replace TypeApp with Call across a program using a registry (drops generic defs)
let replaceTypeAppsInProgramWithRegistry (specRegistry: SpecRegistry) (program: AST.Program) : Result<AST.Program, string> =
    let (AST.Program topLevels) = program
    let rec loop (remaining: AST.TopLevel list) (acc: AST.TopLevel list) : Result<AST.Program, string> =
        match remaining with
        | [] -> Ok (AST.Program (List.rev acc))
        | tl :: rest ->
            match tl with
            | AST.FunctionDef f when not (List.isEmpty f.TypeParams) ->
                loop rest acc
            | AST.FunctionDef f ->
                replaceTypeAppsInFuncWithRegistry specRegistry f
                |> Result.bind (fun f' -> loop rest (AST.FunctionDef f' :: acc))
            | AST.Expression e ->
                replaceTypeAppsWithRegistry specRegistry e
                |> Result.bind (fun e' -> loop rest (AST.Expression e' :: acc))
            | AST.TypeDef td ->
                loop rest (AST.TypeDef td :: acc)

    loop topLevels []

/// Check if a program needs lambda lowering (lambda inlining + lifting)
/// based on lambdas, closures, or function values.
let programNeedsLambdaLowering (knownFuncNames: Set<string>) (program: AST.Program) : bool =
    let rec exprNeedsLambdaLowering (bound: Set<string>) (expr: AST.Expr) : bool =
        match expr with
        | AST.Lambda _ | AST.Apply _ | AST.FuncRef _ | AST.Closure _ ->
            true
        | AST.Var name ->
            Set.contains name knownFuncNames && not (Set.contains name bound)
        | AST.Let (name, value, body) ->
            exprNeedsLambdaLowering bound value
            || exprNeedsLambdaLowering (Set.add name bound) body
        | AST.If (cond, thenBranch, elseBranch) ->
            exprNeedsLambdaLowering bound cond
            || exprNeedsLambdaLowering bound thenBranch
            || exprNeedsLambdaLowering bound elseBranch
        | AST.BinOp (_, left, right) ->
            exprNeedsLambdaLowering bound left
            || exprNeedsLambdaLowering bound right
        | AST.UnaryOp (_, inner) ->
            exprNeedsLambdaLowering bound inner
        | AST.Call (_, args)
        | AST.TypeApp (_, _, args) ->
            args |> List.exists (exprNeedsLambdaLowering bound)
        | AST.TupleLiteral elems
        | AST.ListLiteral elems ->
            elems |> List.exists (exprNeedsLambdaLowering bound)
        | AST.ListCons (headElements, tail) ->
            (headElements |> List.exists (exprNeedsLambdaLowering bound))
            || exprNeedsLambdaLowering bound tail
        | AST.TupleAccess (tuple, _) ->
            exprNeedsLambdaLowering bound tuple
        | AST.RecordLiteral (_, fields) ->
            fields |> List.exists (fun (_, e) -> exprNeedsLambdaLowering bound e)
        | AST.RecordUpdate (record, updates) ->
            exprNeedsLambdaLowering bound record
            || (updates |> List.exists (fun (_, e) -> exprNeedsLambdaLowering bound e))
        | AST.RecordAccess (record, _) ->
            exprNeedsLambdaLowering bound record
        | AST.Constructor (_, _, payload) ->
            payload |> Option.exists (exprNeedsLambdaLowering bound)
        | AST.Match (scrutinee, cases) ->
            exprNeedsLambdaLowering bound scrutinee
            || (cases |> List.exists (fun (mc: AST.MatchCase) ->
                (mc.Guard |> Option.map (exprNeedsLambdaLowering bound) |> Option.defaultValue false)
                || exprNeedsLambdaLowering bound mc.Body))
        | AST.InterpolatedString parts ->
            parts |> List.exists (fun part ->
                match part with
                | AST.StringText _ -> false
                | AST.StringExpr e -> exprNeedsLambdaLowering bound e)
        | _ ->
            false

    let (AST.Program topLevels) = program
    let rec loop (remaining: AST.TopLevel list) : bool =
        match remaining with
        | [] -> false
        | tl :: rest ->
            match tl with
            | AST.FunctionDef f ->
                let paramNames = f.Params |> List.map fst |> Set.ofList
                if exprNeedsLambdaLowering paramNames f.Body then true else loop rest
            | AST.Expression e ->
                if exprNeedsLambdaLowering Set.empty e then true else loop rest
            | AST.TypeDef _ ->
                loop rest

    loop topLevels

// =============================================================================
// Lambda Inlining
// =============================================================================
// For first-class function support, we inline lambdas at their call sites.
// This transforms:
//   let f = (x: int) => x + 1 in f(5)
// Into:
//   let f = (x: int) => x + 1 in ((x: int) => x + 1)(5)
// Which is then handled by immediate application desugaring.

/// Environment mapping variable names to their lambda definitions
type LambdaEnv = Map<string, AST.Expr>

/// Check if a variable occurs in an expression (for dead code elimination)
let rec varOccursInExpr (name: string) (expr: AST.Expr) : bool =
    match expr with
    | AST.UnitLiteral | AST.Int64Literal _ | AST.Int8Literal _ | AST.Int16Literal _ | AST.Int32Literal _
    | AST.UInt8Literal _ | AST.UInt16Literal _ | AST.UInt32Literal _ | AST.UInt64Literal _
    | AST.BoolLiteral _ | AST.StringLiteral _ | AST.CharLiteral _ | AST.FloatLiteral _ -> false
    | AST.Var n -> n = name
    | AST.BinOp (_, left, right) -> varOccursInExpr name left || varOccursInExpr name right
    | AST.UnaryOp (_, inner) -> varOccursInExpr name inner
    | AST.Let (n, value, body) ->
        varOccursInExpr name value || (n <> name && varOccursInExpr name body)
    | AST.If (cond, thenBranch, elseBranch) ->
        varOccursInExpr name cond || varOccursInExpr name thenBranch || varOccursInExpr name elseBranch
    | AST.Call (funcName, args) ->
        // funcName could be a lambda variable reference (parser can't distinguish)
        funcName = name || List.exists (varOccursInExpr name) args
    | AST.TypeApp (_, _, args) -> List.exists (varOccursInExpr name) args
    | AST.TupleLiteral elements -> List.exists (varOccursInExpr name) elements
    | AST.TupleAccess (tuple, _) -> varOccursInExpr name tuple
    | AST.RecordLiteral (_, fields) -> List.exists (fun (_, e) -> varOccursInExpr name e) fields
    | AST.RecordUpdate (record, updates) ->
        varOccursInExpr name record || List.exists (fun (_, e) -> varOccursInExpr name e) updates
    | AST.RecordAccess (record, _) -> varOccursInExpr name record
    | AST.Constructor (_, _, payload) -> Option.exists (varOccursInExpr name) payload
    | AST.Match (scrutinee, cases) ->
        varOccursInExpr name scrutinee ||
        List.exists (fun (mc: AST.MatchCase) ->
            (mc.Guard |> Option.map (varOccursInExpr name) |> Option.defaultValue false) ||
            varOccursInExpr name mc.Body) cases
    | AST.ListLiteral elements -> List.exists (varOccursInExpr name) elements
    | AST.ListCons (headElements, tail) ->
        List.exists (varOccursInExpr name) headElements || varOccursInExpr name tail
    | AST.Lambda (parameters, body) ->
        // If name is shadowed by a parameter, it doesn't occur
        let paramNames = parameters |> List.map fst |> Set.ofList
        if Set.contains name paramNames then false
        else varOccursInExpr name body
    | AST.Apply (func, args) ->
        varOccursInExpr name func || List.exists (varOccursInExpr name) args
    | AST.FuncRef _ ->
        false  // Function references don't contain variable references
    | AST.Closure (_, captures) ->
        // Check if name occurs in captured expressions
        List.exists (varOccursInExpr name) captures
    | AST.InterpolatedString parts ->
        parts |> List.exists (fun part ->
            match part with
            | AST.StringText _ -> false
            | AST.StringExpr e -> varOccursInExpr name e)

/// Inline lambdas at Apply sites
/// lambdaEnv: maps variable names to their lambda expressions
let rec inlineLambdas (expr: AST.Expr) (lambdaEnv: LambdaEnv) : AST.Expr =
    match expr with
    | AST.UnitLiteral | AST.Int64Literal _ | AST.Int8Literal _ | AST.Int16Literal _ | AST.Int32Literal _
    | AST.UInt8Literal _ | AST.UInt16Literal _ | AST.UInt32Literal _ | AST.UInt64Literal _
    | AST.BoolLiteral _ | AST.StringLiteral _ | AST.CharLiteral _ | AST.FloatLiteral _ ->
        expr
    | AST.Var _ -> expr  // Variable references stay as-is (not at call position)
    | AST.BinOp (op, left, right) ->
        AST.BinOp (op, inlineLambdas left lambdaEnv, inlineLambdas right lambdaEnv)
    | AST.UnaryOp (op, inner) ->
        AST.UnaryOp (op, inlineLambdas inner lambdaEnv)
    | AST.Let (name, value, body) ->
        let value' = inlineLambdas value lambdaEnv
        // If the value is a lambda, add it to the environment for the body
        let lambdaEnv' =
            match value' with
            | AST.Lambda _ -> Map.add name value' lambdaEnv
            | _ -> lambdaEnv
        let body' = inlineLambdas body lambdaEnv'
        // Dead lambda elimination: if the value was a lambda and the variable
        // is no longer used in the body (all uses were inlined), drop the binding
        match value' with
        | AST.Lambda _ when not (varOccursInExpr name body') -> body'
        | _ -> AST.Let (name, value', body')
    | AST.If (cond, thenBranch, elseBranch) ->
        AST.If (inlineLambdas cond lambdaEnv, inlineLambdas thenBranch lambdaEnv, inlineLambdas elseBranch lambdaEnv)
    | AST.Call (funcName, args) ->
        let args' = List.map (fun a -> inlineLambdas a lambdaEnv) args
        // Check if funcName is actually a lambda variable (parser can't distinguish)
        match Map.tryFind funcName lambdaEnv with
        | Some lambdaExpr -> AST.Apply (lambdaExpr, args')
        | None -> AST.Call (funcName, args')
    | AST.TypeApp (funcName, typeArgs, args) ->
        AST.TypeApp (funcName, typeArgs, List.map (fun a -> inlineLambdas a lambdaEnv) args)
    | AST.TupleLiteral elements ->
        AST.TupleLiteral (List.map (fun e -> inlineLambdas e lambdaEnv) elements)
    | AST.TupleAccess (tuple, index) ->
        AST.TupleAccess (inlineLambdas tuple lambdaEnv, index)
    | AST.RecordLiteral (typeName, fields) ->
        AST.RecordLiteral (typeName, List.map (fun (n, e) -> (n, inlineLambdas e lambdaEnv)) fields)
    | AST.RecordUpdate (record, updates) ->
        AST.RecordUpdate (inlineLambdas record lambdaEnv, List.map (fun (n, e) -> (n, inlineLambdas e lambdaEnv)) updates)
    | AST.RecordAccess (record, fieldName) ->
        AST.RecordAccess (inlineLambdas record lambdaEnv, fieldName)
    | AST.Constructor (typeName, variantName, payload) ->
        AST.Constructor (typeName, variantName, Option.map (fun e -> inlineLambdas e lambdaEnv) payload)
    | AST.Match (scrutinee, cases) ->
        AST.Match (inlineLambdas scrutinee lambdaEnv,
                   cases |> List.map (fun mc -> { mc with Guard = mc.Guard |> Option.map (fun g -> inlineLambdas g lambdaEnv); Body = inlineLambdas mc.Body lambdaEnv }))
    | AST.ListLiteral elements ->
        AST.ListLiteral (List.map (fun e -> inlineLambdas e lambdaEnv) elements)
    | AST.ListCons (headElements, tail) ->
        AST.ListCons (List.map (fun e -> inlineLambdas e lambdaEnv) headElements, inlineLambdas tail lambdaEnv)
    | AST.Lambda (parameters, body) ->
        // Lambdas can reference outer lambdas, so inline in body
        AST.Lambda (parameters, inlineLambdas body lambdaEnv)
    | AST.Apply (func, args) ->
        let args' = List.map (fun a -> inlineLambdas a lambdaEnv) args
        match func with
        | AST.Var name ->
            // Check if this variable is a known lambda
            match Map.tryFind name lambdaEnv with
            | Some lambdaExpr ->
                // Substitute the lambda at the call site
                AST.Apply (lambdaExpr, args')
            | None ->
                // Unknown function variable - keep as-is (will error later if not valid)
                AST.Apply (AST.Var name, args')
        | _ ->
            // Non-variable function (could be lambda or other expr)
            AST.Apply (inlineLambdas func lambdaEnv, args')
    | AST.FuncRef _ ->
        // Function references don't need lambda inlining
        expr
    | AST.Closure (funcName, captures) ->
        // Inline lambdas in captured expressions
        AST.Closure (funcName, List.map (fun c -> inlineLambdas c lambdaEnv) captures)
    | AST.InterpolatedString parts ->
        let inlinePart part =
            match part with
            | AST.StringText s -> AST.StringText s
            | AST.StringExpr e -> AST.StringExpr (inlineLambdas e lambdaEnv)
        AST.InterpolatedString (List.map inlinePart parts)

/// Inline lambdas in a function definition
let inlineLambdasInFunc (funcDef: AST.FunctionDef) : AST.FunctionDef =
    { funcDef with Body = inlineLambdas funcDef.Body Map.empty }

/// Inline lambdas in a program
let inlineLambdasInProgram (program: AST.Program) : AST.Program =
    let (AST.Program topLevels) = program
    let topLevels' =
        topLevels
        |> List.map (function
            | AST.FunctionDef f -> AST.FunctionDef (inlineLambdasInFunc f)
            | AST.Expression e -> AST.Expression (inlineLambdas e Map.empty)
            | AST.TypeDef t -> AST.TypeDef t)
    AST.Program topLevels'

// ============================================================================
// Lambda Lifting: Convert Lambdas to Top-Level Functions with Closures
// ============================================================================
//
// Lambda lifting transforms nested lambda expressions into top-level functions.
// The process handles both capturing and non-capturing lambdas uniformly.
//
// Algorithm:
// 1. Identify lambdas in argument positions (function calls, let bindings)
// 2. Collect free variables (captures) from each lambda body
// 3. Generate a lifted function with signature: (closure_tuple, original_params...) -> result
// 4. Replace the lambda with a ClosureAlloc expression containing the function and captures
//
// Closure representation at runtime:
//   [func_ptr, cap1, cap2, ...]  -- heap-allocated tuple
//
// The lifted function extracts captures from the closure tuple:
//   let __closure_N(__closure, x, y) =
//       let cap1 = __closure.1
//       let cap2 = __closure.2
//       in <original body with captures replaced>
//
// All function values use closures for uniform calling convention, even non-capturing
// lambdas and function references. This simplifies higher-order function support.
//
// See docs/features/closures.md for detailed documentation.
// ============================================================================

/// State for lambda lifting - tracks generated functions and counter
type LiftState = {
    Counter: int
    LiftedFunctions: AST.FunctionDef list
    TypeEnv: Map<string, AST.Type>  // Variable name -> Type (for tracking types of captured variables)
    FuncParams: Map<string, (string * AST.Type) list>  // Function name -> params (for inferring function value types)
    FuncReturnTypes: Map<string, AST.Type>  // Function name -> Return type (for inferring call result types)
    GenericFuncDefs: Map<string, string list * AST.Type>  // Function name -> (TypeParams, ReturnType) for TypeApp substitution
    TypeReg: TypeRegistry
    VariantLookup: VariantLookup
}

/// Collect free variables in an expression (variables not bound by let or lambda parameters)
let rec freeVars (expr: AST.Expr) (bound: Set<string>) : Set<string> =
    match expr with
    | AST.UnitLiteral | AST.Int64Literal _ | AST.Int8Literal _ | AST.Int16Literal _ | AST.Int32Literal _
    | AST.UInt8Literal _ | AST.UInt16Literal _ | AST.UInt32Literal _ | AST.UInt64Literal _
    | AST.BoolLiteral _ | AST.StringLiteral _ | AST.CharLiteral _ | AST.FloatLiteral _ -> Set.empty
    | AST.Var name -> if Set.contains name bound then Set.empty else Set.singleton name
    | AST.BinOp (_, left, right) -> Set.union (freeVars left bound) (freeVars right bound)
    | AST.UnaryOp (_, inner) -> freeVars inner bound
    | AST.Let (name, value, body) ->
        let valueVars = freeVars value bound
        let bodyVars = freeVars body (Set.add name bound)
        Set.union valueVars bodyVars
    | AST.If (cond, thenBr, elseBr) ->
        Set.union (freeVars cond bound) (Set.union (freeVars thenBr bound) (freeVars elseBr bound))
    | AST.Call (funcName, args) ->
        // Check if funcName is a local variable (not in bound) - if so, it's a free variable
        // Top-level function names will be filtered out later since they won't be in TypeEnv
        let funcFree = if Set.contains funcName bound then Set.empty else Set.singleton funcName
        let argsFree = args |> List.map (fun a -> freeVars a bound) |> List.fold Set.union Set.empty
        Set.union funcFree argsFree
    | AST.TypeApp (_, _, args) ->
        args |> List.map (fun a -> freeVars a bound) |> List.fold Set.union Set.empty
    | AST.TupleLiteral elems | AST.ListLiteral elems ->
        elems |> List.map (fun e -> freeVars e bound) |> List.fold Set.union Set.empty
    | AST.ListCons (headElements, tail) ->
        let headsFree = headElements |> List.map (fun e -> freeVars e bound) |> List.fold Set.union Set.empty
        Set.union headsFree (freeVars tail bound)
    | AST.TupleAccess (tuple, _) -> freeVars tuple bound
    | AST.RecordLiteral (_, fields) ->
        fields |> List.map (fun (_, e) -> freeVars e bound) |> List.fold Set.union Set.empty
    | AST.RecordUpdate (record, updates) ->
        let recordVars = freeVars record bound
        let updateVars = updates |> List.map (fun (_, e) -> freeVars e bound) |> List.fold Set.union Set.empty
        Set.union recordVars updateVars
    | AST.RecordAccess (record, _) -> freeVars record bound
    | AST.Constructor (_, _, payload) ->
        payload |> Option.map (fun e -> freeVars e bound) |> Option.defaultValue Set.empty
    | AST.Match (scrutinee, cases) ->
        let scrutineeVars = freeVars scrutinee bound
        let caseVars = cases |> List.map (fun mc ->
            let guardVars = mc.Guard |> Option.map (fun g -> freeVars g bound) |> Option.defaultValue Set.empty
            Set.union guardVars (freeVars mc.Body bound)) |> List.fold Set.union Set.empty
        Set.union scrutineeVars caseVars
    | AST.Lambda (parameters, body) ->
        let paramNames = parameters |> List.map fst |> Set.ofList
        freeVars body (Set.union bound paramNames)
    | AST.Apply (func, args) ->
        let funcVars = freeVars func bound
        let argVars = args |> List.map (fun a -> freeVars a bound) |> List.fold Set.union Set.empty
        Set.union funcVars argVars
    | AST.FuncRef _ -> Set.empty
    | AST.Closure (_, captures) ->
        // Closure captures may contain free variables
        captures |> List.map (fun c -> freeVars c bound) |> List.fold Set.union Set.empty
    | AST.InterpolatedString parts ->
        parts |> List.choose (fun part ->
            match part with
            | AST.StringText _ -> None
            | AST.StringExpr e -> Some (freeVars e bound))
        |> List.fold Set.union Set.empty

/// Simple type inference for lambda lifting - infers types of simple expressions
/// This allows let-bound variables to be captured in nested lambdas
let rec simpleInferType
    (expr: AST.Expr)
    (typeEnv: Map<string, AST.Type>)
    (funcParams: Map<string, (string * AST.Type) list>)
    (funcReturnTypes: Map<string, AST.Type>)
    (genericFuncDefs: Map<string, string list * AST.Type>)
    (typeReg: TypeRegistry)
    (variantLookup: VariantLookup)
    : AST.Type option =
    let isIntType (typ: AST.Type) : bool =
        match typ with
        | AST.TInt8 | AST.TInt16 | AST.TInt32 | AST.TInt64
        | AST.TUInt8 | AST.TUInt16 | AST.TUInt32 | AST.TUInt64 -> true
        | _ -> false

    let mergeBindings (bindings: Map<string, AST.Type>) (extra: Map<string, AST.Type>) : Map<string, AST.Type> =
        Map.fold (fun acc name typ -> Map.add name typ acc) bindings extra

    let rec extractPatternBindings (pattern: AST.Pattern) (scrutType: AST.Type) : Map<string, AST.Type> =
        match pattern with
        | AST.PVar name -> Map.ofList [(name, scrutType)]
        | AST.PWildcard -> Map.empty
        | AST.PInt64 _
        | AST.PInt8Literal _
        | AST.PInt16Literal _
        | AST.PInt32Literal _
        | AST.PUInt8Literal _
        | AST.PUInt16Literal _
        | AST.PUInt32Literal _
        | AST.PUInt64Literal _
        | AST.PUnit
        | AST.PBool _
        | AST.PString _
        | AST.PFloat _ -> Map.empty
        | AST.PTuple innerPats ->
            match scrutType with
            | AST.TTuple elemTypes when List.length elemTypes = List.length innerPats ->
                List.zip innerPats elemTypes
                |> List.fold (fun acc (pat, typ) -> mergeBindings acc (extractPatternBindings pat typ)) Map.empty
            | _ -> Map.empty
        | AST.PRecord (typeName, fieldPats) ->
            match Map.tryFind typeName typeReg with
            | Some fields ->
                fieldPats
                |> List.fold (fun acc (fieldName, pat) ->
                    match fields |> List.tryFind (fun (name, _) -> name = fieldName) with
                    | Some (_, fieldType) -> mergeBindings acc (extractPatternBindings pat fieldType)
                    | None -> acc) Map.empty
            | None -> Map.empty
        | AST.PConstructor (variantName, payloadPat) ->
            match Map.tryFind variantName variantLookup, payloadPat with
            | Some (typeName, typeParams, _, Some payloadType), Some pat ->
                let subst =
                    match scrutType with
                    | AST.TSum (scrutTypeName, typeArgs)
                        when scrutTypeName = typeName
                             && List.length typeParams = List.length typeArgs ->
                        List.zip typeParams typeArgs |> Map.ofList
                    | _ -> Map.empty
                extractPatternBindings pat (applySubstToType subst payloadType)
            | _ -> Map.empty
        | AST.PList innerPats ->
            match scrutType with
            | AST.TList elemType ->
                innerPats
                |> List.fold (fun acc pat -> mergeBindings acc (extractPatternBindings pat elemType)) Map.empty
            | _ -> Map.empty
        | AST.PListCons (headPats, tailPat) ->
            match scrutType with
            | AST.TList elemType ->
                let headBindings =
                    headPats
                    |> List.fold (fun acc pat -> mergeBindings acc (extractPatternBindings pat elemType)) Map.empty
                mergeBindings headBindings (extractPatternBindings tailPat scrutType)
            | _ -> Map.empty

    match expr with
    | AST.Int64Literal _ -> Some AST.TInt64
    | AST.Int8Literal _ -> Some AST.TInt8
    | AST.Int16Literal _ -> Some AST.TInt16
    | AST.Int32Literal _ -> Some AST.TInt32
    | AST.UInt8Literal _ -> Some AST.TUInt8
    | AST.UInt16Literal _ -> Some AST.TUInt16
    | AST.UInt32Literal _ -> Some AST.TUInt32
    | AST.UInt64Literal _ -> Some AST.TUInt64
    | AST.BoolLiteral _ -> Some AST.TBool
    | AST.StringLiteral _ -> Some AST.TString
    | AST.CharLiteral _ -> Some AST.TChar
    | AST.FloatLiteral _ -> Some AST.TFloat64
    | AST.UnitLiteral -> Some AST.TUnit
    | AST.Var name ->
        match Map.tryFind name typeEnv with
        | Some typ -> Some typ
        | None ->
            match Map.tryFind name funcParams, Map.tryFind name funcReturnTypes with
            | Some parameters, Some returnType ->
                Some (AST.TFunction (parameters |> List.map snd, returnType))
            | _ -> None
    | AST.Let (name, value, body) ->
        let valueType = simpleInferType value typeEnv funcParams funcReturnTypes genericFuncDefs typeReg variantLookup
        let typeEnv' =
            match valueType with
            | Some typ -> Map.add name typ typeEnv
            | None -> typeEnv
        simpleInferType body typeEnv' funcParams funcReturnTypes genericFuncDefs typeReg variantLookup
    | AST.TupleLiteral elements ->
        // Recursively infer types of tuple elements
        let elemTypes = elements |> List.map (fun e -> simpleInferType e typeEnv funcParams funcReturnTypes genericFuncDefs typeReg variantLookup)
        if List.forall Option.isSome elemTypes then
            Some (AST.TTuple (elemTypes |> List.map Option.get))
        else
            None
    | AST.TupleAccess (tupleExpr, index) ->
        match simpleInferType tupleExpr typeEnv funcParams funcReturnTypes genericFuncDefs typeReg variantLookup with
        | Some (AST.TTuple elemTypes) when index >= 0 && index < List.length elemTypes ->
            Some (List.item index elemTypes)
        | _ -> None
    | AST.RecordLiteral (typeName, _) ->
        // Record literal has the record's type
        Some (AST.TRecord (typeName, []))
    | AST.RecordAccess (recordExpr, fieldName) ->
        match simpleInferType recordExpr typeEnv funcParams funcReturnTypes genericFuncDefs typeReg variantLookup with
        | Some (AST.TRecord (typeName, _)) ->
            match Map.tryFind typeName typeReg with
            | Some fields -> fields |> List.tryFind (fun (name, _) -> name = fieldName) |> Option.map snd
            | None -> None
        | _ -> None
    | AST.Constructor (typeName, variantName, _) ->
        // Sum type constructor has the sum type
        match Map.tryFind variantName variantLookup with
        | Some (sumTypeName, typeParams, _, _) ->
            let typeArgs =
                if List.isEmpty typeParams then
                    []
                else
                    typeParams |> List.map AST.TVar
            Some (AST.TSum (sumTypeName, typeArgs))
        | None ->
            if typeName = "" then
                None
            else
                Some (AST.TSum (typeName, []))
    | AST.BinOp (op, left, right) ->
        let leftType = simpleInferType left typeEnv funcParams funcReturnTypes genericFuncDefs typeReg variantLookup
        let rightType = simpleInferType right typeEnv funcParams funcReturnTypes genericFuncDefs typeReg variantLookup
        match op with
        | AST.Add | AST.Sub | AST.Mul | AST.Div | AST.Mod ->
            match leftType, rightType with
            | Some lt, Some rt when lt = rt && (isIntType lt || lt = AST.TFloat64) -> Some lt
            | _ -> None
        | AST.Shl | AST.Shr | AST.BitAnd | AST.BitOr | AST.BitXor ->
            match leftType, rightType with
            | Some lt, Some rt when lt = rt && isIntType lt -> Some lt
            | _ -> None
        | AST.Eq | AST.Neq | AST.Lt | AST.Gt | AST.Lte | AST.Gte | AST.And | AST.Or -> Some AST.TBool
        | AST.StringConcat -> Some AST.TString
    | AST.Call (funcName, args) ->
        // Look up the function's return type, checking local bindings first
        match Map.tryFind funcName typeEnv with
        | Some (AST.TFunction (paramTypes, returnType)) ->
            let argCount = List.length args
            let paramCount = List.length paramTypes
            if argCount = paramCount then
                Some returnType
            elif argCount < paramCount then
                Some (AST.TFunction (paramTypes |> List.skip argCount, returnType))
            else
                None
        | _ ->
            Map.tryFind funcName funcReturnTypes
    | AST.TypeApp (funcName, typeArgs, _) ->
        // Look up the generic function's definition and apply type substitution
        match Map.tryFind funcName genericFuncDefs with
        | Some (typeParams, returnType) when List.length typeParams = List.length typeArgs ->
            // Build substitution from type params to type args
            let subst = List.zip typeParams typeArgs |> Map.ofList
            Some (applySubstToType subst returnType)
        | _ ->
            // Fall back to funcReturnTypes for non-generic or arity mismatch
            Map.tryFind funcName funcReturnTypes
    | AST.If (_, thenExpr, elseExpr) ->
        match simpleInferType thenExpr typeEnv funcParams funcReturnTypes genericFuncDefs typeReg variantLookup,
              simpleInferType elseExpr typeEnv funcParams funcReturnTypes genericFuncDefs typeReg variantLookup with
        | Some thenType, Some elseType when thenType = elseType -> Some thenType
        | _ -> None
    | AST.Match (scrutinee, cases) ->
        let scrutineeType = simpleInferType scrutinee typeEnv funcParams funcReturnTypes genericFuncDefs typeReg variantLookup
        let caseTypes =
            cases
            |> List.map (fun mc ->
                let patterns = mc.Patterns.Head :: mc.Patterns.Tail
                let caseEnv =
                    match scrutineeType with
                    | Some scrutType ->
                        patterns
                        |> List.map (fun pat -> extractPatternBindings pat scrutType)
                        |> List.fold mergeBindings typeEnv
                    | None -> typeEnv
                simpleInferType mc.Body caseEnv funcParams funcReturnTypes genericFuncDefs typeReg variantLookup)
        if List.forall Option.isSome caseTypes then
            let types = caseTypes |> List.choose id
            match types with
            | first :: rest when rest |> List.forall (fun t -> t = first) -> Some first
            | _ -> None
        else
            None
    | AST.Lambda (parameters, body) ->
        let paramTypes = parameters |> List.map snd
        let lambdaParamTypes = parameters |> Map.ofList
        let typeEnv' = Map.fold (fun acc k v -> Map.add k v acc) typeEnv lambdaParamTypes
        match simpleInferType body typeEnv' funcParams funcReturnTypes genericFuncDefs typeReg variantLookup with
        | Some returnType -> Some (AST.TFunction (paramTypes, returnType))
        | None -> None
    | AST.Apply (funcExpr, args) ->
        match simpleInferType funcExpr typeEnv funcParams funcReturnTypes genericFuncDefs typeReg variantLookup with
        | Some (AST.TFunction (paramTypes, returnType)) ->
            let argCount = List.length args
            let paramCount = List.length paramTypes
            if argCount = paramCount then
                Some returnType
            elif argCount < paramCount then
                Some (AST.TFunction (paramTypes |> List.skip argCount, returnType))
            else
                None
        | _ -> None
    | _ -> None  // Complex expressions require full type inference

let inferLambdaReturnType (body: AST.Expr) (state: LiftState) : Result<AST.Type, string> =
    match simpleInferType body state.TypeEnv state.FuncParams state.FuncReturnTypes state.GenericFuncDefs state.TypeReg state.VariantLookup with
    | Some returnType -> Ok returnType
    | None -> Error "Lambda lifting could not infer return type for lambda body"

/// Lift lambdas in an expression, returning (transformed expr, new state)
let rec liftLambdasInExpr (expr: AST.Expr) (state: LiftState) : Result<AST.Expr * LiftState, string> =
    match expr with
    | AST.UnitLiteral | AST.Int64Literal _ | AST.Int8Literal _ | AST.Int16Literal _ | AST.Int32Literal _
    | AST.UInt8Literal _ | AST.UInt16Literal _ | AST.UInt32Literal _ | AST.UInt64Literal _
    | AST.BoolLiteral _ | AST.StringLiteral _ | AST.CharLiteral _ | AST.FloatLiteral _ | AST.Var _ | AST.FuncRef _ | AST.Closure _ ->
        Ok (expr, state)
    | AST.BinOp (op, left, right) ->
        liftLambdasInExpr left state
        |> Result.bind (fun (left', state1) ->
            liftLambdasInExpr right state1
            |> Result.map (fun (right', state2) -> (AST.BinOp (op, left', right'), state2)))
    | AST.UnaryOp (op, inner) ->
        liftLambdasInExpr inner state
        |> Result.map (fun (inner', state') -> (AST.UnaryOp (op, inner'), state'))
    | AST.Let (name, value, body) ->
        liftLambdasInExpr value state
        |> Result.bind (fun (value', state1) ->
            // Try to infer the type of the value for capturing in nested lambdas
            let valueType = simpleInferType value state1.TypeEnv state1.FuncParams state1.FuncReturnTypes state1.GenericFuncDefs state1.TypeReg state1.VariantLookup
            let state1' = match valueType with
                          | Some t -> { state1 with TypeEnv = Map.add name t state1.TypeEnv }
                          | None -> state1
            liftLambdasInExpr body state1'
            |> Result.map (fun (body', state2) ->
                // Restore TypeEnv (remove the let binding)
                let state2' = { state2 with TypeEnv = Map.remove name state2.TypeEnv }
                (AST.Let (name, value', body'), state2')))
    | AST.If (cond, thenBr, elseBr) ->
        liftLambdasInExpr cond state
        |> Result.bind (fun (cond', state1) ->
            liftLambdasInExpr thenBr state1
            |> Result.bind (fun (thenBr', state2) ->
                liftLambdasInExpr elseBr state2
                |> Result.map (fun (elseBr', state3) -> (AST.If (cond', thenBr', elseBr'), state3))))
    | AST.Call (funcName, args) ->
        // Process args, lifting any lambdas
        liftLambdasInArgs args state
        |> Result.map (fun (args', state') -> (AST.Call (funcName, args'), state'))
    | AST.TypeApp (funcName, typeArgs, args) ->
        liftLambdasInArgs args state
        |> Result.map (fun (args', state') -> (AST.TypeApp (funcName, typeArgs, args'), state'))
    | AST.TupleLiteral elems ->
        liftLambdasInList elems state
        |> Result.map (fun (elems', state') -> (AST.TupleLiteral elems', state'))
    | AST.ListLiteral elems ->
        liftLambdasInList elems state
        |> Result.map (fun (elems', state') -> (AST.ListLiteral elems', state'))
    | AST.ListCons (headElements, tail) ->
        liftLambdasInList headElements state
        |> Result.bind (fun (heads', state') ->
            liftLambdasInExpr tail state'
            |> Result.map (fun (tail', state'') -> (AST.ListCons (heads', tail'), state'')))
    | AST.TupleAccess (tuple, index) ->
        liftLambdasInExpr tuple state
        |> Result.map (fun (tuple', state') -> (AST.TupleAccess (tuple', index), state'))
    | AST.RecordLiteral (typeName, fields) ->
        liftLambdasInFields fields state
        |> Result.map (fun (fields', state') -> (AST.RecordLiteral (typeName, fields'), state'))
    | AST.RecordUpdate (record, updates) ->
        liftLambdasInExpr record state
        |> Result.bind (fun (record', state1) ->
            liftLambdasInFields updates state1
            |> Result.map (fun (updates', state2) -> (AST.RecordUpdate (record', updates'), state2)))
    | AST.RecordAccess (record, fieldName) ->
        liftLambdasInExpr record state
        |> Result.map (fun (record', state') -> (AST.RecordAccess (record', fieldName), state'))
    | AST.Constructor (typeName, variantName, payload) ->
        match payload with
        | None -> Ok (expr, state)
        | Some p ->
            liftLambdasInExpr p state
            |> Result.map (fun (p', state') -> (AST.Constructor (typeName, variantName, Some p'), state'))
    | AST.Match (scrutinee, cases) ->
        liftLambdasInExpr scrutinee state
        |> Result.bind (fun (scrutinee', state1) ->
            liftLambdasInCases cases state1
            |> Result.map (fun (cases', state2) -> (AST.Match (scrutinee', cases'), state2)))
    | AST.Lambda (parameters, body) ->
        // Lambda in expression position - lift it to a closure
        // Add lambda parameters to type environment before processing body
        let lambdaParamTypes = parameters |> Map.ofList
        let stateWithLambdaParams = { state with TypeEnv = Map.fold (fun acc k v -> Map.add k v acc) state.TypeEnv lambdaParamTypes }
        // First, lift any lambdas within the body
        liftLambdasInExpr body stateWithLambdaParams
        |> Result.bind (fun (body', state1) ->
            // Now lift this lambda itself to a closure
            let paramNames = parameters |> List.map fst |> Set.ofList
            let freeVarsInBody = freeVars body' paramNames
            // Filter to only include variables actually in TypeEnv (excludes top-level function names)
            let captures = freeVarsInBody |> Set.filter (fun name -> Map.containsKey name state.TypeEnv) |> Set.toList

            // Get actual types of captured variables from type environment
            let rec collectCaptureTypes remaining acc =
                match remaining with
                | [] -> Ok (List.rev acc)
                | name :: rest ->
                    match Map.tryFind name state.TypeEnv with
                    | Some t -> collectCaptureTypes rest (t :: acc)
                    | None -> Error $"Missing type for captured variable: {name}"

            collectCaptureTypes captures []
            |> Result.bind (fun captureTypes ->
                // Create lifted function
                let funcName = $"__closure_{state1.Counter}"
                // First element is function pointer (Int64), rest are captures with their actual types
                let closureTupleTypes = AST.TInt64 :: captureTypes
                let closureParam = ("__closure", AST.TTuple closureTupleTypes)

                // Build body that extracts captures from closure tuple
                let bodyWithExtractions =
                    if List.isEmpty captures then
                        body'
                    else
                        captures
                        |> List.mapi (fun i capName ->
                            (capName, AST.TupleAccess (AST.Var "__closure", i + 1)))
                        |> List.foldBack (fun (capName, accessor) acc ->
                            AST.Let (capName, accessor, acc)) <| body'

                let stateForReturnType = {
                    stateWithLambdaParams with
                        FuncParams = state1.FuncParams
                        FuncReturnTypes = state1.FuncReturnTypes
                        GenericFuncDefs = state1.GenericFuncDefs
                }

                inferLambdaReturnType body stateForReturnType
                |> Result.bind (fun returnType ->
                    let funcDef : AST.FunctionDef = {
                        Name = funcName
                        TypeParams = []
                        Params = closureParam :: parameters
                        ReturnType = returnType
                        Body = bodyWithExtractions
                    }
                    let state' = {
                        Counter = state1.Counter + 1
                        LiftedFunctions = funcDef :: state1.LiftedFunctions
                        TypeEnv = state.TypeEnv  // Restore original TypeEnv (exclude lambda params)
                        FuncParams = state1.FuncParams
                        FuncReturnTypes = state1.FuncReturnTypes
                        GenericFuncDefs = state1.GenericFuncDefs
                        TypeReg = state1.TypeReg
                        VariantLookup = state1.VariantLookup
                    }
                    // Replace lambda with Closure
                    let captureExprs = captures |> List.map AST.Var
                    Ok (AST.Closure (funcName, captureExprs), state'))))
    | AST.Apply (func, args) ->
        liftLambdasInExpr func state
        |> Result.bind (fun (func', state1) ->
            liftLambdasInArgs args state1
            |> Result.map (fun (args', state2) -> (AST.Apply (func', args'), state2)))
    | AST.InterpolatedString parts ->
        let rec liftParts (ps: AST.StringPart list) (st: LiftState) (acc: AST.StringPart list) : Result<AST.StringPart list * LiftState, string> =
            match ps with
            | [] -> Ok (List.rev acc, st)
            | AST.StringText s :: rest ->
                liftParts rest st (AST.StringText s :: acc)
            | AST.StringExpr e :: rest ->
                liftLambdasInExpr e st
                |> Result.bind (fun (e', st') ->
                    liftParts rest st' (AST.StringExpr e' :: acc))
        liftParts parts state []
        |> Result.map (fun (parts', state') -> (AST.InterpolatedString parts', state'))

/// Lift lambdas in function arguments, converting all lambdas to Closures
/// (even non-capturing lambdas become trivial closures for uniform calling convention)
/// Also wraps FuncRef in closures for uniform calling convention
and liftLambdasInArgs (args: AST.Expr list) (state: LiftState) : Result<AST.Expr list * LiftState, string> =
    let rec loop (remaining: AST.Expr list) (state: LiftState) (acc: AST.Expr list) =
        match remaining with
        | [] -> Ok (List.rev acc, state)
        | arg :: rest ->
            match arg with
            | AST.Lambda (parameters, body) ->
                // Add lambda parameters to type environment before processing body
                let lambdaParamTypes = parameters |> Map.ofList
                let stateWithLambdaParams = { state with TypeEnv = Map.fold (fun acc k v -> Map.add k v acc) state.TypeEnv lambdaParamTypes }
                // First, recursively lift any nested lambdas in the body
                liftLambdasInExpr body stateWithLambdaParams
                |> Result.bind (fun (body', state1) ->
                    // Check for free variables (captures)
                    let paramNames = parameters |> List.map fst |> Set.ofList
                    let freeVarsInBody = freeVars body' paramNames
                    // Filter to only include variables actually in TypeEnv (excludes top-level function names)
                    let captures = freeVarsInBody |> Set.filter (fun name -> Map.containsKey name state.TypeEnv) |> Set.toList

                    // Get actual types of captured variables from type environment
                    let rec collectCaptureTypes remaining acc =
                        match remaining with
                        | [] -> Ok (List.rev acc)
                        | name :: rest ->
                            match Map.tryFind name state.TypeEnv with
                            | Some t -> collectCaptureTypes rest (t :: acc)
                            | None -> Error $"Missing type for captured variable: {name}"

                    let buildLiftedFunc captureTypes =
                        // All lambdas become closures (even non-capturing ones) for uniform calling convention
                        // The lifted function takes closure as first param, then original params
                        let funcName = $"__closure_{state1.Counter}"
                        // First element is function pointer (Int64), rest are captures with their actual types
                        let closureTupleTypes = AST.TInt64 :: captureTypes
                        let closureParam = ("__closure", AST.TTuple closureTupleTypes)

                        // Build body that extracts captures from closure tuple:
                        // let cap1 = __closure.1 in let cap2 = __closure.2 in ... original_body
                        let bodyWithExtractions =
                            if List.isEmpty captures then
                                body'  // No captures to extract
                            else
                                captures
                                |> List.mapi (fun i capName ->
                                    // Capture at index i+1 (index 0 is the function pointer)
                                    (capName, AST.TupleAccess (AST.Var "__closure", i + 1)))
                                |> List.foldBack (fun (capName, accessor) acc ->
                                    AST.Let (capName, accessor, acc)) <| body'

                        let stateForReturnType = {
                            stateWithLambdaParams with
                                FuncParams = state1.FuncParams
                                FuncReturnTypes = state1.FuncReturnTypes
                                GenericFuncDefs = state1.GenericFuncDefs
                        }

                        inferLambdaReturnType body stateForReturnType
                        |> Result.bind (fun returnType ->
                            let funcDef : AST.FunctionDef = {
                                Name = funcName
                                TypeParams = []
                                Params = closureParam :: parameters  // Closure is always first param
                                ReturnType = returnType
                                Body = bodyWithExtractions
                            }
                            let state' = {
                                Counter = state1.Counter + 1
                                LiftedFunctions = funcDef :: state1.LiftedFunctions
                                TypeEnv = state.TypeEnv  // Restore original TypeEnv (exclude lambda params)
                                FuncParams = state1.FuncParams
                                FuncReturnTypes = state1.FuncReturnTypes
                                GenericFuncDefs = state1.GenericFuncDefs
                                TypeReg = state1.TypeReg
                                VariantLookup = state1.VariantLookup
                            }
                            // Replace lambda with Closure (captures may be empty for non-capturing lambdas)
                            let captureExprs = captures |> List.map AST.Var
                            loop rest state' (AST.Closure (funcName, captureExprs) :: acc))

                    collectCaptureTypes captures []
                    |> Result.bind buildLiftedFunc)

            | AST.FuncRef origFuncName ->
                // Named function used as value - wrap in a closure for uniform calling convention
                // Create wrapper: __funcref_wrapper_N(__closure, ...params) = origFunc(...params)
                // Look up the actual function signature to generate correct wrapper
                match Map.tryFind origFuncName state.FuncParams, Map.tryFind origFuncName state.FuncReturnTypes with
                | Some origParams, Some origReturnType ->
                    let wrapperName = $"__funcref_wrapper_{state.Counter}"
                    let closureParam = ("__closure", AST.TTuple [AST.TInt64])
                    // Generate parameter names for wrapper that match original function's parameters
                    let wrapperParams = origParams |> List.mapi (fun i (_, t) -> ($"__arg{i}", t))
                    let wrapperArgs = wrapperParams |> List.map (fun (name, _) -> AST.Var name)
                    let wrapperBody = AST.Call (origFuncName, wrapperArgs)
                    let wrapperDef : AST.FunctionDef = {
                        Name = wrapperName
                        TypeParams = []
                        Params = closureParam :: wrapperParams
                        ReturnType = origReturnType
                        Body = wrapperBody
                    }
                    let state' = {
                        Counter = state.Counter + 1
                        LiftedFunctions = wrapperDef :: state.LiftedFunctions
                        TypeEnv = state.TypeEnv
                        FuncParams = state.FuncParams
                        FuncReturnTypes = state.FuncReturnTypes
                        GenericFuncDefs = state.GenericFuncDefs
                        TypeReg = state.TypeReg
                        VariantLookup = state.VariantLookup
                    }
                    // Create trivial closure with no captures
                    loop rest state' (AST.Closure (wrapperName, []) :: acc)
                | None, _ ->
                    Error $"FuncRef to unknown function '{origFuncName}': function parameters not found"
                | _, None ->
                    Error $"FuncRef to unknown function '{origFuncName}': return type not found"

            | AST.Var varName ->
                // Check if this is a function being passed as value
                // For now, treat as potential function ref - will be handled at ANF level
                liftLambdasInExpr arg state
                |> Result.bind (fun (arg', state') -> loop rest state' (arg' :: acc))

            | other ->
                liftLambdasInExpr other state
                |> Result.bind (fun (other', state') -> loop rest state' (other' :: acc))
    loop args state []

/// Helper to lift lambdas in a list of expressions
and liftLambdasInList (exprs: AST.Expr list) (state: LiftState) : Result<AST.Expr list * LiftState, string> =
    let rec loop (remaining: AST.Expr list) (state: LiftState) (acc: AST.Expr list) =
        match remaining with
        | [] -> Ok (List.rev acc, state)
        | e :: rest ->
            liftLambdasInExpr e state
            |> Result.bind (fun (e', state') -> loop rest state' (e' :: acc))
    loop exprs state []

/// Helper to lift lambdas in record fields
and liftLambdasInFields (fields: (string * AST.Expr) list) (state: LiftState) : Result<(string * AST.Expr) list * LiftState, string> =
    let rec loop (remaining: (string * AST.Expr) list) (state: LiftState) (acc: (string * AST.Expr) list) =
        match remaining with
        | [] -> Ok (List.rev acc, state)
        | (name, e) :: rest ->
            liftLambdasInExpr e state
            |> Result.bind (fun (e', state') -> loop rest state' ((name, e') :: acc))
    loop fields state []

/// Helper to lift lambdas in match cases
and liftLambdasInCases (cases: AST.MatchCase list) (state: LiftState) : Result<AST.MatchCase list * LiftState, string> =
    let rec loop (remaining: AST.MatchCase list) (state: LiftState) (acc: AST.MatchCase list) =
        match remaining with
        | [] -> Ok (List.rev acc, state)
        | mc :: rest ->
            // Lift lambdas in guard if present
            let guardResult =
                match mc.Guard with
                | None -> Ok (None, state)
                | Some g ->
                    liftLambdasInExpr g state
                    |> Result.map (fun (g', s) -> (Some g', s))
            guardResult
            |> Result.bind (fun (guard', state1) ->
                liftLambdasInExpr mc.Body state1
                |> Result.bind (fun (body', state2) ->
                    let newCase = { mc with Guard = guard'; Body = body' }
                    loop rest state2 (newCase :: acc)))
    loop cases state []

/// Lift lambdas in a function definition
let liftLambdasInFunc (funcDef: AST.FunctionDef) (state: LiftState) : Result<AST.FunctionDef * LiftState, string> =
    // Add function parameters to the type environment
    let paramTypes = funcDef.Params |> List.map (fun (name, typ) -> (name, typ)) |> Map.ofList
    let stateWithParams = { state with TypeEnv = Map.fold (fun acc k v -> Map.add k v acc) state.TypeEnv paramTypes }
    liftLambdasInExpr funcDef.Body stateWithParams
    |> Result.map (fun (body', state') ->
        // Restore original TypeEnv (remove parameters) after processing the function
        ({ funcDef with Body = body' }, { state' with TypeEnv = state.TypeEnv }))

/// State extended to include known function names and their parameters
type LiftStateWithFuncs = {
    State: LiftState
    FuncParams: Map<string, (string * AST.Type) list>  // function name -> params (for generating wrappers)
    GeneratedWrappers: Map<string, string>  // original func name -> wrapper name
}

/// Generate a wrapper for a named function used as a value
let generateFuncWrapper (origFuncName: string) (funcParams: Map<string, (string * AST.Type) list>) (stateWithFuncs: LiftStateWithFuncs) : Result<(AST.FunctionDef * LiftStateWithFuncs), string> =
    match Map.tryFind origFuncName funcParams with
    | Some parameters ->
        // Create wrapper: __funcref_wrapper_N(__closure, ...params) = origFunc(...params)
        let wrapperName = $"__funcref_wrapper_{stateWithFuncs.State.Counter}"
        let closureParam = ("__closure", AST.TTuple [AST.TInt64])
        let wrapperBody = AST.Call (origFuncName, parameters |> List.map (fun (name, _) -> AST.Var name))
        let wrapperDef : AST.FunctionDef = {
            Name = wrapperName
            TypeParams = []
            Params = closureParam :: parameters
            ReturnType = AST.TInt64  // Simplified
            Body = wrapperBody
        }
        let newState = {
            stateWithFuncs with
                State = { stateWithFuncs.State with Counter = stateWithFuncs.State.Counter + 1 }
                GeneratedWrappers = Map.add origFuncName wrapperName stateWithFuncs.GeneratedWrappers
        }
        Ok (wrapperDef, newState)
    | None ->
        Error $"Cannot find parameters for function '{origFuncName}'"

/// Lift lambdas in a program, generating new top-level functions
let rec liftLambdasInProgram
    (baseTypeReg: TypeRegistry)
    (baseVariantLookup: VariantLookup)
    (baseFuncParams: Map<string, (string * AST.Type) list>)
    (baseFuncReturnTypes: Map<string, AST.Type>)
    (program: AST.Program)
    : Result<AST.Program, string> =
    let (AST.Program topLevels) = program

    let typeRegBase : TypeRegistry =
        topLevels
        |> List.choose (function
            | AST.TypeDef (AST.RecordDef (name, _typeParams, fields)) -> Some (name, fields)
            | _ -> None)
        |> Map.ofList

    let aliasReg : AliasRegistry =
        topLevels
        |> List.choose (function
            | AST.TypeDef (AST.TypeAlias (name, typeParams, targetType)) -> Some (name, (typeParams, targetType))
            | _ -> None)
        |> Map.ofList

    let typeReg = expandTypeRegWithAliases typeRegBase aliasReg

    let variantLookup : VariantLookup =
        topLevels
        |> List.choose (function
            | AST.TypeDef (AST.SumTypeDef (typeName, typeParams, variants)) ->
                Some (typeName, typeParams, variants)
            | _ -> None)
        |> List.collect (fun (typeName, typeParams, variants) ->
            variants
            |> List.mapi (fun idx variant -> (variant.Name, (typeName, typeParams, idx, variant.Payload))))
        |> Map.ofList

    let mergeMapsLocal m1 m2 = Map.fold (fun acc k v -> Map.add k v acc) m1 m2
    let mergedTypeReg = mergeMapsLocal baseTypeReg typeReg
    let mergedVariantLookup = mergeMapsLocal baseVariantLookup variantLookup

    // First pass: collect all function definitions and their parameters
    let userFuncParams : Map<string, (string * AST.Type) list> =
        topLevels
        |> List.choose (function
            | AST.FunctionDef f -> Some (f.Name, f.Params)
            | _ -> None)
        |> Map.ofList

    // Collect user function return types
    let userFuncReturnTypes : Map<string, AST.Type> =
        topLevels
        |> List.choose (function
            | AST.FunctionDef f -> Some (f.Name, f.ReturnType)
            | _ -> None)
        |> Map.ofList

    // Add module function parameters from Stdlib
    let moduleRegistry = Stdlib.buildModuleRegistry ()
    let moduleFuncParams : Map<string, (string * AST.Type) list> =
        moduleRegistry
        |> Map.toList
        |> List.map (fun (qualifiedName, moduleFunc) ->
            // Create parameter names like "arg0", "arg1" for each parameter type
            let paramList = moduleFunc.ParamTypes |> List.mapi (fun i t -> ($"arg{i}", t))
            (qualifiedName, paramList))
        |> Map.ofList

    // Collect module function return types
    let moduleFuncReturnTypes : Map<string, AST.Type> =
        moduleRegistry
        |> Map.toList
        |> List.map (fun (qualifiedName, moduleFunc) -> (qualifiedName, moduleFunc.ReturnType))
        |> Map.ofList

    // Collect user generic function definitions (for TypeApp substitution)
    let userGenericFuncDefs : Map<string, string list * AST.Type> =
        topLevels
        |> List.choose (function
            | AST.FunctionDef f when not (List.isEmpty f.TypeParams) ->
                Some (f.Name, (f.TypeParams, f.ReturnType))
            | _ -> None)
        |> Map.ofList

    // Collect module generic function definitions (for TypeApp substitution)
    let moduleGenericFuncDefs : Map<string, string list * AST.Type> =
        moduleRegistry
        |> Map.toList
        |> List.choose (fun (qualifiedName, moduleFunc) ->
            if not (List.isEmpty moduleFunc.TypeParams) then
                Some (qualifiedName, (moduleFunc.TypeParams, moduleFunc.ReturnType))
            else
                None)
        |> Map.ofList

    let funcParams =
        Map.fold (fun acc k v -> Map.add k v acc) baseFuncParams (Map.fold (fun acc k v -> Map.add k v acc) userFuncParams moduleFuncParams)
    let funcReturnTypes =
        Map.fold (fun acc k v -> Map.add k v acc) baseFuncReturnTypes (Map.fold (fun acc k v -> Map.add k v acc) userFuncReturnTypes moduleFuncReturnTypes)
    let genericFuncDefs = Map.fold (fun acc k v -> Map.add k v acc) userGenericFuncDefs moduleGenericFuncDefs

    let initialState = {
        Counter = 0
        LiftedFunctions = []
        TypeEnv = Map.empty
        FuncParams = funcParams
        FuncReturnTypes = funcReturnTypes
        GenericFuncDefs = genericFuncDefs
        TypeReg = mergedTypeReg
        VariantLookup = mergedVariantLookup
    }

    let rec processTopLevels (remaining: AST.TopLevel list) (state: LiftState) (acc: AST.TopLevel list) : Result<AST.TopLevel list * LiftState, string> =
        match remaining with
        | [] -> Ok (List.rev acc, state)
        | tl :: rest ->
            match tl with
            | AST.FunctionDef f ->
                liftLambdasInFunc f state
                |> Result.bind (fun (f', state') ->
                    processTopLevels rest state' (AST.FunctionDef f' :: acc))
            | AST.Expression e ->
                liftLambdasInExpr e state
                |> Result.bind (fun (e', state') ->
                    processTopLevels rest state' (AST.Expression e' :: acc))
            | AST.TypeDef t ->
                processTopLevels rest state (AST.TypeDef t :: acc)

    processTopLevels topLevels initialState []
    |> Result.bind (fun (topLevels', state') ->
        // Second pass: find all functions used as values and generate wrappers
        // Look for Var references to known functions in Call arguments
        let funcNamesUsedAsValues =
            topLevels'
            |> List.collect (function
                | AST.FunctionDef f -> collectFuncRefsInExpr f.Body funcParams
                | AST.Expression e -> collectFuncRefsInExpr e funcParams
                | _ -> [])
            |> List.distinct

        // Generate wrappers for functions used as values
        let stateWithFuncs = { State = state'; FuncParams = funcParams; GeneratedWrappers = Map.empty }
        let rec generateWrappers (funcNames: string list) (st: LiftStateWithFuncs) (wrapperAcc: AST.FunctionDef list) =
            match funcNames with
            | [] -> Ok (wrapperAcc, st)
            | name :: rest ->
                generateFuncWrapper name funcParams st
                |> Result.bind (fun (wrapperDef, st') ->
                    generateWrappers rest st' (wrapperDef :: wrapperAcc))

        generateWrappers funcNamesUsedAsValues stateWithFuncs []
        |> Result.map (fun (wrappers, finalStateWithFuncs) ->
            // Replace function references with wrapper references in the program
            let topLevels'' = topLevels' |> List.map (replaceFuncRefsWithWrappers finalStateWithFuncs.GeneratedWrappers)
            // Add wrappers and lifted functions to the program
            let liftedFuncDefs = (wrappers @ finalStateWithFuncs.State.LiftedFunctions) |> List.rev |> List.map AST.FunctionDef
            AST.Program (liftedFuncDefs @ topLevels'')))

/// Collect function names that are used as values (not in Call position)
and collectFuncRefsInExpr (expr: AST.Expr) (knownFuncs: Map<string, (string * AST.Type) list>) : string list =
    match expr with
    | AST.Call (_, args) ->
        // Check if any arg is a reference to a known function
        args
        |> List.collect (fun arg ->
            match arg with
            | AST.Var name when Map.containsKey name knownFuncs -> [name]
            | _ -> collectFuncRefsInExpr arg knownFuncs)
    | AST.Let (_, value, body) ->
        // Also check if value is a function reference being bound
        let valueRefs =
            match value with
            | AST.Var name when Map.containsKey name knownFuncs -> [name]
            | _ -> collectFuncRefsInExpr value knownFuncs
        valueRefs @ collectFuncRefsInExpr body knownFuncs
    | AST.If (c, t, e) ->
        collectFuncRefsInExpr c knownFuncs @ collectFuncRefsInExpr t knownFuncs @ collectFuncRefsInExpr e knownFuncs
    | AST.BinOp (_, l, r) ->
        collectFuncRefsInExpr l knownFuncs @ collectFuncRefsInExpr r knownFuncs
    | AST.UnaryOp (_, e) -> collectFuncRefsInExpr e knownFuncs
    | AST.TupleLiteral es | AST.ListLiteral es ->
        es |> List.collect (fun e -> collectFuncRefsInExpr e knownFuncs)
    | AST.ListCons (headElements, tail) ->
        (headElements |> List.collect (fun e -> collectFuncRefsInExpr e knownFuncs)) @
        collectFuncRefsInExpr tail knownFuncs
    | AST.TupleAccess (e, _) -> collectFuncRefsInExpr e knownFuncs
    | AST.RecordLiteral (_, fields) ->
        fields |> List.collect (fun (_, e) -> collectFuncRefsInExpr e knownFuncs)
    | AST.RecordAccess (e, _) -> collectFuncRefsInExpr e knownFuncs
    | AST.Constructor (_, _, payload) ->
        payload |> Option.map (fun e -> collectFuncRefsInExpr e knownFuncs) |> Option.defaultValue []
    | AST.Match (scrut, cases) ->
        collectFuncRefsInExpr scrut knownFuncs @ (cases |> List.collect (fun mc ->
            (mc.Guard |> Option.map (fun g -> collectFuncRefsInExpr g knownFuncs) |> Option.defaultValue []) @
            collectFuncRefsInExpr mc.Body knownFuncs))
    | AST.Lambda (_, body) -> collectFuncRefsInExpr body knownFuncs
    | AST.Apply (f, args) ->
        collectFuncRefsInExpr f knownFuncs @ (args |> List.collect (fun e -> collectFuncRefsInExpr e knownFuncs))
    | AST.Closure (_, caps) ->
        caps |> List.collect (fun e -> collectFuncRefsInExpr e knownFuncs)
    | AST.TypeApp (_, _, args) ->
        args |> List.collect (fun e -> collectFuncRefsInExpr e knownFuncs)
    | _ -> []

/// Replace function references with wrapper references in a TopLevel
and replaceFuncRefsWithWrappers (wrapperMap: Map<string, string>) (topLevel: AST.TopLevel) : AST.TopLevel =
    match topLevel with
    | AST.FunctionDef f ->
        AST.FunctionDef { f with Body = replaceInExpr wrapperMap f.Body }
    | AST.Expression e ->
        AST.Expression (replaceInExpr wrapperMap e)
    | AST.TypeDef t -> AST.TypeDef t

/// Replace function references with wrapper references in an expression
and replaceInExpr (wrapperMap: Map<string, string>) (expr: AST.Expr) : AST.Expr =
    match expr with
    | AST.Var name when Map.containsKey name wrapperMap ->
        // This is a function reference used as a value - replace with closure to wrapper
        AST.Closure (Map.find name wrapperMap, [])
    | AST.Closure (funcName, caps) ->
        // If this closure references a known function, use the wrapper instead
        let newFuncName = Map.tryFind funcName wrapperMap |> Option.defaultValue funcName
        AST.Closure (newFuncName, caps |> List.map (replaceInExpr wrapperMap))
    | AST.Call (name, args) ->
        AST.Call (name, args |> List.map (replaceInExpr wrapperMap))
    | AST.Let (n, v, b) ->
        AST.Let (n, replaceInExpr wrapperMap v, replaceInExpr wrapperMap b)
    | AST.If (c, t, e) ->
        AST.If (replaceInExpr wrapperMap c, replaceInExpr wrapperMap t, replaceInExpr wrapperMap e)
    | AST.BinOp (op, l, r) ->
        AST.BinOp (op, replaceInExpr wrapperMap l, replaceInExpr wrapperMap r)
    | AST.UnaryOp (op, e) ->
        AST.UnaryOp (op, replaceInExpr wrapperMap e)
    | AST.TupleLiteral es ->
        AST.TupleLiteral (es |> List.map (replaceInExpr wrapperMap))
    | AST.TupleAccess (e, i) ->
        AST.TupleAccess (replaceInExpr wrapperMap e, i)
    | AST.RecordLiteral (t, fields) ->
        AST.RecordLiteral (t, fields |> List.map (fun (n, e) -> (n, replaceInExpr wrapperMap e)))
    | AST.RecordAccess (e, f) ->
        AST.RecordAccess (replaceInExpr wrapperMap e, f)
    | AST.Constructor (t, v, payload) ->
        AST.Constructor (t, v, payload |> Option.map (replaceInExpr wrapperMap))
    | AST.Match (scrut, cases) ->
        AST.Match (replaceInExpr wrapperMap scrut,
                   cases |> List.map (fun mc -> { mc with Guard = mc.Guard |> Option.map (replaceInExpr wrapperMap); Body = replaceInExpr wrapperMap mc.Body }))
    | AST.ListLiteral es ->
        AST.ListLiteral (es |> List.map (replaceInExpr wrapperMap))
    | AST.ListCons (headElements, tail) ->
        AST.ListCons (headElements |> List.map (replaceInExpr wrapperMap), replaceInExpr wrapperMap tail)
    | AST.Lambda (ps, body) ->
        AST.Lambda (ps, replaceInExpr wrapperMap body)
    | AST.Apply (f, args) ->
        AST.Apply (replaceInExpr wrapperMap f, args |> List.map (replaceInExpr wrapperMap))
    | AST.TypeApp (n, ts, args) ->
        AST.TypeApp (n, ts, args |> List.map (replaceInExpr wrapperMap))
    | _ -> expr

/// Monomorphize a program: collect all specializations, generate specialized functions, replace TypeApps
/// Uses iterative approach: keep specializing until no new concrete TypeApps are found
let monomorphize (program: AST.Program) : AST.Program =
    let (AST.Program topLevels) = program

    // Collect generic function definitions
    let genericFuncDefs : GenericFuncDefs =
        topLevels
        |> List.choose (function
            | AST.FunctionDef f when not (List.isEmpty f.TypeParams) -> Some (f.Name, f)
            | _ -> None)
        |> Map.ofList

    // Collect initial specialization sites from non-generic functions and expressions
    let initialSpecs : Set<SpecKey> =
        topLevels
        |> List.map (function
            | AST.FunctionDef f when List.isEmpty f.TypeParams -> collectTypeAppsFromFunc f
            | AST.Expression e -> collectTypeApps e
            | _ -> Set.empty)
        |> List.fold Set.union Set.empty

    // Iterate: specialize, collect new TypeApps from specialized bodies, repeat
    let rec iterate (pendingSpecs: Set<SpecKey>) (processedSpecs: Set<SpecKey>) (accFuncs: AST.FunctionDef list) =
        // Filter to only specs not yet processed
        let newSpecs = Set.difference pendingSpecs processedSpecs
        if Set.isEmpty newSpecs then
            // No new specs, we're done
            accFuncs
        else
            // Generate specialized functions for new specs
            let (newFuncs, newPendingSpecs) =
                newSpecs
                |> Set.toList
                |> List.fold (fun (funcs, pending) (funcName, typeArgs) ->
                    match Map.tryFind funcName genericFuncDefs with
                    | Some funcDef ->
                        let specialized = specializeFunction funcDef typeArgs
                        // Collect TypeApps from the specialized body (these may be new specs)
                        let bodySpecs = collectTypeAppsFromFunc specialized
                        (specialized :: funcs, Set.union pending bodySpecs)
                    | None ->
                        (funcs, pending)) ([], Set.empty)

            // Continue with new pending specs
            iterate newPendingSpecs (Set.union processedSpecs newSpecs) (newFuncs @ accFuncs)

    // Run iterative specialization
    let specializedFuncs =
        iterate initialSpecs Set.empty []

    // Replace all TypeApps with Calls in the program
    let (specializedFuncsReplaced, transformedTopLevels) =
        let specializedFuncsReplaced = specializedFuncs |> List.map replaceTypeAppsInFunc
        let transformedTopLevels =
            topLevels
            |> List.choose (function
                | AST.FunctionDef f when not (List.isEmpty f.TypeParams) ->
                    // Skip generic function definitions (they're replaced by specializations)
                    None
                | AST.FunctionDef f ->
                    Some (AST.FunctionDef (replaceTypeAppsInFunc f))
                | AST.Expression e ->
                    Some (AST.Expression (replaceTypeApps e))
                | AST.TypeDef td ->
                    Some (AST.TypeDef td))
        (specializedFuncsReplaced, transformedTopLevels)

    // Add specialized functions to the program
    let specializationTopLevels =
        specializedFuncsReplaced |> List.map AST.FunctionDef

    AST.Program (specializationTopLevels @ transformedTopLevels)

/// Monomorphize a program with access to external generic function definitions.
/// Used when user code needs to specialize stdlib generics - the stdlib generic
/// function bodies are passed in as externalGenericDefs so they can be specialized
/// without merging the full stdlib AST with user code.
/// Uses iterative approach: keep specializing until no new concrete TypeApps are found
let monomorphizeWithExternalDefs (externalGenericDefs: GenericFuncDefs) (program: AST.Program) : AST.Program =
    let (AST.Program topLevels) = program

    // Collect generic function definitions from this program
    let localGenericDefs =
        extractGenericFuncDefs program

    // Merge external defs with local defs (local takes precedence)
    let genericFuncDefs =
        Map.fold (fun acc k v -> Map.add k v acc) externalGenericDefs localGenericDefs

    // Collect initial specialization sites from non-generic functions and expressions
    let initialSpecs : Set<SpecKey> =
        topLevels
        |> List.map (function
            | AST.FunctionDef f when List.isEmpty f.TypeParams -> collectTypeAppsFromFunc f
            | AST.Expression e -> collectTypeApps e
            | _ -> Set.empty)
        |> List.fold Set.union Set.empty

    // Iterate: specialize, collect new TypeApps from specialized bodies, repeat
    let rec iterate (pendingSpecs: Set<SpecKey>) (processedSpecs: Set<SpecKey>) (accFuncs: AST.FunctionDef list) =
        // Filter to only specs not yet processed
        let newSpecs = Set.difference pendingSpecs processedSpecs
        if Set.isEmpty newSpecs then
            // No new specs, we're done
            accFuncs
        else
            // Generate specialized functions for new specs
            let (newFuncs, newPendingSpecs) =
                newSpecs
                |> Set.toList
                |> List.fold (fun (funcs, pending) (funcName, typeArgs) ->
                    match Map.tryFind funcName genericFuncDefs with
                    | Some funcDef ->
                        let specialized = specializeFunction funcDef typeArgs
                        // Collect TypeApps from the specialized body (these may be new specs)
                        let bodySpecs = collectTypeAppsFromFunc specialized
                        (specialized :: funcs, Set.union pending bodySpecs)
                    | None ->
                        (funcs, pending)) ([], Set.empty)

            // Continue with new pending specs
            iterate newPendingSpecs (Set.union processedSpecs newSpecs) (newFuncs @ accFuncs)

    // Run iterative specialization
    let specializedFuncs =
        iterate initialSpecs Set.empty []

    // Replace all TypeApps with Calls in the program
    let (specializedFuncsReplaced, transformedTopLevels) =
        let specializedFuncsReplaced = specializedFuncs |> List.map replaceTypeAppsInFunc
        let transformedTopLevels =
            topLevels
            |> List.choose (function
                | AST.FunctionDef f when not (List.isEmpty f.TypeParams) ->
                    // Skip generic function definitions (they're replaced by specializations)
                    None
                | AST.FunctionDef f ->
                    Some (AST.FunctionDef (replaceTypeAppsInFunc f))
                | AST.Expression e ->
                    Some (AST.Expression (replaceTypeApps e))
                | AST.TypeDef td ->
                    Some (AST.TypeDef td))
        (specializedFuncsReplaced, transformedTopLevels)

    // Add specialized functions to the program
    let specializationTopLevels =
        specializedFuncsReplaced |> List.map AST.FunctionDef

    AST.Program (specializationTopLevels @ transformedTopLevels)

/// Convert AST.BinOp to ANF.BinOp
/// Note: StringConcat is handled separately as ANF.StringConcat CExpr
let convertBinOp (op: AST.BinOp) : ANF.BinOp =
    match op with
    | AST.Add -> ANF.Add
    | AST.Sub -> ANF.Sub
    | AST.Mul -> ANF.Mul
    | AST.Div -> ANF.Div
    | AST.Mod -> ANF.Mod
    | AST.Shl -> ANF.Shl
    | AST.Shr -> ANF.Shr
    | AST.BitAnd -> ANF.BitAnd
    | AST.BitOr -> ANF.BitOr
    | AST.BitXor -> ANF.BitXor
    | AST.Eq -> ANF.Eq
    | AST.Neq -> ANF.Neq
    | AST.Lt -> ANF.Lt
    | AST.Gt -> ANF.Gt
    | AST.Lte -> ANF.Lte
    | AST.Gte -> ANF.Gte
    | AST.And -> ANF.And
    | AST.Or -> ANF.Or
    | AST.StringConcat -> ANF.Add  // Never reached - StringConcat handled as CExpr

/// Convert AST.UnaryOp to ANF.UnaryOp
let convertUnaryOp (op: AST.UnaryOp) : ANF.UnaryOp =
    match op with
    | AST.Neg -> ANF.Neg
    | AST.Not -> ANF.Not
    | AST.BitNot -> ANF.BitNot

/// Check if a type requires structural equality (compound types)
let isCompoundType (typ: AST.Type) : bool =
    match typ with
    | AST.TTuple _ -> true
    | AST.TRecord _ -> true
    | AST.TSum _ -> true
    | _ -> false

/// Generate structural equality comparison for compound types.
/// Returns a list of bindings and the final result atom that holds the comparison result.
let rec generateStructuralEquality
    (leftAtom: ANF.Atom)
    (rightAtom: ANF.Atom)
    (typ: AST.Type)
    (varGen: ANF.VarGen)
    (typeReg: TypeRegistry)
    (variantLookup: VariantLookup)
    : (ANF.TempId * ANF.CExpr) list * ANF.Atom * ANF.VarGen =

    match typ with
    | AST.TTuple elemTypes ->
        // Compare each tuple element and AND the results
        let rec compareElements index types accBindings accResults vg =
            match types with
            | [] ->
                // AND all results together
                match accResults with
                | [] ->
                    // Empty tuple - always equal
                    let (trueVar, vg') = ANF.freshVar vg
                    (accBindings @ [(trueVar, ANF.Atom (ANF.BoolLiteral true))], ANF.Var trueVar, vg')
                | [single] ->
                    (accBindings, single, vg)
                | first :: rest ->
                    // Chain ANDs: result = r0 && r1 && r2 ...
                    let rec chainAnds results accBindings vg =
                        match results with
                        | [] -> Crash.crash "empty results in chainAnds"
                        | [last] -> (accBindings, last, vg)
                        | a :: b :: rest ->
                            let (andVar, vg') = ANF.freshVar vg
                            let andExpr = ANF.Prim (ANF.And, a, b)
                            chainAnds (ANF.Var andVar :: rest) (accBindings @ [(andVar, andExpr)]) vg'
                    chainAnds (first :: rest) accBindings vg

            | elemType :: restTypes ->
                // Get left[index] and right[index]
                let (leftElemVar, vg1) = ANF.freshVar vg
                let leftGet = ANF.TupleGet (leftAtom, index)
                let (rightElemVar, vg2) = ANF.freshVar vg1
                let rightGet = ANF.TupleGet (rightAtom, index)

                let baseBindings = accBindings @ [(leftElemVar, leftGet); (rightElemVar, rightGet)]

                // Check if element type is also compound
                if isCompoundType elemType then
                    // Recursive structural comparison
                    let (nestedBindings, resultAtom, vg3) =
                        generateStructuralEquality (ANF.Var leftElemVar) (ANF.Var rightElemVar) elemType vg2 typeReg variantLookup
                    compareElements (index + 1) restTypes (baseBindings @ nestedBindings) (accResults @ [resultAtom]) vg3
                else
                    // Primitive comparison
                    let (cmpVar, vg3) = ANF.freshVar vg2
                    let cmpExpr = ANF.Prim (ANF.Eq, ANF.Var leftElemVar, ANF.Var rightElemVar)
                    compareElements (index + 1) restTypes (baseBindings @ [(cmpVar, cmpExpr)]) (accResults @ [ANF.Var cmpVar]) vg3

        compareElements 0 elemTypes [] [] varGen

    | AST.TRecord (typeName, _) ->
        // Compare each record field and AND the results
        match Map.tryFind typeName typeReg with
        | None ->
            // Unknown record type - fall back to pointer comparison
            let (cmpVar, vg') = ANF.freshVar varGen
            ([(cmpVar, ANF.Prim (ANF.Eq, leftAtom, rightAtom))], ANF.Var cmpVar, vg')
        | Some fields ->
            let rec compareFields index fieldList accBindings accResults vg =
                match fieldList with
                | [] ->
                    match accResults with
                    | [] ->
                        let (trueVar, vg') = ANF.freshVar vg
                        (accBindings @ [(trueVar, ANF.Atom (ANF.BoolLiteral true))], ANF.Var trueVar, vg')
                    | [single] ->
                        (accBindings, single, vg)
                    | first :: rest ->
                        let rec chainAnds results accBindings vg =
                            match results with
                            | [] -> Crash.crash "empty results in chainAnds"
                            | [last] -> (accBindings, last, vg)
                            | a :: b :: rest ->
                                let (andVar, vg') = ANF.freshVar vg
                                let andExpr = ANF.Prim (ANF.And, a, b)
                                chainAnds (ANF.Var andVar :: rest) (accBindings @ [(andVar, andExpr)]) vg'
                        chainAnds (first :: rest) accBindings vg

                | (_, fieldType) :: restFields ->
                    // Get left.field and right.field (using TupleGet with field index)
                    let (leftFieldVar, vg1) = ANF.freshVar vg
                    let leftGet = ANF.TupleGet (leftAtom, index)
                    let (rightFieldVar, vg2) = ANF.freshVar vg1
                    let rightGet = ANF.TupleGet (rightAtom, index)

                    let baseBindings = accBindings @ [(leftFieldVar, leftGet); (rightFieldVar, rightGet)]

                    if isCompoundType fieldType then
                        let (nestedBindings, resultAtom, vg3) =
                            generateStructuralEquality (ANF.Var leftFieldVar) (ANF.Var rightFieldVar) fieldType vg2 typeReg variantLookup
                        compareFields (index + 1) restFields (baseBindings @ nestedBindings) (accResults @ [resultAtom]) vg3
                    else
                        let (cmpVar, vg3) = ANF.freshVar vg2
                        let cmpExpr = ANF.Prim (ANF.Eq, ANF.Var leftFieldVar, ANF.Var rightFieldVar)
                        compareFields (index + 1) restFields (baseBindings @ [(cmpVar, cmpExpr)]) (accResults @ [ANF.Var cmpVar]) vg3

            compareFields 0 fields [] [] varGen

    | AST.TSum (typeName, _) ->
        // Check if ANY variant in this type has a payload
        let typeVariants =
            variantLookup
            |> Map.filter (fun _ (tName, _, _, _) -> tName = typeName)
        let hasAnyPayload =
            typeVariants |> Map.exists (fun _ (_, _, _, pType) -> pType.IsSome)

        if not hasAnyPayload then
            // Pure enum - tags are integers, direct comparison works
            let (cmpVar, vg') = ANF.freshVar varGen
            ([(cmpVar, ANF.Prim (ANF.Eq, leftAtom, rightAtom))], ANF.Var cmpVar, vg')
        else
            // Mixed enum - uniform [tag, payload] representation
            // Compare tag (index 0) AND payload (index 1)
            let (leftTagVar, vg1) = ANF.freshVar varGen
            let (rightTagVar, vg2) = ANF.freshVar vg1
            let (tagEqVar, vg3) = ANF.freshVar vg2
            let (leftPayloadVar, vg4) = ANF.freshVar vg3
            let (rightPayloadVar, vg5) = ANF.freshVar vg4
            let (payloadEqVar, vg6) = ANF.freshVar vg5
            let (resultVar, vg7) = ANF.freshVar vg6

            let bindings = [
                (leftTagVar, ANF.TupleGet (leftAtom, 0))
                (rightTagVar, ANF.TupleGet (rightAtom, 0))
                (tagEqVar, ANF.Prim (ANF.Eq, ANF.Var leftTagVar, ANF.Var rightTagVar))
                (leftPayloadVar, ANF.TupleGet (leftAtom, 1))
                (rightPayloadVar, ANF.TupleGet (rightAtom, 1))
                (payloadEqVar, ANF.Prim (ANF.Eq, ANF.Var leftPayloadVar, ANF.Var rightPayloadVar))
                (resultVar, ANF.Prim (ANF.And, ANF.Var tagEqVar, ANF.Var payloadEqVar))
            ]
            (bindings, ANF.Var resultVar, vg7)

    | _ ->
        // Primitive types - simple comparison
        let (cmpVar, vg') = ANF.freshVar varGen
        ([(cmpVar, ANF.Prim (ANF.Eq, leftAtom, rightAtom))], ANF.Var cmpVar, vg')

/// Infer the type of an expression using type environment and registries
/// Used for type-directed field lookup in record access
let rec inferType (expr: AST.Expr) (typeEnv: Map<string, AST.Type>) (typeReg: TypeRegistry) (variantLookup: VariantLookup) (funcReg: FunctionRegistry) (moduleRegistry: AST.ModuleRegistry) : Result<AST.Type, string> =
    match expr with
    | AST.UnitLiteral -> Ok AST.TUnit
    | AST.Int64Literal _ -> Ok AST.TInt64
    | AST.Int8Literal _ -> Ok AST.TInt8
    | AST.Int16Literal _ -> Ok AST.TInt16
    | AST.Int32Literal _ -> Ok AST.TInt32
    | AST.UInt8Literal _ -> Ok AST.TUInt8
    | AST.UInt16Literal _ -> Ok AST.TUInt16
    | AST.UInt32Literal _ -> Ok AST.TUInt32
    | AST.UInt64Literal _ -> Ok AST.TUInt64
    | AST.BoolLiteral _ -> Ok AST.TBool
    | AST.StringLiteral _ -> Ok AST.TString
    | AST.CharLiteral _ -> Ok AST.TChar
    | AST.FloatLiteral _ -> Ok AST.TFloat64
    | AST.Var name ->
        match Map.tryFind name typeEnv with
        | Some t -> Ok t
        | None ->
            // Check if it's a module function (e.g., Stdlib.Int64.add)
            match Stdlib.tryGetFunctionWithFallback moduleRegistry name with
            | Some (moduleFunc, _) -> Ok (Stdlib.getFunctionType moduleFunc)
            | None -> Error $"Cannot infer type: undefined variable '{name}'"
    | AST.RecordLiteral (typeName, fields) ->
        if typeName = "" then
            // Anonymous record literal - try to find matching type by field names
            let literalFieldNames = fields |> List.map fst |> Set.ofList
            let matchingTypes =
                typeReg
                |> Map.toList
                |> List.filter (fun (_, typeFields) ->
                    let typeFieldNames = typeFields |> List.map fst |> Set.ofList
                    typeFieldNames = literalFieldNames)
                |> List.map fst
            match matchingTypes with
            | [singleMatch] -> Ok (AST.TRecord (singleMatch, []))
            | [] -> Error "Cannot infer type: no record type matches the field names"
            | matches ->
                let names = String.concat ", " matches
                Error $"Ambiguous record literal: matches multiple types: {names}"
        else
            Ok (AST.TRecord (typeName, []))
    | AST.RecordUpdate (recordExpr, _) ->
        // Record update returns the same type as the record being updated
        inferType recordExpr typeEnv typeReg variantLookup funcReg moduleRegistry
    | AST.RecordAccess (recordExpr, fieldName) ->
        inferType recordExpr typeEnv typeReg variantLookup funcReg moduleRegistry
        |> Result.bind (fun recordType ->
            match recordType with
            | AST.TRecord (typeName, _) ->
                match Map.tryFind typeName typeReg with
                | Some fields ->
                    match List.tryFind (fun (name, _) -> name = fieldName) fields with
                    | Some (_, fieldType) -> Ok fieldType
                    | None -> Error $"Record type {typeName} has no field '{fieldName}'"
                | None -> Error $"Unknown record type: {typeName}"
            | _ -> Error $"Cannot access field on non-record type")
    | AST.TupleLiteral elems ->
        elems
        |> List.map (fun e -> inferType e typeEnv typeReg variantLookup funcReg moduleRegistry)
        |> List.fold (fun acc r ->
            match acc, r with
            | Ok types, Ok t -> Ok (types @ [t])
            | Error e, _ -> Error e
            | _, Error e -> Error e) (Ok [])
        |> Result.map AST.TTuple
    | AST.TupleAccess (tupleExpr, index) ->
        inferType tupleExpr typeEnv typeReg variantLookup funcReg moduleRegistry
        |> Result.bind (fun tupleType ->
            match tupleType with
            | AST.TTuple elemTypes when index >= 0 && index < List.length elemTypes ->
                Ok (List.item index elemTypes)
            | AST.TTuple _ -> Error $"Tuple index {index} out of bounds"
            | _ -> Error "Cannot access index on non-tuple type")
    | AST.Constructor (_, variantName, _) ->
        match Map.tryFind variantName variantLookup with
        | Some (typeName, _, _, _) -> Ok (AST.TSum (typeName, []))  // Type args inferred during type checking
        | None -> Error $"Unknown constructor: {variantName}"
    | AST.ListLiteral elements ->
        match elements with
        | [] -> Ok (AST.TList (AST.TVar "t"))  // Preserve unknown element type for empty lists
        | first :: _ ->
            inferType first typeEnv typeReg variantLookup funcReg moduleRegistry
            |> Result.map (fun elemType -> AST.TList elemType)
    | AST.ListCons (headElements, tail) ->
        // List cons has same element type as tail, but refine unknown element types from heads.
        inferType tail typeEnv typeReg variantLookup funcReg moduleRegistry
        |> Result.bind (fun tailType ->
            match tailType with
            | AST.TList elemType ->
                let reconcileElemType (current: AST.Type) (next: AST.Type) : Result<AST.Type, string> =
                    if containsTypeVar current && not (containsTypeVar next) then Ok next
                    elif containsTypeVar next && not (containsTypeVar current) then Ok current
                    elif current = next then Ok current
                    else Error $"List cons element type mismatch: {typeToString current} vs {typeToString next}"

                let rec refineElemType (current: AST.Type) (elems: AST.Expr list) : Result<AST.Type, string> =
                    match elems with
                    | [] -> Ok current
                    | head :: rest ->
                        inferType head typeEnv typeReg variantLookup funcReg moduleRegistry
                        |> Result.bind (fun headType ->
                            reconcileElemType current headType
                            |> Result.bind (fun refined -> refineElemType refined rest))

                refineElemType elemType headElements
                |> Result.map (fun finalElemType -> AST.TList finalElemType)
            | _ -> Ok tailType)
    | AST.Let (name, value, body) ->
        inferType value typeEnv typeReg variantLookup funcReg moduleRegistry
        |> Result.bind (fun valueType ->
            let typeEnv' = Map.add name valueType typeEnv
            inferType body typeEnv' typeReg variantLookup funcReg moduleRegistry)
    | AST.If (_, thenExpr, _) ->
        // Both branches should have same type, just infer from then branch
        inferType thenExpr typeEnv typeReg variantLookup funcReg moduleRegistry
    | AST.BinOp (op, left, right) ->
        let ensureSameType () =
            inferType left typeEnv typeReg variantLookup funcReg moduleRegistry
            |> Result.bind (fun leftType ->
                inferType right typeEnv typeReg variantLookup funcReg moduleRegistry
                |> Result.bind (fun rightType ->
                    if leftType = rightType then Ok leftType
                    else Error $"Binary operator operands must match: left={leftType}, right={rightType}"))
        match op with
        | AST.Add | AST.Sub | AST.Mul | AST.Div | AST.Mod ->
            ensureSameType ()
            |> Result.bind (fun operandType ->
                match operandType with
                | AST.TInt8 | AST.TInt16 | AST.TInt32 | AST.TInt64
                | AST.TUInt8 | AST.TUInt16 | AST.TUInt32 | AST.TUInt64
                | AST.TFloat64 -> Ok operandType
                | _ -> Error $"Arithmetic operator requires numeric operands, got {operandType}")
        | AST.Shl | AST.Shr | AST.BitAnd | AST.BitOr | AST.BitXor ->
            ensureSameType ()
            |> Result.bind (fun operandType ->
                match operandType with
                | AST.TInt8 | AST.TInt16 | AST.TInt32 | AST.TInt64
                | AST.TUInt8 | AST.TUInt16 | AST.TUInt32 | AST.TUInt64 -> Ok operandType
                | _ -> Error $"Bitwise operator requires integer operands, got {operandType}")
        | AST.Eq | AST.Neq -> Ok AST.TBool
        | AST.Lt | AST.Gt | AST.Lte | AST.Gte ->
            ensureSameType ()
            |> Result.bind (fun operandType ->
                match operandType with
                | AST.TInt8 | AST.TInt16 | AST.TInt32 | AST.TInt64
                | AST.TUInt8 | AST.TUInt16 | AST.TUInt32 | AST.TUInt64
                | AST.TFloat64 -> Ok AST.TBool
                | _ -> Error $"Comparison operator requires numeric operands, got {operandType}")
        | AST.And | AST.Or -> Ok AST.TBool
        | AST.StringConcat -> Ok AST.TString
    | AST.UnaryOp (op, inner) ->
        inferType inner typeEnv typeReg variantLookup funcReg moduleRegistry
        |> Result.bind (fun innerType ->
            match op with
            | AST.Neg ->
                match innerType with
                | AST.TInt8 | AST.TInt16 | AST.TInt32 | AST.TInt64 | AST.TFloat64 -> Ok innerType
                | _ -> Error $"Negation requires numeric operand, got {innerType}"
            | AST.Not ->
                match innerType with
                | AST.TBool -> Ok AST.TBool
                | _ -> Error $"Logical not requires Bool operand, got {innerType}"
            | AST.BitNot ->
                match innerType with
                | AST.TInt8 | AST.TInt16 | AST.TInt32 | AST.TInt64 -> Ok innerType
                | _ -> Error $"Bitwise not requires integer operand, got {innerType}")
    | AST.Match (scrutinee, cases) ->
        // Infer from first case body, but first extend environment with pattern variables
        // Infer scrutinee type to help with pattern variable typing
        let scrutineeTypeResult = inferType scrutinee typeEnv typeReg variantLookup funcReg moduleRegistry

        let rec substituteType (subst: Map<string, AST.Type>) (typ: AST.Type) : AST.Type =
            match typ with
            | AST.TVar name -> Map.tryFind name subst |> Option.defaultValue typ
            | AST.TTuple elems -> AST.TTuple (List.map (substituteType subst) elems)
            | AST.TRecord (name, args) -> AST.TRecord (name, List.map (substituteType subst) args)
            | AST.TList elem -> AST.TList (substituteType subst elem)
            | AST.TDict (k, v) -> AST.TDict (substituteType subst k, substituteType subst v)
            | AST.TSum (name, args) -> AST.TSum (name, List.map (substituteType subst) args)
            | AST.TFunction (args, ret) -> AST.TFunction (List.map (substituteType subst) args, substituteType subst ret)
            | _ -> typ

        // Helper to extract pattern variable names and infer their types
        let rec extractPatternBindings (pattern: AST.Pattern) (scrutType: AST.Type) : Map<string, AST.Type> =
            match pattern with
            | AST.PVar name -> Map.ofList [(name, scrutType)]
            | AST.PWildcard -> Map.empty
            | AST.PInt64 _
            | AST.PInt8Literal _
            | AST.PInt16Literal _
            | AST.PInt32Literal _
            | AST.PUInt8Literal _
            | AST.PUInt16Literal _
            | AST.PUInt32Literal _
            | AST.PUInt64Literal _
            | AST.PUnit
            | AST.PBool _
            | AST.PString _
            | AST.PFloat _ -> Map.empty
            | AST.PTuple innerPats ->
                match scrutType with
                | AST.TTuple elemTypes when List.length elemTypes = List.length innerPats ->
                    List.zip innerPats elemTypes
                    |> List.fold (fun acc (pat, typ) -> Map.fold (fun m k v -> Map.add k v m) acc (extractPatternBindings pat typ)) Map.empty
                | _ -> List.fold (fun acc pat -> Map.fold (fun m k v -> Map.add k v m) acc (extractPatternBindings pat AST.TInt64)) Map.empty innerPats
            | AST.PRecord (patternRecordName, fieldPats) ->
                // Preserve concrete field types from the matched record.
                // Falling back to Int64 here causes downstream mis-lowering (e.g. string == uses pointer eq).
                let recordFields =
                    match scrutType with
                    | AST.TRecord (scrutRecordName, _) ->
                        if Map.containsKey scrutRecordName typeReg then
                            Map.find scrutRecordName typeReg
                        elif Map.containsKey patternRecordName typeReg then
                            Map.find patternRecordName typeReg
                        else
                            Crash.crash $"PRecord pattern could not find record type '{scrutRecordName}' (pattern: '{patternRecordName}')"
                    | _ ->
                        Crash.crash $"PRecord pattern expects TRecord scrutinee, got {scrutType}"

                fieldPats
                |> List.fold (fun acc (fieldName, pat) ->
                    let fieldType =
                        match List.tryFind (fun (name, _) -> name = fieldName) recordFields with
                        | Some (_, typ) -> typ
                        | None -> Crash.crash $"PRecord pattern field '{fieldName}' not found on record '{patternRecordName}'"

                    Map.fold
                        (fun m k v -> Map.add k v m)
                        acc
                        (extractPatternBindings pat fieldType))
                    Map.empty
            | AST.PConstructor (variantName, payloadPat) ->
                match payloadPat with
                | None -> Map.empty
                | Some payloadPattern ->
                    let payloadType =
                        match Map.tryFind variantName variantLookup with
                        | Some (_, typeParams, _, Some payloadTypeTemplate) ->
                            match scrutType with
                            | AST.TSum (_, typeArgs) when List.length typeParams = List.length typeArgs ->
                                let subst = List.zip typeParams typeArgs |> Map.ofList
                                substituteType subst payloadTypeTemplate
                            | _ -> payloadTypeTemplate
                        | Some (_, _, _, None) ->
                            Crash.crash $"Constructor '{variantName}' has no payload type"
                        | None ->
                            Crash.crash $"Unknown constructor '{variantName}' in pattern"
                    extractPatternBindings payloadPattern payloadType
            | AST.PList innerPats ->
                let elemType =
                    match scrutType with
                    | AST.TList t -> t
                    | _ -> Crash.crash $"PList pattern expects TList scrutinee, got {scrutType}"
                innerPats |> List.fold (fun acc pat -> Map.fold (fun m k v -> Map.add k v m) acc (extractPatternBindings pat elemType)) Map.empty
            | AST.PListCons (headPats, tailPat) ->
                let elemType =
                    match scrutType with
                    | AST.TList t -> t
                    | _ -> Crash.crash $"PListCons pattern expects TList scrutinee, got {scrutType}"
                let headBindings = headPats |> List.fold (fun acc pat -> Map.fold (fun m k v -> Map.add k v m) acc (extractPatternBindings pat elemType)) Map.empty
                let tailBindings = extractPatternBindings tailPat scrutType
                Map.fold (fun m k v -> Map.add k v m) headBindings tailBindings

        match cases with
        | mc :: _ ->
            let patternType =
                match scrutineeTypeResult with
                | Ok t -> t
                | Error msg -> Crash.crash $"Pattern match: Could not determine scrutinee type: {msg}"
            // Get bindings from first pattern (cases can have multiple patterns, use first)
            let patBindings =
                mc.Patterns
                |> AST.NonEmptyList.toList
                |> List.fold (fun acc pat -> Map.fold (fun m k v -> Map.add k v m) acc (extractPatternBindings pat patternType)) Map.empty
            let typeEnv' = Map.fold (fun m k v -> Map.add k v m) typeEnv patBindings
            inferType mc.Body typeEnv' typeReg variantLookup funcReg moduleRegistry
        | [] -> Error "Empty match expression"
    | AST.Call (funcName, args) ->
        if isBuiltinUnwrapName funcName then
            match args with
            | [argExpr] ->
                inferType argExpr typeEnv typeReg variantLookup funcReg moduleRegistry
                |> Result.bind (fun argType ->
                    match argType with
                    | AST.TSum ("Stdlib.Option.Option", [valueType]) -> Ok valueType
                    | AST.TSum ("Stdlib.Result.Result", [okType; _]) -> Ok okType
                    | AST.TSum ("Stdlib.Option.Option", []) ->
                        match argExpr with
                        | AST.Constructor (_, "Some", Some payloadExpr) ->
                            inferType payloadExpr typeEnv typeReg variantLookup funcReg moduleRegistry
                        | _ ->
                            // Type args may be unavailable in ANF inferType.
                            // Use Unit to avoid leaking unresolved type variables into later passes.
                            Ok AST.TUnit
                    | AST.TSum ("Stdlib.Result.Result", []) ->
                        match argExpr with
                        | AST.Constructor (_, "Ok", Some payloadExpr) ->
                            inferType payloadExpr typeEnv typeReg variantLookup funcReg moduleRegistry
                        | _ ->
                            // Type args may be unavailable in ANF inferType.
                            // Use Unit to avoid leaking unresolved type variables into later passes.
                            Ok AST.TUnit
                    | _ ->
                        Error $"Internal error: Builtin.unwrap expects Option/Result argument, got {typeToString argType}")
            | _ ->
                Error $"Internal error: Builtin.unwrap expects 1 argument, got {List.length args}"
        else
            // Look up function return type from the function registry
            match Map.tryFind funcName funcReg with
            | Some (AST.TFunction (_, returnType)) -> Ok returnType
            | Some _ -> Error $"Expected function type for {funcName} in funcReg"
            | None ->
                // Check if it's a function parameter (variable with function type)
                match Map.tryFind funcName typeEnv with
                | Some (AST.TFunction (_, returnType)) -> Ok returnType
                | _ ->
                // Check if it's a module function (e.g., Stdlib.File.exists)
                match Stdlib.tryGetFunctionWithFallback moduleRegistry funcName with
                | Some (moduleFunc, _) -> Ok moduleFunc.ReturnType
                | None ->
                    // Check if it's a monomorphized intrinsic (e.g., __raw_get_i64)
                    // These are raw memory operations that work with 8-byte values
                    if funcName.StartsWith("__raw_get_") then
                        // __raw_get<T> returns T, but at ANF level it's just Int64 (8 bytes)
                        Ok AST.TInt64
                    elif funcName.StartsWith("__raw_set_") then
                        // __raw_set<T> returns Unit
                        Ok AST.TUnit
                    // Key intrinsics for Dict - monomorphized versions
                    elif funcName.StartsWith("__hash_") then
                        // __hash<k> returns Int64 (hash value)
                        Ok AST.TInt64
                    elif funcName.StartsWith("__key_eq_") then
                        // __key_eq<k> returns Bool (equality check)
                        Ok AST.TBool
                    // Dict intrinsics - monomorphized versions
                    elif funcName.StartsWith("__empty_dict_") then
                        // __empty_dict<k, v> returns Dict<k, v> - but at ANF level it's Int64 (null ptr)
                        Ok AST.TInt64
                    elif funcName.StartsWith("__dict_is_null_") then
                        // __dict_is_null<k, v> returns Bool
                        Ok AST.TBool
                    elif funcName.StartsWith("__dict_get_tag_") then
                        // __dict_get_tag<k, v> returns Int64 (tag bits)
                        Ok AST.TInt64
                    elif funcName.StartsWith("__dict_to_rawptr_") then
                        // __dict_to_rawptr<k, v> returns RawPtr (as Int64)
                        Ok AST.TInt64
                    elif funcName.StartsWith("__rawptr_to_dict_") then
                        // __rawptr_to_dict<k, v> returns Dict<k, v> (as Int64)
                        Ok AST.TInt64
                    // List intrinsics - monomorphized versions for Finger Tree
                    elif funcName.StartsWith("__list_is_null_") then
                        // __list_is_null<a> returns Bool
                        Ok AST.TBool
                    elif funcName.StartsWith("__list_get_tag_") then
                        // __list_get_tag<a> returns Int64 (tag bits)
                        Ok AST.TInt64
                    elif funcName.StartsWith("__list_to_rawptr_") then
                        // __list_to_rawptr<a> returns RawPtr (as Int64)
                        Ok AST.TInt64
                    elif funcName.StartsWith("__rawptr_to_list_") then
                        // __rawptr_to_list<a> returns List<a> - parse element type from mangled name
                        let suffix = funcName.Substring("__rawptr_to_list_".Length)
                        tryParseMangledType variantLookup suffix
                        |> Result.map AST.TList
                    elif funcName.StartsWith("__list_empty_") then
                        // __list_empty<a> returns List<a> - but at ANF level it's Int64 (null ptr)
                        Ok AST.TInt64
                    else
                        Error $"Unknown function: '{funcName}'"
    | AST.TypeApp (_funcName, _typeArgs, _args) ->
        // Generic function call - not yet implemented
        Error "Generic function calls not yet implemented"
    | AST.Lambda (parameters, body) ->
        // Lambda has function type (paramTypes) -> returnType
        let paramTypes = parameters |> List.map snd
        let typeEnv' = parameters |> List.fold (fun env (name, ty) -> Map.add name ty env) typeEnv
        inferType body typeEnv' typeReg variantLookup funcReg moduleRegistry
        |> Result.map (fun returnType -> AST.TFunction (paramTypes, returnType))
    | AST.Apply (func, _args) ->
        // Apply result is the return type of the function
        inferType func typeEnv typeReg variantLookup funcReg moduleRegistry
        |> Result.bind (fun funcType ->
            match funcType with
            | AST.TFunction (_, returnType) -> Ok returnType
            | _ -> Error "Apply requires a function type")
    | AST.FuncRef name ->
        // Function reference has the function's type
        match Map.tryFind name funcReg with
        | Some returnType -> Ok returnType
        | None -> Error $"Cannot infer type: undefined function '{name}'"
    | AST.Closure (funcName, _) ->
        // Closure has function type (without the closure param)
        match Map.tryFind funcName funcReg with
        | Some (AST.TFunction (_ :: restParams, returnType)) ->
            Ok (AST.TFunction (restParams, returnType))
        | Some funcType -> Ok funcType
        | None -> Error $"Cannot infer type: undefined closure function '{funcName}'"
    | AST.InterpolatedString _ ->
        // Interpolated strings are always String type
        Ok AST.TString

/// Convert AST expression to ANF
/// env maps user variable names to ANF TempIds and their types
/// typeReg maps record type names to field definitions
/// variantLookup maps variant names to (type name, tag index)
/// funcReg maps function names to their return types
let rec toANF (expr: AST.Expr) (varGen: ANF.VarGen) (env: VarEnv) (typeReg: TypeRegistry) (variantLookup: VariantLookup) (funcReg: FunctionRegistry) (moduleRegistry: AST.ModuleRegistry) : Result<ANF.AExpr * ANF.VarGen, string> =
    match expr with
    | AST.UnitLiteral ->
        // Unit literal becomes return of unit value (represented as 0)
        Ok (ANF.Return (ANF.UnitLiteral), varGen)

    | AST.Int64Literal n ->
        // Integer literal (default Int64)
        Ok (ANF.Return (ANF.IntLiteral (ANF.Int64 n)), varGen)

    | AST.Int8Literal n ->
        Ok (ANF.Return (ANF.IntLiteral (ANF.Int8 n)), varGen)

    | AST.Int16Literal n ->
        Ok (ANF.Return (ANF.IntLiteral (ANF.Int16 n)), varGen)

    | AST.Int32Literal n ->
        Ok (ANF.Return (ANF.IntLiteral (ANF.Int32 n)), varGen)

    | AST.UInt8Literal n ->
        Ok (ANF.Return (ANF.IntLiteral (ANF.UInt8 n)), varGen)

    | AST.UInt16Literal n ->
        Ok (ANF.Return (ANF.IntLiteral (ANF.UInt16 n)), varGen)

    | AST.UInt32Literal n ->
        Ok (ANF.Return (ANF.IntLiteral (ANF.UInt32 n)), varGen)

    | AST.UInt64Literal n ->
        Ok (ANF.Return (ANF.IntLiteral (ANF.UInt64 n)), varGen)

    | AST.BoolLiteral b ->
        // Boolean literal becomes return
        Ok (ANF.Return (ANF.BoolLiteral b), varGen)

    | AST.StringLiteral s ->
        // String literal becomes return
        Ok (ANF.Return (ANF.StringLiteral s), varGen)

    | AST.CharLiteral s ->
        // Char literal becomes return (stored as string, same runtime representation)
        Ok (ANF.Return (ANF.StringLiteral s), varGen)

    | AST.FloatLiteral f ->
        // Float literal becomes return
        Ok (ANF.Return (ANF.FloatLiteral f), varGen)

    | AST.Var name ->
        // Variable reference: look up in environment
        match Map.tryFind name env with
        | Some (tempId, _) -> Ok (ANF.Return (ANF.Var tempId), varGen)
        | None ->
            // Check if it's a module function (e.g., Stdlib.Int64.add)
            match Stdlib.tryGetFunctionWithFallback moduleRegistry name with
            | Some (_, _) ->
                // Module function reference - wrap in closure for uniform calling convention
                // Note: name should already be resolved by the type checker
                let (closureId, varGen') = ANF.freshVar varGen
                let closureAlloc = ANF.ClosureAlloc (name, [])
                Ok (ANF.Let (closureId, closureAlloc, ANF.Return (ANF.Var closureId)), varGen')
            | None ->
                // Check if it's a function reference (function name used as value)
                if Map.containsKey name funcReg then
                    // Wrap in closure for uniform calling convention
                    let (closureId, varGen') = ANF.freshVar varGen
                    let closureAlloc = ANF.ClosureAlloc (name, [])
                    Ok (ANF.Let (closureId, closureAlloc, ANF.Return (ANF.Var closureId)), varGen')
                else
                    Error $"Undefined variable: {name}"

    | AST.FuncRef name ->
        // Explicit function reference - wrap in closure for uniform calling convention
        let (closureId, varGen') = ANF.freshVar varGen
        let closureAlloc = ANF.ClosureAlloc (name, [])
        Ok (ANF.Let (closureId, closureAlloc, ANF.Return (ANF.Var closureId)), varGen')

    | AST.Closure (funcName, captures) ->
        // Closure: allocate closure tuple with function address and captured values
        // Convert each capture expression to an atom
        let rec convertCaptures (caps: AST.Expr list) (vg: ANF.VarGen) (acc: (ANF.Atom * (ANF.TempId * ANF.CExpr) list) list) =
            match caps with
            | [] -> Ok (List.rev acc, vg)
            | cap :: rest ->
                toAtom cap vg env typeReg variantLookup funcReg moduleRegistry
                |> Result.bind (fun (capAtom, capBindings, vg') ->
                    convertCaptures rest vg' ((capAtom, capBindings) :: acc))
        convertCaptures captures varGen []
        |> Result.map (fun (captureResults, varGen1) ->
            let captureAtoms = captureResults |> List.map fst
            let allBindings = captureResults |> List.collect snd
            // Generate ClosureAlloc: allocate closure tuple
            let (closureId, varGen2) = ANF.freshVar varGen1
            let closureAlloc = ANF.ClosureAlloc (funcName, captureAtoms)
            let finalExpr = ANF.Let (closureId, closureAlloc, ANF.Return (ANF.Var closureId))
            let exprWithBindings = wrapBindings allBindings finalExpr
            (exprWithBindings, varGen2))

    | AST.Let (name, value, body) ->
        // Let binding: convert value to atom, allocate fresh temp, convert body with extended env
        // Infer the type of the value for type-directed field lookup
        let typeEnv = typeEnvFromVarEnv env
        inferType value typeEnv typeReg variantLookup funcReg moduleRegistry
        |> Result.bind (fun valueType ->
            // Try toAtom first; if it fails for complex expressions like Match, use toANF
            match toAtom value varGen env typeReg variantLookup funcReg moduleRegistry with
            | Ok (valueAtom, valueBindings, varGen1) ->
                let (tempId, varGen2) = ANF.freshVar varGen1
                let env' = Map.add name (tempId, valueType) env
                toANF body varGen2 env' typeReg variantLookup funcReg moduleRegistry |> Result.map (fun (bodyExpr, varGen3) ->
                    // Build: valueBindings + let tempId = valueAtom + body
                    let finalExpr = ANF.Let (tempId, ANF.Atom valueAtom, bodyExpr)
                    let exprWithBindings = wrapBindings valueBindings finalExpr
                    (exprWithBindings, varGen3))
            | Error _ ->
                // Complex expression (like Match) - compile with toANF and transform returns
                let (tempId, varGen1) = ANF.freshVar varGen
                let env' = Map.add name (tempId, valueType) env
                toANF value varGen1 env typeReg variantLookup funcReg moduleRegistry
                |> Result.bind (fun (valueExpr, varGen2) ->
                    toANF body varGen2 env' typeReg variantLookup funcReg moduleRegistry
                    |> Result.map (fun (bodyExpr, varGen3) ->
                        // Transform: replace all Returns in valueExpr with Let bindings to tempId + bodyExpr
                        let rec transformReturns expr =
                            match expr with
                            | ANF.Return atom -> ANF.Let (tempId, ANF.Atom atom, bodyExpr)
                            | ANF.Let (id, cexpr, rest) -> ANF.Let (id, cexpr, transformReturns rest)
                            | ANF.If (cond, thenBr, elseBr) ->
                                ANF.If (cond, transformReturns thenBr, transformReturns elseBr)
                        (transformReturns valueExpr, varGen3))))

    | AST.UnaryOp (AST.Neg, innerExpr) ->
        // Unary negation: use operand type to select float vs integer path
        let typeEnv = typeEnvFromVarEnv env
        inferType innerExpr typeEnv typeReg variantLookup funcReg moduleRegistry
        |> Result.bind (fun innerType ->
            match innerType with
            | AST.TFloat64 ->
                match innerExpr with
                | AST.FloatLiteral f ->
                    // Constant-fold negative float literals at compile time
                    Ok (ANF.Return (ANF.FloatLiteral (-f)), varGen)
                | _ ->
                    toAtom innerExpr varGen env typeReg variantLookup funcReg moduleRegistry
                    |> Result.map (fun (innerAtom, innerBindings, varGen1) ->
                        let (tempVar, varGen2) = ANF.freshVar varGen1
                        let cexpr = ANF.FloatNeg innerAtom
                        let finalExpr = ANF.Let (tempVar, cexpr, ANF.Return (ANF.Var tempVar))
                        let exprWithBindings = wrapBindings innerBindings finalExpr
                        (exprWithBindings, varGen2))
            | AST.TInt64 ->
                match innerExpr with
                | AST.Int64Literal n when n = System.Int64.MinValue ->
                    // The lexer stores INT64_MIN as a sentinel for "9223372036854775808"
                    // When negated, it should remain INT64_MIN (mathematically correct)
                    Ok (ANF.Return (ANF.IntLiteral (ANF.Int64 System.Int64.MinValue)), varGen)
                | _ ->
                    let zeroExpr = AST.Int64Literal 0L
                    toANF (AST.BinOp (AST.Sub, zeroExpr, innerExpr)) varGen env typeReg variantLookup funcReg moduleRegistry
            | AST.TInt32 ->
                let zeroExpr = AST.Int32Literal 0l
                toANF (AST.BinOp (AST.Sub, zeroExpr, innerExpr)) varGen env typeReg variantLookup funcReg moduleRegistry
            | AST.TInt16 ->
                let zeroExpr = AST.Int16Literal 0s
                toANF (AST.BinOp (AST.Sub, zeroExpr, innerExpr)) varGen env typeReg variantLookup funcReg moduleRegistry
            | AST.TInt8 ->
                let zeroExpr = AST.Int8Literal 0y
                toANF (AST.BinOp (AST.Sub, zeroExpr, innerExpr)) varGen env typeReg variantLookup funcReg moduleRegistry
            | _ ->
                Error $"Negation requires numeric operand, got {innerType}")

    | AST.UnaryOp (AST.Not, innerExpr) ->
        // Boolean not: convert operand to atom and apply Not
        toAtom innerExpr varGen env typeReg variantLookup funcReg moduleRegistry |> Result.map (fun (innerAtom, innerBindings, varGen1) ->
            // Create unary op and bind to fresh variable
            let (tempVar, varGen2) = ANF.freshVar varGen1
            let cexpr = ANF.UnaryPrim (ANF.Not, innerAtom)

            // Build the expression: innerBindings + let tempVar = op
            let finalExpr = ANF.Let (tempVar, cexpr, ANF.Return (ANF.Var tempVar))
            let exprWithBindings = wrapBindings innerBindings finalExpr

            (exprWithBindings, varGen2))

    | AST.UnaryOp (AST.BitNot, innerExpr) ->
        // Bitwise NOT: convert operand to atom and apply BitNot
        toAtom innerExpr varGen env typeReg variantLookup funcReg moduleRegistry |> Result.map (fun (innerAtom, innerBindings, varGen1) ->
            // Create unary op and bind to fresh variable
            let (tempVar, varGen2) = ANF.freshVar varGen1
            let cexpr = ANF.UnaryPrim (ANF.BitNot, innerAtom)

            // Build the expression: innerBindings + let tempVar = op
            let finalExpr = ANF.Let (tempVar, cexpr, ANF.Return (ANF.Var tempVar))
            let exprWithBindings = wrapBindings innerBindings finalExpr

            (exprWithBindings, varGen2))

    | AST.BinOp (op, left, right) ->
        toANFBoundAtom left varGen env typeReg variantLookup funcReg moduleRegistry
        |> Result.bind (fun (leftExpr, leftAtom, varGen1) ->
            toANFBoundAtom right varGen1 env typeReg variantLookup funcReg moduleRegistry
            |> Result.bind (fun (rightExpr, rightAtom, varGen2) ->
                let typeEnv = typeEnvFromVarEnv env
                let buildCoreExpr () : Result<ANF.AExpr * ANF.VarGen, string> =
                    match op with
                    | AST.Eq | AST.Neq ->
                        // Infer type of left operand to check if structural comparison is needed
                        match inferType left typeEnv typeReg variantLookup funcReg moduleRegistry with
                        | Ok operandType when isCompoundType operandType ->
                            // Generate structural equality
                            let (eqBindings, eqResultAtom, varGen3) =
                                generateStructuralEquality leftAtom rightAtom operandType varGen2 typeReg variantLookup
                            // For Neq, negate the result
                            let (finalAtom, finalBindings, varGen4) =
                                if op = AST.Neq then
                                    let (negVar, vg) = ANF.freshVar varGen3
                                    let negExpr = ANF.UnaryPrim (ANF.Not, eqResultAtom)
                                    (ANF.Var negVar, eqBindings @ [(negVar, negExpr)], vg)
                                else
                                    (eqResultAtom, eqBindings, varGen3)
                            Ok (wrapBindings finalBindings (ANF.Return finalAtom), varGen4)
                        | Ok AST.TString ->
                            // String equality - call __string_eq
                            let (tempVar, varGen3) = ANF.freshVar varGen2
                            let cexpr = ANF.Call ("__string_eq", [leftAtom; rightAtom])
                            // For Neq, negate the result
                            let (finalAtom, finalBindings, varGen4) =
                                if op = AST.Neq then
                                    let (negVar, vg) = ANF.freshVar varGen3
                                    let negExpr = ANF.UnaryPrim (ANF.Not, ANF.Var tempVar)
                                    (ANF.Var negVar, [(tempVar, cexpr); (negVar, negExpr)], vg)
                                else
                                    (ANF.Var tempVar, [(tempVar, cexpr)], varGen3)
                            Ok (wrapBindings finalBindings (ANF.Return finalAtom), varGen4)
                        | _ ->
                            // Primitive type or type inference failed - use simple comparison
                            let (tempVar, varGen3) = ANF.freshVar varGen2
                            let cexpr = ANF.Prim (convertBinOp op, leftAtom, rightAtom)
                            Ok (ANF.Let (tempVar, cexpr, ANF.Return (ANF.Var tempVar)), varGen3)
                    | AST.StringConcat ->
                        // String concatenation
                        let (tempVar, varGen3) = ANF.freshVar varGen2
                        let cexpr = ANF.StringConcat (leftAtom, rightAtom)
                        Ok (ANF.Let (tempVar, cexpr, ANF.Return (ANF.Var tempVar)), varGen3)
                    // Arithmetic, bitwise, and comparison operators - use simple primitive
                    | AST.Add | AST.Sub | AST.Mul | AST.Div | AST.Mod
                    | AST.Shl | AST.Shr | AST.BitAnd | AST.BitOr | AST.BitXor
                    | AST.Lt | AST.Gt | AST.Lte | AST.Gte
                    | AST.And | AST.Or ->
                        let (tempVar, varGen3) = ANF.freshVar varGen2
                        let cexpr = ANF.Prim (convertBinOp op, leftAtom, rightAtom)
                        Ok (ANF.Let (tempVar, cexpr, ANF.Return (ANF.Var tempVar)), varGen3)

                buildCoreExpr ()
                |> Result.map (fun (coreExpr, varGen3) ->
                    let withRight = bindReturns rightExpr (fun _ -> coreExpr)
                    let withLeft = bindReturns leftExpr (fun _ -> withRight)
                    (withLeft, varGen3))))

    | AST.If (cond, thenBranch, elseBranch) ->
        // If expression: convert condition to atom, both branches to ANF
        // Try toAtom first; if it fails for complex expressions like Match, use toANF
        match toAtom cond varGen env typeReg variantLookup funcReg moduleRegistry with
        | Ok (condAtom, condBindings, varGen1) ->
            toANF thenBranch varGen1 env typeReg variantLookup funcReg moduleRegistry |> Result.bind (fun (thenExpr, varGen2) ->
                toANF elseBranch varGen2 env typeReg variantLookup funcReg moduleRegistry |> Result.map (fun (elseExpr, varGen3) ->
                    // Build the expression: condBindings + if condAtom then thenExpr else elseExpr
                    let finalExpr = ANF.If (condAtom, thenExpr, elseExpr)
                    let exprWithBindings = wrapBindings condBindings finalExpr
                    (exprWithBindings, varGen3)))
        | Error _ ->
            // Complex condition (like Match) - compile with toANF and transform
            // Create: let condTemp = <cond> in if condTemp then <then> else <else>
            let (condTemp, varGen1) = ANF.freshVar varGen
            toANF cond varGen1 env typeReg variantLookup funcReg moduleRegistry
            |> Result.bind (fun (condExpr, varGen2) ->
                toANF thenBranch varGen2 env typeReg variantLookup funcReg moduleRegistry
                |> Result.bind (fun (thenExpr, varGen3) ->
                    toANF elseBranch varGen3 env typeReg variantLookup funcReg moduleRegistry
                    |> Result.map (fun (elseExpr, varGen4) ->
                        // Transform: replace Returns in condExpr with Let + If
                        let ifExpr = ANF.If (ANF.Var condTemp, thenExpr, elseExpr)
                        let rec transformReturns expr =
                            match expr with
                            | ANF.Return atom -> ANF.Let (condTemp, ANF.Atom atom, ifExpr)
                            | ANF.Let (id, cexpr, rest) -> ANF.Let (id, cexpr, transformReturns rest)
                            | ANF.If (c, t, e) -> ANF.If (c, transformReturns t, transformReturns e)
                        (transformReturns condExpr, varGen4))))

    | AST.Call (funcName, args) when isBuiltinUnwrapName funcName ->
        match args with
        | [argExpr] ->
            let typeEnv = typeEnvFromVarEnv env
            inferType argExpr typeEnv typeReg variantLookup funcReg moduleRegistry
            |> Result.bind (fun argType ->
                let lookupVariantInfo (expectedTypeName: string) (variantName: string) : Result<int * AST.Type option, string> =
                    match Map.tryFind variantName variantLookup with
                    | Some (typeName, _, tag, payloadTypeOpt) when typeName = expectedTypeName ->
                        Ok (tag, payloadTypeOpt)
                    | Some (typeName, _, _, _) ->
                        Error $"Builtin.unwrap expected variant {variantName} in {expectedTypeName}, got {typeName}"
                    | None ->
                        Error $"Builtin.unwrap could not find variant tag for {expectedTypeName}.{variantName}"

                let buildUnwrapExpr (successTag: int) (payloadType: AST.Type) (failureMessage: string) : Result<ANF.AExpr * ANF.VarGen, string> =
                    toAtom argExpr varGen env typeReg variantLookup funcReg moduleRegistry
                    |> Result.map (fun (argAtom0, argBindings0, vg1) ->
                        let (argAtom, argBindings, vg2) =
                            match argAtom0 with
                            | ANF.Var _ -> (argAtom0, argBindings0, vg1)
                            | _ ->
                                let (argVar, vg') = ANF.freshVar vg1
                                (ANF.Var argVar, argBindings0 @ [(argVar, ANF.Atom argAtom0)], vg')

                        let (tagVar, vg3) = ANF.freshVar vg2
                        let (isSuccessVar, vg4) = ANF.freshVar vg3
                        let tagBindings = [
                            (tagVar, ANF.TupleGet (argAtom, 0))
                            (isSuccessVar, ANF.Prim (ANF.Eq, ANF.Var tagVar, ANF.IntLiteral (ANF.Int64 (int64 successTag))))
                        ]

                        let normalizedPayloadType =
                            if containsTypeVar payloadType then AST.TUnit else payloadType

                        let (payloadVar, vg5) = ANF.freshVar vg4
                        let (typedPayloadVar, vg6) = ANF.freshVar vg5
                        let thenBranch =
                            ANF.Let (
                                payloadVar,
                                ANF.TupleGet (argAtom, 1),
                                ANF.Let (
                                    typedPayloadVar,
                                    ANF.TypedAtom (ANF.Var payloadVar, normalizedPayloadType),
                                    ANF.Return (ANF.Var typedPayloadVar)
                                )
                            )

                        let (printVar, vg7) = ANF.freshVar vg6
                        let elseBranch =
                            ANF.Let (
                                printVar,
                                ANF.RuntimeError failureMessage,
                                ANF.Return ANF.UnitLiteral
                            )

                        let ifExpr = ANF.If (ANF.Var isSuccessVar, thenBranch, elseBranch)
                        let finalExpr = wrapBindings (argBindings @ tagBindings) ifExpr
                        (finalExpr, vg7))

                match argType with
                | AST.TSum ("Stdlib.Option.Option", [valueType]) ->
                    lookupVariantInfo "Stdlib.Option.Option" "Some"
                    |> Result.bind (fun (successTag, _) ->
                        buildUnwrapExpr successTag valueType "Cannot unwrap None")
                | AST.TSum ("Stdlib.Option.Option", []) ->
                    lookupVariantInfo "Stdlib.Option.Option" "Some"
                    |> Result.bind (fun (successTag, payloadTypeOpt) ->
                        let payloadTypeResult =
                            match argExpr with
                            | AST.Constructor (_, "Some", Some payloadExpr) ->
                                inferType payloadExpr typeEnv typeReg variantLookup funcReg moduleRegistry
                            | _ ->
                                match payloadTypeOpt with
                                | Some payloadType -> Ok payloadType
                                | None -> Ok AST.TUnit
                        payloadTypeResult
                        |> Result.bind (fun payloadType ->
                            buildUnwrapExpr successTag payloadType "Cannot unwrap None"))
                | AST.TSum ("Stdlib.Result.Result", [okType; _]) ->
                    lookupVariantInfo "Stdlib.Result.Result" "Ok"
                    |> Result.bind (fun (successTag, _) ->
                        let failureMessage =
                            match argExpr with
                            | AST.Constructor (_, "Error", Some payloadExpr) ->
                                match unwrapErrorPayloadToString payloadExpr with
                                | Some payloadText -> $"Cannot unwrap Error: {payloadText}"
                                | None -> "Cannot unwrap Error"
                            | _ ->
                                "Cannot unwrap Error"
                        buildUnwrapExpr successTag okType failureMessage)
                | AST.TSum ("Stdlib.Result.Result", []) ->
                    lookupVariantInfo "Stdlib.Result.Result" "Ok"
                    |> Result.bind (fun (successTag, payloadTypeOpt) ->
                        let payloadTypeResult =
                            match argExpr with
                            | AST.Constructor (_, "Ok", Some payloadExpr) ->
                                inferType payloadExpr typeEnv typeReg variantLookup funcReg moduleRegistry
                            | _ ->
                                match payloadTypeOpt with
                                | Some payloadType -> Ok payloadType
                                | None -> Ok AST.TUnit
                        let failureMessage =
                            match argExpr with
                            | AST.Constructor (_, "Error", Some payloadExpr) ->
                                match unwrapErrorPayloadToString payloadExpr with
                                | Some payloadText -> $"Cannot unwrap Error: {payloadText}"
                                | None -> "Cannot unwrap Error"
                            | _ ->
                                "Cannot unwrap Error"
                        payloadTypeResult
                        |> Result.bind (fun payloadType ->
                            buildUnwrapExpr successTag payloadType failureMessage))
                | _ ->
                    Error $"Internal error: Builtin.unwrap should have been typechecked as Option/Result, got {typeToString argType}")
        | _ ->
            Error $"Internal error: Builtin.unwrap should have exactly 1 argument, got {List.length args}"

    | AST.Call (funcName, args) ->
        // Function call: convert all arguments to atoms
        // If an argument is a function reference, wrap it in a trivial closure for uniform calling convention
        let wrapFuncRefInClosure (argExpr: ANF.AExpr) (atom: ANF.Atom) (vg: ANF.VarGen) : ANF.AExpr * ANF.Atom * ANF.VarGen =
            match atom with
            | ANF.FuncRef fnName ->
                // Function reference needs to be wrapped in a closure.
                let (closureId, vg') = ANF.freshVar vg
                let closureExpr = ANF.Let (closureId, ANF.ClosureAlloc (fnName, []), ANF.Return (ANF.Var closureId))
                let wrappedExpr = bindReturns argExpr (fun _ -> closureExpr)
                (wrappedExpr, ANF.Var closureId, vg')
            | _ ->
                (argExpr, atom, vg)

        let rec convertArgs
            (argExprs: AST.Expr list)
            (vg: ANF.VarGen)
            (accExprs: ANF.AExpr list)
            (accAtoms: ANF.Atom list)
            : Result<ANF.AExpr list * ANF.Atom list * ANF.VarGen, string> =
            match argExprs with
            | [] ->
                Ok (List.rev accExprs, List.rev accAtoms, vg)
            | arg :: rest ->
                toANFBoundAtom arg vg env typeReg variantLookup funcReg moduleRegistry
                |> Result.bind (fun (argExpr, argAtom, vg') ->
                    // Wrap function references in closures for uniform calling convention.
                    let (wrappedExpr, wrappedAtom, vg'') = wrapFuncRefInClosure argExpr argAtom vg'
                    convertArgs rest vg'' (wrappedExpr :: accExprs) (wrappedAtom :: accAtoms))

        // Regular function call (including module functions like Stdlib.Int64.add)
        convertArgs args varGen [] []
        |> Result.bind (fun (argSetupExprs, argAtoms, varGen1) ->
            // Bind call result to fresh variable
            let (resultVar, varGen2) = ANF.freshVar varGen1
            let withArgSetups (finalExpr: ANF.AExpr) =
                List.foldBack
                    (fun argExpr acc -> bindReturns argExpr (fun _ -> acc))
                    argSetupExprs
                    finalExpr
            // Check if funcName is a variable (indirect call) or a defined function (direct call)
            match Map.tryFind funcName env with
            | Some (tempId, AST.TFunction (_, _)) ->
                // Variable with function type - use closure call
                // All function values are now closures (even non-capturing ones)
                let callExpr = ANF.ClosureCall (ANF.Var tempId, argAtoms)
                let finalExpr = ANF.Let (resultVar, callExpr, ANF.Return (ANF.Var resultVar))
                Ok (withArgSetups finalExpr, varGen2)
            | Some (_, varType) ->
                // Variable exists but is not a function type
                Error $"Cannot call '{funcName}' - it has type {varType}, not a function type"
            | None ->
                // Not a variable - check if it's a file intrinsic first
                match tryFileIntrinsic funcName argAtoms with
                | Some intrinsicExpr ->
                    // File I/O intrinsic call
                    let finalExpr = ANF.Let (resultVar, intrinsicExpr, ANF.Return (ANF.Var resultVar))
                    Ok (withArgSetups finalExpr, varGen2)
                | None ->
                    // Check if it's a raw memory intrinsic
                    match tryRawMemoryIntrinsic funcName argAtoms with
                    | Some intrinsicExpr ->
                        // Raw memory intrinsic call
                        let finalExpr = ANF.Let (resultVar, intrinsicExpr, ANF.Return (ANF.Var resultVar))
                        Ok (withArgSetups finalExpr, varGen2)
                    | None ->
                    // Check if it's a Float intrinsic
                    match tryFloatIntrinsic funcName argAtoms with
                    | Some intrinsicExpr ->
                        // Float intrinsic call
                        let finalExpr = ANF.Let (resultVar, intrinsicExpr, ANF.Return (ANF.Var resultVar))
                        Ok (withArgSetups finalExpr, varGen2)
                    | None ->
                    // Check if it's a constant-fold intrinsic (Platform, Path)
                    match tryConstantFoldIntrinsic funcName argAtoms with
                    | Some intrinsicExpr ->
                        // Constant-folded intrinsic
                        let finalExpr = ANF.Let (resultVar, intrinsicExpr, ANF.Return (ANF.Var resultVar))
                        Ok (withArgSetups finalExpr, varGen2)
                    | None ->
                    // Check if it's a random intrinsic
                    match tryRandomIntrinsic funcName argAtoms with
                    | Some intrinsicExpr ->
                        // Random intrinsic call
                        let finalExpr = ANF.Let (resultVar, intrinsicExpr, ANF.Return (ANF.Var resultVar))
                        Ok (withArgSetups finalExpr, varGen2)
                    | None ->
                    // Check if it's a date intrinsic
                    match tryDateIntrinsic funcName argAtoms with
                    | Some intrinsicExpr ->
                        // Date intrinsic call
                        let finalExpr = ANF.Let (resultVar, intrinsicExpr, ANF.Return (ANF.Var resultVar))
                        Ok (withArgSetups finalExpr, varGen2)
                    | None ->
                    // Check if it's a defined function
                    match Map.tryFind funcName funcReg with
                    | Some _ ->
                        // Direct call to defined function
                        let callExpr = ANF.Call (funcName, argAtoms)
                        let finalExpr = ANF.Let (resultVar, callExpr, ANF.Return (ANF.Var resultVar))
                        Ok (withArgSetups finalExpr, varGen2)
                    | None ->
                        // Unknown function - could be error or forward reference
                        // For now, assume it's a valid function (will fail at link time if not)
                        let callExpr = ANF.Call (funcName, argAtoms)
                        let finalExpr = ANF.Let (resultVar, callExpr, ANF.Return (ANF.Var resultVar))
                        Ok (withArgSetups finalExpr, varGen2))

    | AST.TypeApp (_funcName, _typeArgs, _args) ->
        // Generic function call - not yet implemented
        Error "Generic function calls not yet implemented"

    | AST.TupleLiteral elements ->
        // Convert all elements to atoms
        let rec convertElements (elems: AST.Expr list) (vg: ANF.VarGen) (accAtoms: ANF.Atom list) (accBindings: (ANF.TempId * ANF.CExpr) list) : Result<ANF.Atom list * (ANF.TempId * ANF.CExpr) list * ANF.VarGen, string> =
            match elems with
            | [] -> Ok (List.rev accAtoms, accBindings, vg)
            | elem :: rest ->
                toAtom elem vg env typeReg variantLookup funcReg moduleRegistry
                |> Result.bind (fun (elemAtom, elemBindings, vg') ->
                    convertElements rest vg' (elemAtom :: accAtoms) (accBindings @ elemBindings))

        convertElements elements varGen [] []
        |> Result.map (fun (elemAtoms, elemBindings, varGen1) ->
            // Create TupleAlloc and bind to fresh variable
            let (resultVar, varGen2) = ANF.freshVar varGen1
            let tupleExpr = ANF.TupleAlloc elemAtoms
            let finalExpr = ANF.Let (resultVar, tupleExpr, ANF.Return (ANF.Var resultVar))
            let exprWithBindings = wrapBindings elemBindings finalExpr

            (exprWithBindings, varGen2))

    | AST.TupleAccess (tupleExpr, index) ->
        // Convert tuple to atom and create TupleGet
        toAtom tupleExpr varGen env typeReg variantLookup funcReg moduleRegistry
        |> Result.map (fun (tupleAtom, tupleBindings, varGen1) ->
            let (resultVar, varGen2) = ANF.freshVar varGen1
            let getExpr = ANF.TupleGet (tupleAtom, index)
            let finalExpr = ANF.Let (resultVar, getExpr, ANF.Return (ANF.Var resultVar))
            let exprWithBindings = wrapBindings tupleBindings finalExpr

            (exprWithBindings, varGen2))

    | AST.RecordLiteral (typeName, fields) ->
        // Records are compiled like tuples - allocate heap space and store fields
        // Get field order from type registry (or use order from literal if anonymous)
        let fieldOrder =
            if typeName = "" then
                fields |> List.map fst  // Use literal order for anonymous records
            else
                match Map.tryFind typeName typeReg with
                | Some typeFields -> typeFields |> List.map fst
                | None -> Crash.crash $"Record type '{typeName}' not found in typeReg"

        // Reorder field values according to type definition order
        let fieldMap = Map.ofList fields
        let orderedValues =
            fieldOrder
            |> List.choose (fun fname -> Map.tryFind fname fieldMap)

        // Convert to TupleLiteral and reuse tuple handling
        toANF (AST.TupleLiteral orderedValues) varGen env typeReg variantLookup funcReg moduleRegistry

    | AST.RecordUpdate (recordExpr, updates) ->
        // Record update: { record with field1 = val1, field2 = val2 }
        // Desugar to creating a new record with updated fields
        let typeEnv = typeEnvFromVarEnv env
        inferType recordExpr typeEnv typeReg variantLookup funcReg moduleRegistry
        |> Result.bind (fun recordType ->
            match recordType with
            | AST.TRecord (typeName, _) ->
                match Map.tryFind typeName typeReg with
                | Some typeFields ->
                    // Build a map of updates
                    let updateMap = Map.ofList updates
                    // For each field in the type, use update value or access from original record
                    let newFields =
                        typeFields
                        |> List.map (fun (fname, _) ->
                            match Map.tryFind fname updateMap with
                            | Some updateExpr -> (fname, updateExpr)
                            | None -> (fname, AST.RecordAccess (recordExpr, fname)))
                    // Create a new record literal with the combined fields
                    toANF (AST.RecordLiteral (typeName, newFields)) varGen env typeReg variantLookup funcReg moduleRegistry
                | None ->
                    Error $"Unknown record type: {typeName}"
            | _ ->
                Error "Cannot use record update syntax on non-record type")

    | AST.RecordAccess (recordExpr, fieldName) ->
        // Records are compiled like tuples - field access becomes TupleGet
        // Use type-directed lookup: infer the record type, then find field index
        let typeEnv = typeEnvFromVarEnv env
        inferType recordExpr typeEnv typeReg variantLookup funcReg moduleRegistry
        |> Result.bind (fun recordType ->
            match recordType with
            | AST.TRecord (typeName, _) ->
                // Look up field index in the specific record type
                match Map.tryFind typeName typeReg with
                | Some fields ->
                    match List.tryFindIndex (fun (name, _) -> name = fieldName) fields with
                    | Some index ->
                        toAtom recordExpr varGen env typeReg variantLookup funcReg moduleRegistry
                        |> Result.map (fun (recordAtom, recordBindings, varGen1) ->
                            let (resultVar, varGen2) = ANF.freshVar varGen1
                            let getExpr = ANF.TupleGet (recordAtom, index)
                            let finalExpr = ANF.Let (resultVar, getExpr, ANF.Return (ANF.Var resultVar))
                            let exprWithBindings = wrapBindings recordBindings finalExpr
                            (exprWithBindings, varGen2))
                    | None ->
                        Error $"Record type '{typeName}' has no field '{fieldName}'"
                | None ->
                    Error $"Unknown record type: {typeName}"
            | _ ->
                Error $"Cannot access field '{fieldName}' on non-record type")

    | AST.Constructor (_, variantName, payload) ->
        match Map.tryFind variantName variantLookup with
        | None ->
            Error $"Unknown constructor: {variantName}"
        | Some (typeName, _, tag, _) ->
            // Check if ANY variant in this type has a payload
            // If so, all variants must be heap-allocated for consistency
            // Note: We get typeName from variantLookup, not from AST (which may be empty)
            let typeHasPayloadVariants =
                variantLookup
                |> Map.exists (fun _ (tName, _, _, pType) -> tName = typeName && pType.IsSome)

            match payload with
            | None when not typeHasPayloadVariants ->
                // Pure enum type (no payloads anywhere): return tag as an integer
                Ok (ANF.Return (ANF.IntLiteral (ANF.Int64 (int64 tag))), varGen)
            | None ->
                // No payload but type has other variants with payloads
                // Heap-allocate as [tag, 0] for uniform 2-element structure
                // This enables consistent structural equality comparison
                let tagAtom = ANF.IntLiteral (ANF.Int64 (int64 tag))
                let dummyPayload = ANF.IntLiteral (ANF.Int64 0L)
                let (resultVar, varGen1) = ANF.freshVar varGen
                let tupleExpr = ANF.TupleAlloc [tagAtom; dummyPayload]
                let finalExpr = ANF.Let (resultVar, tupleExpr, ANF.Return (ANF.Var resultVar))
                Ok (finalExpr, varGen1)
            | Some payloadExpr ->
                // Variant with payload: allocate [tag, payload] on heap
                toAtom payloadExpr varGen env typeReg variantLookup funcReg moduleRegistry
                |> Result.map (fun (payloadAtom, payloadBindings, varGen1) ->
                    let tagAtom = ANF.IntLiteral (ANF.Int64 (int64 tag))
                    // Create TupleAlloc [tag, payload] and bind to fresh variable
                    let (resultVar, varGen2) = ANF.freshVar varGen1
                    let tupleExpr = ANF.TupleAlloc [tagAtom; payloadAtom]
                    let finalExpr = ANF.Let (resultVar, tupleExpr, ANF.Return (ANF.Var resultVar))
                    let exprWithBindings = wrapBindings payloadBindings finalExpr
                    (exprWithBindings, varGen2))

    | AST.ListLiteral elements ->
        // Compile list literal as FingerTree
        // Tags: EMPTY=0, SINGLE=1, DEEP=2, NODE2=3, NODE3=4, LEAF=5
        // DEEP layout: [measure:8][prefixCount:8][p0:8][p1:8][p2:8][p3:8][middle:8][suffixCount:8][s0:8][s1:8][s2:8][s3:8]

        // Increment refcount for heap elements stored in leaves
        let addLeafInc (elemAtom: ANF.Atom) (elemType: AST.Type) (vg: ANF.VarGen) (bindings: (ANF.TempId * ANF.CExpr) list) =
            match elemAtom with
            | ANF.Var _ when ANF.isHeapType elemType ->
                let size = ANF.payloadSize elemType typeReg
                let (incVar, vg1) = ANF.freshVar vg
                let incExpr = ANF.RefCountInc (elemAtom, size)
                (vg1, bindings @ [(incVar, incExpr)])
            | _ ->
                (vg, bindings)

        // Helper to create a LEAF node wrapping an element
        let allocLeaf (elemAtom: ANF.Atom) (elemType: AST.Type) (vg: ANF.VarGen) (bindings: (ANF.TempId * ANF.CExpr) list) =
            let (ptrVar, vg1) = ANF.freshVar vg
            let (setVar, vg2) = ANF.freshVar vg1
            let allocExpr = ANF.RawAlloc (ANF.IntLiteral (ANF.Int64 8L))
            let setExpr = ANF.RawSet (ANF.Var ptrVar, ANF.IntLiteral (ANF.Int64 0L), elemAtom, None)
            let (vg3, bindings3) = addLeafInc elemAtom elemType vg2 (bindings @ [(ptrVar, allocExpr); (setVar, setExpr)])
            let (taggedVar, vg4) = ANF.freshVar vg3
            let tagExpr = ANF.Prim (ANF.BitOr, ANF.Var ptrVar, ANF.IntLiteral (ANF.Int64 5L))  // tag 5 = LEAF
            let newBindings = bindings3 @ [(taggedVar, tagExpr)]
            (ANF.Var taggedVar, newBindings, vg4)

        // Helper to create a SINGLE node containing a TreeNode
        let allocSingle (nodeAtom: ANF.Atom) (vg: ANF.VarGen) (bindings: (ANF.TempId * ANF.CExpr) list) =
            let (ptrVar, vg1) = ANF.freshVar vg
            let (setVar, vg2) = ANF.freshVar vg1
            let (taggedVar, vg3) = ANF.freshVar vg2
            let allocExpr = ANF.RawAlloc (ANF.IntLiteral (ANF.Int64 8L))
            let setExpr = ANF.RawSet (ANF.Var ptrVar, ANF.IntLiteral (ANF.Int64 0L), nodeAtom, None)
            let tagExpr = ANF.Prim (ANF.BitOr, ANF.Var ptrVar, ANF.IntLiteral (ANF.Int64 1L))  // tag 1 = SINGLE
            let newBindings = bindings @ [(ptrVar, allocExpr); (setVar, setExpr); (taggedVar, tagExpr)]
            (ANF.Var taggedVar, newBindings, vg3)

        // Helper to create a DEEP node
        let allocDeep (measure: int) (prefixNodes: ANF.Atom list) (middle: ANF.Atom) (suffixNodes: ANF.Atom list) (vg: ANF.VarGen) (bindings: (ANF.TempId * ANF.CExpr) list) =
            let prefixCount = List.length prefixNodes
            let suffixCount = List.length suffixNodes
            let (ptrVar, vg1) = ANF.freshVar vg
            let allocExpr = ANF.RawAlloc (ANF.IntLiteral (ANF.Int64 96L))  // 12 fields * 8 bytes

            // Build all the set operations
            let setAt offset value vg bindings =
                let (setVar, vg') = ANF.freshVar vg
                let setExpr = ANF.RawSet (ANF.Var ptrVar, ANF.IntLiteral (ANF.Int64 (int64 offset)), value, None)
                (vg', bindings @ [(setVar, setExpr)])

            let (vg2, bindings2) = setAt 0 (ANF.IntLiteral (ANF.Int64 (int64 measure))) vg1 (bindings @ [(ptrVar, allocExpr)])
            let (vg3, bindings3) = setAt 8 (ANF.IntLiteral (ANF.Int64 (int64 prefixCount))) vg2 bindings2

            // Set prefix nodes (p0-p3 at offsets 16, 24, 32, 40)
            let rec setPrefix nodes offset vg bindings =
                match nodes with
                | [] -> (vg, bindings)
                | n :: rest ->
                    let (vg', bindings') = setAt offset n vg bindings
                    setPrefix rest (offset + 8) vg' bindings'
            let (vg4, bindings4) = setPrefix prefixNodes 16 vg3 bindings3

            // Set middle at offset 48 (type-uniform: another FingerTree of nodes)
            let (vg5, bindings5) = setAt 48 middle vg4 bindings4

            // Set suffix count at offset 56
            let (vg6, bindings6) = setAt 56 (ANF.IntLiteral (ANF.Int64 (int64 suffixCount))) vg5 bindings5

            // Set suffix nodes (s0-s3 at offsets 64, 72, 80, 88)
            let (vg7, bindings7) = setPrefix suffixNodes 64 vg6 bindings6

            // Tag with DEEP (2)
            let (taggedVar, vg8) = ANF.freshVar vg7
            let tagExpr = ANF.Prim (ANF.BitOr, ANF.Var ptrVar, ANF.IntLiteral (ANF.Int64 2L))
            (ANF.Var taggedVar, bindings7 @ [(taggedVar, tagExpr)], vg8)

        // Build FingerTree nodes for middle spines without using pushBack.
        let emptyTree = ANF.IntLiteral (ANF.Int64 0L)

        let nodeAtom (node: ANF.Atom, _measure: int) = node
        let nodeMeasure (_node: ANF.Atom, measure: int) = measure

        // Helper to create a NODE2 (tag 3): [child0:8][child1:8][measure:8]
        let allocNode2 (left: ANF.Atom * int) (right: ANF.Atom * int) (vg: ANF.VarGen) (bindings: (ANF.TempId * ANF.CExpr) list) =
            let (ptrVar, vg1) = ANF.freshVar vg
            let allocExpr = ANF.RawAlloc (ANF.IntLiteral (ANF.Int64 24L))
            let (set0Var, vg2) = ANF.freshVar vg1
            let set0Expr = ANF.RawSet (ANF.Var ptrVar, ANF.IntLiteral (ANF.Int64 0L), nodeAtom left, None)
            let (set1Var, vg3) = ANF.freshVar vg2
            let set1Expr = ANF.RawSet (ANF.Var ptrVar, ANF.IntLiteral (ANF.Int64 8L), nodeAtom right, None)
            let measure = nodeMeasure left + nodeMeasure right
            let (set2Var, vg4) = ANF.freshVar vg3
            let set2Expr = ANF.RawSet (ANF.Var ptrVar, ANF.IntLiteral (ANF.Int64 16L), ANF.IntLiteral (ANF.Int64 (int64 measure)), None)
            let (taggedVar, vg5) = ANF.freshVar vg4
            let tagExpr = ANF.Prim (ANF.BitOr, ANF.Var ptrVar, ANF.IntLiteral (ANF.Int64 3L))  // tag 3 = NODE2
            let newBindings = bindings @ [(ptrVar, allocExpr); (set0Var, set0Expr); (set1Var, set1Expr); (set2Var, set2Expr); (taggedVar, tagExpr)]
            ((ANF.Var taggedVar, measure), newBindings, vg5)

        // Helper to create a NODE3 (tag 4): [child0:8][child1:8][child2:8][measure:8]
        let allocNode3 (first: ANF.Atom * int) (second: ANF.Atom * int) (third: ANF.Atom * int) (vg: ANF.VarGen) (bindings: (ANF.TempId * ANF.CExpr) list) =
            let (ptrVar, vg1) = ANF.freshVar vg
            let allocExpr = ANF.RawAlloc (ANF.IntLiteral (ANF.Int64 32L))
            let (set0Var, vg2) = ANF.freshVar vg1
            let set0Expr = ANF.RawSet (ANF.Var ptrVar, ANF.IntLiteral (ANF.Int64 0L), nodeAtom first, None)
            let (set1Var, vg3) = ANF.freshVar vg2
            let set1Expr = ANF.RawSet (ANF.Var ptrVar, ANF.IntLiteral (ANF.Int64 8L), nodeAtom second, None)
            let (set2Var, vg4) = ANF.freshVar vg3
            let set2Expr = ANF.RawSet (ANF.Var ptrVar, ANF.IntLiteral (ANF.Int64 16L), nodeAtom third, None)
            let measure = nodeMeasure first + nodeMeasure second + nodeMeasure third
            let (set3Var, vg5) = ANF.freshVar vg4
            let set3Expr = ANF.RawSet (ANF.Var ptrVar, ANF.IntLiteral (ANF.Int64 24L), ANF.IntLiteral (ANF.Int64 (int64 measure)), None)
            let (taggedVar, vg6) = ANF.freshVar vg5
            let tagExpr = ANF.Prim (ANF.BitOr, ANF.Var ptrVar, ANF.IntLiteral (ANF.Int64 4L))  // tag 4 = NODE3
            let newBindings =
                bindings @ [(ptrVar, allocExpr); (set0Var, set0Expr); (set1Var, set1Expr); (set2Var, set2Expr); (set3Var, set3Expr); (taggedVar, tagExpr)]
            ((ANF.Var taggedVar, measure), newBindings, vg6)

        let splitAt count nodes =
            let rec loop remaining acc rest =
                match remaining, rest with
                | 0, _ -> Ok (List.rev acc, rest)
                | _, [] -> Error "List literal: not enough nodes for split"
                | n, x :: xs -> loop (n - 1) (x :: acc) xs
            loop count [] nodes

        let groupSizes nodeCount =
            if nodeCount < 2 then
                Error "List literal: middle spine needs at least 2 nodes"
            else
                match nodeCount % 3 with
                | 0 -> Ok (List.replicate (nodeCount / 3) 3)
                | 1 ->
                    if nodeCount < 4 then
                        Error "List literal: invalid middle spine size"
                    else
                        Ok (2 :: 2 :: List.replicate ((nodeCount - 4) / 3) 3)
                | _ ->
                    Ok (2 :: List.replicate ((nodeCount - 2) / 3) 3)

        let rec buildGroupedNodes sizes nodes vg bindings acc =
            match sizes with
            | [] -> Ok (List.rev acc, bindings, vg)
            | size :: rest ->
                splitAt size nodes
                |> Result.bind (fun (group, remaining) ->
                    match size, group with
                    | 2, [a; b] ->
                        let (nodeInfo, bindings1, vg1) = allocNode2 a b vg bindings
                        buildGroupedNodes rest remaining vg1 bindings1 (nodeInfo :: acc)
                    | 3, [a; b; c] ->
                        let (nodeInfo, bindings1, vg1) = allocNode3 a b c vg bindings
                        buildGroupedNodes rest remaining vg1 bindings1 (nodeInfo :: acc)
                    | _ ->
                        Error $"List literal: unexpected group size {size}")

        let rec buildTree (nodes: (ANF.Atom * int) list) (vg: ANF.VarGen) (bindings: (ANF.TempId * ANF.CExpr) list) =
            let nodeCount = List.length nodes
            match nodes with
            | [] -> Ok (emptyTree, bindings, vg)
            | [single] ->
                let (resultAtom, resultBindings, vg1) = allocSingle (nodeAtom single) vg bindings
                Ok (resultAtom, resultBindings, vg1)
            | first :: rest when nodeCount <= 5 ->
                let totalMeasure = nodes |> List.sumBy nodeMeasure
                let prefixNodes = [nodeAtom first]
                let suffixNodes = rest |> List.map nodeAtom
                let (resultAtom, resultBindings, vg1) = allocDeep totalMeasure prefixNodes emptyTree suffixNodes vg bindings
                Ok (resultAtom, resultBindings, vg1)
            | _ ->
                splitAt 2 nodes
                |> Result.bind (fun (prefixNodes, rest) ->
                    let restLength = List.length rest
                    let middleCount = restLength - 2
                    splitAt middleCount rest
                    |> Result.bind (fun (middleNodes, suffixNodes) ->
                        groupSizes (List.length middleNodes)
                        |> Result.bind (fun sizes ->
                            buildGroupedNodes sizes middleNodes vg bindings []
                            |> Result.bind (fun (groupedMiddle, bindings1, vg1) ->
                                buildTree groupedMiddle vg1 bindings1
                                |> Result.map (fun (middleTree, bindings2, vg2) ->
                                    let totalMeasure = nodes |> List.sumBy nodeMeasure
                                    let prefixAtoms = prefixNodes |> List.map nodeAtom
                                    let suffixAtoms = suffixNodes |> List.map nodeAtom
                                    let (resultAtom, resultBindings, vg3) =
                                        allocDeep totalMeasure prefixAtoms middleTree suffixAtoms vg2 bindings2
                                    (resultAtom, resultBindings, vg3))))))

        if List.isEmpty elements then
            // Empty list is EMPTY (represented as 0)
            Ok (ANF.Return (ANF.IntLiteral (ANF.Int64 0L)), varGen)
        else
            let typeEnv = typeEnvFromVarEnv env

            // Convert all elements to atoms first
            let rec convertElements (elems: AST.Expr list) (vg: ANF.VarGen) (acc: (ANF.Atom * AST.Type * (ANF.TempId * ANF.CExpr) list) list) =
                match elems with
                | [] -> Ok (List.rev acc, vg)
                | e :: rest ->
                    inferType e typeEnv typeReg variantLookup funcReg moduleRegistry
                    |> Result.bind (fun elemType ->
                        toAtom e vg env typeReg variantLookup funcReg moduleRegistry
                        |> Result.bind (fun (atom, bindings, vg') ->
                            convertElements rest vg' ((atom, elemType, bindings) :: acc)))

            convertElements elements varGen []
            |> Result.bind (fun (atomsWithBindings, varGen1) ->
                // Flatten all element bindings
                let elemBindings = atomsWithBindings |> List.collect (fun (_, _, bindings) -> bindings)
                let elemAtoms = atomsWithBindings |> List.map (fun (atom, elemType, _) -> (atom, elemType))

                // Create LEAF nodes for all elements
                let rec createLeaves (atoms: (ANF.Atom * AST.Type) list) (vg: ANF.VarGen) (bindings: (ANF.TempId * ANF.CExpr) list) (acc: ANF.Atom list) =
                    match atoms with
                    | [] -> (List.rev acc, bindings, vg)
                    | (a, elemType) :: rest ->
                        let (leafAtom, bindings', vg') = allocLeaf a elemType vg bindings
                        createLeaves rest vg' bindings' (leafAtom :: acc)

                let (leafAtoms, leafBindings, varGen2) = createLeaves elemAtoms varGen1 elemBindings []
                let leafNodes = leafAtoms |> List.map (fun atom -> (atom, 1))

                buildTree leafNodes varGen2 leafBindings
                |> Result.map (fun (resultAtom, resultBindings, varGen3) ->
                    let finalExpr = ANF.Return resultAtom
                    let exprWithBindings = wrapBindings resultBindings finalExpr
                    (exprWithBindings, varGen3)))

    | AST.ListCons (headElements, tail) ->
        // Compile list cons: [a, b, ...tail] prepends elements to tail
        // Use Stdlib.__FingerTree.push to prepend each element
        toAtom tail varGen env typeReg variantLookup funcReg moduleRegistry
        |> Result.bind (fun (tailAtom, tailBindings, varGen1) ->
            // Build list by prepending elements from right to left
            // [a, b, ...tail] means push(push(tail, b), a)
            let rec buildList (elems: AST.Expr list) (vg: ANF.VarGen) (currentList: ANF.Atom) (allBindings: (ANF.TempId * ANF.CExpr) list) : Result<ANF.Atom * (ANF.TempId * ANF.CExpr) list * ANF.VarGen, string> =
                match elems with
                | [] -> Ok (currentList, allBindings, vg)
                | elem :: rest ->
                    // First build the rest of the list, then prepend this element
                    buildList rest vg currentList allBindings
                    |> Result.bind (fun (restList, restBindings, vg1) ->
                        toAtom elem vg1 env typeReg variantLookup funcReg moduleRegistry
                        |> Result.map (fun (elemAtom, elemBindings, vg2) ->
                            let (pushVar, vg3) = ANF.freshVar vg2
                            // Call Stdlib.__FingerTree.push to prepend element
                            let pushExpr = ANF.Call ("Stdlib.__FingerTree.push_i64", [restList; elemAtom])
                            let newBindings = restBindings @ elemBindings @ [(pushVar, pushExpr)]
                            (ANF.Var pushVar, newBindings, vg3)))

            if List.isEmpty headElements then
                // No head elements, just return tail
                let finalExpr = ANF.Return tailAtom
                let exprWithBindings = wrapBindings tailBindings finalExpr
                Ok (exprWithBindings, varGen1)
            else
                // Build the list by pushing elements
                buildList headElements varGen1 tailAtom tailBindings
                |> Result.map (fun (listAtom, listBindings, varGen2) ->
                    let finalExpr = ANF.Return listAtom
                    let exprWithBindings = wrapBindings listBindings finalExpr
                    (exprWithBindings, varGen2)))

    | AST.Match (scrutinee, cases) ->
        // Infer scrutinee type to pass to pattern extraction for correct typing
        let typeEnv = typeEnvFromVarEnv env
        match inferType scrutinee typeEnv typeReg variantLookup funcReg moduleRegistry with
        | Error msg -> Error $"Match scrutinee type inference failed: {msg}"
        | Ok scrutType ->
        // Compile match to if-else chain
        // First convert scrutinee to atom
        toAtom scrutinee varGen env typeReg variantLookup funcReg moduleRegistry
        |> Result.bind (fun (scrutineeAtom, scrutineeBindings, varGen1) ->
            // Check if any pattern needs to access list structure
            // If so, we must ensure scrutinee is a variable (can't TupleGet on literal)
            let hasNonEmptyListPattern =
                cases |> List.exists (fun mc ->
                    mc.Patterns |> AST.NonEmptyList.toList |> List.exists (fun pat ->
                        match pat with
                        | AST.PList (_ :: _) -> true
                        | AST.PListCons (_ :: _, _) -> true  // [h, ...t] also needs list access
                        | _ -> false))

            // If there are non-empty list patterns, bind the scrutinee to a variable
            let (scrutineeAtom', scrutineeBindings', varGen1') =
                match scrutineeAtom with
                | ANF.Var _ -> (scrutineeAtom, scrutineeBindings, varGen1)
                | _ when hasNonEmptyListPattern ->
                    let (tempVar, vg) = ANF.freshVar varGen1
                    (ANF.Var tempVar, scrutineeBindings @ [(tempVar, ANF.Atom scrutineeAtom)], vg)
                | _ -> (scrutineeAtom, scrutineeBindings, varGen1)

            // Check if the TYPE that a variant belongs to has any variant with a payload
            // This determines if values are heap-allocated or simple integers
            let typeHasAnyPayload (variantName: string) : bool =
                match Map.tryFind variantName variantLookup with
                | Some (typeName, _, _, _) ->
                    variantLookup
                    |> Map.exists (fun _ (tName, _, _, pType) -> tName = typeName && pType.IsSome)
                | None -> false

            // Check if pattern always matches (wildcard or variable)
            let rec patternAlwaysMatches (pattern: AST.Pattern) : bool =
                match pattern with
                | AST.PUnit -> true
                | AST.PWildcard -> true
                | AST.PVar _ -> true
                | _ -> false

            // Extract pattern bindings and compile body with extended environment
            // scrutType is the type of the scrutinee, used to determine correct types for pattern variables
            let rec extractAndCompileBody (pattern: AST.Pattern) (body: AST.Expr) (scrutAtom: ANF.Atom) (scrutType: AST.Type) (currentEnv: VarEnv) (vg: ANF.VarGen) : Result<ANF.AExpr * ANF.VarGen, string> =
                match pattern with
                | AST.PUnit -> toANF body vg currentEnv typeReg variantLookup funcReg moduleRegistry
                | AST.PWildcard -> toANF body vg currentEnv typeReg variantLookup funcReg moduleRegistry
                | AST.PInt64 _
                | AST.PInt8Literal _
                | AST.PInt16Literal _
                | AST.PInt32Literal _
                | AST.PUInt8Literal _
                | AST.PUInt16Literal _
                | AST.PUInt32Literal _
                | AST.PUInt64Literal _ ->
                    toANF body vg currentEnv typeReg variantLookup funcReg moduleRegistry
                | AST.PBool _ -> toANF body vg currentEnv typeReg variantLookup funcReg moduleRegistry
                | AST.PString _ -> toANF body vg currentEnv typeReg variantLookup funcReg moduleRegistry
                | AST.PFloat _ -> toANF body vg currentEnv typeReg variantLookup funcReg moduleRegistry
                | AST.PVar name ->
                    // Bind scrutinee to variable name with the correct type
                    let (tempId, vg1) = ANF.freshVar vg
                    let env' = Map.add name (tempId, scrutType) currentEnv
                    toANF body vg1 env' typeReg variantLookup funcReg moduleRegistry
                    |> Result.map (fun (bodyExpr, vg2) ->
                        let expr = ANF.Let (tempId, ANF.Atom scrutAtom, bodyExpr)
                        (expr, vg2))
                | AST.PConstructor (constructorName, payloadPattern) ->
                    match payloadPattern with
                    | None -> toANF body vg currentEnv typeReg variantLookup funcReg moduleRegistry
                    | Some innerPattern ->
                        // Extract payload from heap-allocated variant
                        // Variant layout: [tag:8][payload:8], so payload is at index 1
                        let (payloadVar, vg1) = ANF.freshVar vg
                        let (typedPayloadVar, vg2) = ANF.freshVar vg1
                        let payloadExpr = ANF.TupleGet (scrutAtom, 1)
                        // Get payload type from variant lookup if available
                        let payloadTypeResult =
                            match Map.tryFind constructorName variantLookup with
                            | Some (_, typeParams, _, Some payloadTypeTemplate) ->
                                // Apply type substitution if scrutType has type args
                                let payloadType =
                                    match scrutType with
                                    | AST.TSum (_, typeArgs) when List.length typeParams = List.length typeArgs ->
                                        let subst = List.zip typeParams typeArgs |> Map.ofList
                                        let rec substitute t =
                                            match t with
                                            | AST.TVar name -> Map.tryFind name subst |> Option.defaultValue t
                                            | AST.TTuple elems -> AST.TTuple (List.map substitute elems)
                                            | AST.TList elem -> AST.TList (substitute elem)
                                            | AST.TDict (k, v) -> AST.TDict (substitute k, substitute v)
                                            | AST.TSum (name, args) -> AST.TSum (name, List.map substitute args)
                                            | AST.TFunction (args, ret) -> AST.TFunction (List.map substitute args, substitute ret)
                                            | _ -> t
                                        substitute payloadTypeTemplate
                                    | _ -> payloadTypeTemplate
                                Ok payloadType
                            | Some (_, _, _, None) ->
                                Error $"Constructor '{constructorName}' has no payload type in variant lookup"
                            | None ->
                                Error $"Constructor '{constructorName}' not found in variant lookup"
                        // Now compile the inner pattern with the payload
                        payloadTypeResult
                        |> Result.bind (fun payloadType ->
                            let typedPayloadExpr = ANF.TypedAtom (ANF.Var payloadVar, payloadType)
                            extractAndCompileBody innerPattern body (ANF.Var typedPayloadVar) payloadType currentEnv vg2
                            |> Result.map (fun (innerExpr, vg3) ->
                                let expr = ANF.Let (payloadVar, payloadExpr, ANF.Let (typedPayloadVar, typedPayloadExpr, innerExpr))
                                (expr, vg3)))
                | AST.PTuple patterns ->
                    // Recursively collect all variable bindings from a pattern
                    // Returns: updated env, list of bindings, updated vargen
                    // sourceType is the type of the source being matched, used to get correct element types
                    let rec collectPatternBindings (pat: AST.Pattern) (sourceAtom: ANF.Atom) (sourceType: AST.Type) (env: VarEnv) (bindings: (ANF.TempId * ANF.CExpr) list) (vg: ANF.VarGen) : Result<VarEnv * (ANF.TempId * ANF.CExpr) list * ANF.VarGen, string> =
                        match pat with
                        | AST.PInt64 _
                        | AST.PInt8Literal _
                        | AST.PInt16Literal _
                        | AST.PInt32Literal _
                        | AST.PUInt8Literal _
                        | AST.PUInt16Literal _
                        | AST.PUInt32Literal _
                        | AST.PUInt64Literal _
                        | AST.PUnit
                        | AST.PWildcard
                        | AST.PBool _
                        | AST.PString _
                        | AST.PFloat _ ->
                            // No variable bindings
                            Ok (env, bindings, vg)
                        | AST.PVar name ->
                            // Bind the source to a variable with the correct type
                            // Use TypedAtom to preserve the semantic type (e.g., tuple element type)
                            // even when the source comes from a function with generic return type
                            let (tempId, vg1) = ANF.freshVar vg
                            let binding = (tempId, ANF.TypedAtom (sourceAtom, sourceType))
                            let newEnv = Map.add name (tempId, sourceType) env
                            Ok (newEnv, binding :: bindings, vg1)
                        | AST.PTuple innerPatterns ->
                            let unknownElemTypes =
                                innerPatterns
                                |> List.mapi (fun idx _ -> AST.TVar $"__tuple_elem_{idx}")

                            // Extract each element and recursively collect bindings
                            let rec collectFromTuple (pats: AST.Pattern list) (types: AST.Type list) (idx: int) (env: VarEnv) (bindings: (ANF.TempId * ANF.CExpr) list) (vg: ANF.VarGen) =
                                match pats, types with
                                | [], _ -> Ok (env, bindings, vg)
                                | p :: rest, t :: restTypes ->
                                    // Extract raw element with TupleGet
                                    let (rawElemVar, vg1) = ANF.freshVar vg
                                    let rawElemExpr = ANF.TupleGet (sourceAtom, idx)
                                    let rawElemBinding = (rawElemVar, rawElemExpr)
                                    // Wrap with TypedAtom to preserve correct element type in TypeMap
                                    let (elemVar, vg1') = ANF.freshVar vg1
                                    let elemExpr = ANF.TypedAtom (ANF.Var rawElemVar, t)
                                    let elemBinding = (elemVar, elemExpr)
                                    // Recursively collect bindings from this element's pattern with correct type
                                    collectPatternBindings p (ANF.Var elemVar) t env (elemBinding :: rawElemBinding :: bindings) vg1'
                                    |> Result.bind (fun (env', bindings', vg') ->
                                        collectFromTuple rest restTypes (idx + 1) env' bindings' vg')
                                | _ ->
                                    Error "Tuple pattern element/type mismatch"

                            let elemTypes =
                                match sourceType with
                                | AST.TTuple types when List.length types = List.length innerPatterns -> types
                                | _ -> unknownElemTypes

                            collectFromTuple innerPatterns elemTypes 0 env bindings vg
                        | AST.PConstructor (constructorName, payloadPattern) ->
                            let rec substituteType (subst: Map<string, AST.Type>) (typ: AST.Type) : AST.Type =
                                match typ with
                                | AST.TVar name -> Map.tryFind name subst |> Option.defaultValue typ
                                | AST.TTuple elems -> AST.TTuple (List.map (substituteType subst) elems)
                                | AST.TRecord (name, args) -> AST.TRecord (name, List.map (substituteType subst) args)
                                | AST.TList elem -> AST.TList (substituteType subst elem)
                                | AST.TDict (k, v) -> AST.TDict (substituteType subst k, substituteType subst v)
                                | AST.TSum (name, args) -> AST.TSum (name, List.map (substituteType subst) args)
                                | AST.TFunction (args, ret) -> AST.TFunction (List.map (substituteType subst) args, substituteType subst ret)
                                | _ -> typ

                            let resolvePayloadType (constructorName: string) (scrutineeType: AST.Type) : Result<AST.Type, string> =
                                match Map.tryFind constructorName variantLookup with
                                | Some (_, typeParams, _, Some payloadTypeTemplate) ->
                                    let payloadType =
                                        match scrutineeType with
                                        | AST.TSum (_, typeArgs) when List.length typeParams = List.length typeArgs ->
                                            let subst = List.zip typeParams typeArgs |> Map.ofList
                                            substituteType subst payloadTypeTemplate
                                        | _ -> payloadTypeTemplate
                                    Ok payloadType
                                | Some (_, _, _, None) ->
                                    Error $"Constructor '{constructorName}' has no payload type"
                                | None ->
                                    Error $"Unknown constructor '{constructorName}' in pattern"

                            match payloadPattern with
                            | None -> Ok (env, bindings, vg)
                            | Some innerPat ->
                                // Extract payload (at index 1) and recursively collect
                                let (payloadVar, vg1) = ANF.freshVar vg
                                let payloadExpr = ANF.TupleGet (sourceAtom, 1)
                                let payloadBinding = (payloadVar, payloadExpr)
                                resolvePayloadType constructorName sourceType
                                |> Result.bind (fun payloadType ->
                                    collectPatternBindings innerPat (ANF.Var payloadVar) payloadType env (payloadBinding :: bindings) vg1)
                        | AST.PRecord (_, fieldPatterns) ->
                            // Extract field types from record type (look up in type registry)
                            let fieldTypes =
                                match sourceType with
                                | AST.TRecord (recordName, _) ->
                                    match Map.tryFind recordName typeReg with
                                    | Some fields -> fields |> List.map snd
                                    | None -> List.replicate (List.length fieldPatterns) AST.TInt64
                                | _ -> List.replicate (List.length fieldPatterns) AST.TInt64
                            // Extract each field and recursively collect bindings
                            let rec collectFromRecord (fields: (string * AST.Pattern) list) (types: AST.Type list) (idx: int) (env: VarEnv) (bindings: (ANF.TempId * ANF.CExpr) list) (vg: ANF.VarGen) =
                                match fields, types with
                                | [], _ -> Ok (env, bindings, vg)
                                | (_, p) :: rest, t :: restTypes ->
                                    let (fieldVar, vg1) = ANF.freshVar vg
                                    let fieldExpr = ANF.TupleGet (sourceAtom, idx)
                                    let fieldBinding = (fieldVar, fieldExpr)
                                    collectPatternBindings p (ANF.Var fieldVar) t env (fieldBinding :: bindings) vg1
                                    |> Result.bind (fun (env', bindings', vg') ->
                                        collectFromRecord rest restTypes (idx + 1) env' bindings' vg')
                                | (_, p) :: rest, [] ->
                                    let (fieldVar, vg1) = ANF.freshVar vg
                                    let fieldExpr = ANF.TupleGet (sourceAtom, idx)
                                    let fieldBinding = (fieldVar, fieldExpr)
                                    collectPatternBindings p (ANF.Var fieldVar) AST.TInt64 env (fieldBinding :: bindings) vg1
                                    |> Result.bind (fun (env', bindings', vg') ->
                                        collectFromRecord rest [] (idx + 1) env' bindings' vg')
                            collectFromRecord fieldPatterns fieldTypes 0 env bindings vg
                        | AST.PList innerPatterns ->
                            // Extract element type from list type
                            let elemType =
                                match sourceType with
                                | AST.TList t -> t
                                | _ -> AST.TInt64
                            // For list patterns, extract head elements using FingerTree operations
                            // Use _i64 versions which work for any element type at runtime (all values are 64-bit)
                            // The correct element type is tracked in the VarEnv/TypeMap, not in the function name
                            let rec collectFromList (pats: AST.Pattern list) (currentList: ANF.Atom) (env: VarEnv) (bindings: (ANF.TempId * ANF.CExpr) list) (vg: ANF.VarGen) =
                                match pats with
                                | [] -> Ok (env, bindings, vg)
                                | p :: rest ->
                                    // Lists are FingerTrees - use headUnsafe/tail to extract
                                    let (headVar, vg1) = ANF.freshVar vg
                                    let headExpr = ANF.Call ("Stdlib.__FingerTree.headUnsafe_i64", [currentList])
                                    let headBinding = (headVar, headExpr)
                                    collectPatternBindings p (ANF.Var headVar) elemType env (headBinding :: bindings) vg1
                                    |> Result.bind (fun (env', bindings', vg') ->
                                        if List.isEmpty rest then
                                            Ok (env', bindings', vg')
                                        else
                                            // Get tail for next iteration
                                            let (tailVar, vg2) = ANF.freshVar vg'
                                            let tailExpr = ANF.Call ("Stdlib.__FingerTree.tail_i64", [currentList])
                                            let tailBinding = (tailVar, tailExpr)
                                            collectFromList rest (ANF.Var tailVar) env' (tailBinding :: bindings') vg2)
                            collectFromList innerPatterns sourceAtom env bindings vg
                        | AST.PListCons (headPatterns, tailPattern) ->
                            // Extract element type from list type
                            let elemType =
                                match sourceType with
                                | AST.TList t -> t
                                | _ -> AST.TInt64
                            // Extract head elements then bind tail using FingerTree operations
                            // Use _i64 versions which work for any element type at runtime (all values are 64-bit)
                            // The correct element type is tracked in the VarEnv/TypeMap, not in the function name
                            let rec collectHeads (pats: AST.Pattern list) (currentList: ANF.Atom) (env: VarEnv) (bindings: (ANF.TempId * ANF.CExpr) list) (vg: ANF.VarGen) =
                                match pats with
                                | [] ->
                                    // Bind the remaining list to tail pattern (tail has same type as source)
                                    collectPatternBindings tailPattern currentList sourceType env bindings vg
                                | p :: rest ->
                                    // Lists are FingerTrees - use headUnsafe/tail to extract
                                    let (rawHeadVar, vg1) = ANF.freshVar vg
                                    let rawHeadExpr = ANF.Call ("Stdlib.__FingerTree.headUnsafe_i64", [currentList])
                                    let rawHeadBinding = (rawHeadVar, rawHeadExpr)
                                    // Wrap with TypedAtom to preserve correct element type in TypeMap
                                    let (headVar, vg1') = ANF.freshVar vg1
                                    let headExpr = ANF.TypedAtom (ANF.Var rawHeadVar, elemType)
                                    let headBinding = (headVar, headExpr)
                                    collectPatternBindings p (ANF.Var headVar) elemType env (headBinding :: rawHeadBinding :: bindings) vg1'
                                    |> Result.bind (fun (env', bindings', vg') ->
                                        let (rawTailVar, vg2) = ANF.freshVar vg'
                                        let rawTailExpr = ANF.Call ("Stdlib.__FingerTree.tail_i64", [currentList])
                                        let rawTailBinding = (rawTailVar, rawTailExpr)
                                        // Wrap tail with TypedAtom to preserve list type
                                        let (tailVar, vg2') = ANF.freshVar vg2
                                        let tailExpr = ANF.TypedAtom (ANF.Var rawTailVar, sourceType)
                                        let tailBinding = (tailVar, tailExpr)
                                        collectHeads rest (ANF.Var tailVar) env' (tailBinding :: rawTailBinding :: bindings') vg2')
                            collectHeads headPatterns sourceAtom env bindings vg

                    // Collect all bindings from the tuple pattern, then compile body
                    collectPatternBindings (AST.PTuple patterns) scrutAtom scrutType currentEnv [] vg
                    |> Result.bind (fun (newEnv, bindings, vg1) ->
                        toANF body vg1 newEnv typeReg variantLookup funcReg moduleRegistry
                        |> Result.map (fun (bodyExpr, vg2) ->
                            let finalExpr = wrapBindings (List.rev bindings) bodyExpr
                            (finalExpr, vg2)))
                | AST.PRecord (_, fieldPatterns) ->
                    // Extract field types from record type
                    let fieldTypes =
                        match scrutType with
                        | AST.TRecord (recordName, _) ->
                            match Map.tryFind recordName typeReg with
                            | Some fields -> fields |> List.map snd
                            | None -> List.replicate (List.length fieldPatterns) AST.TInt64
                        | _ -> List.replicate (List.length fieldPatterns) AST.TInt64
                    // Extract each field and bind pattern variables
                    let rec collectRecordBindings (fields: (string * AST.Pattern) list) (types: AST.Type list) (env: VarEnv) (bindings: (ANF.TempId * ANF.CExpr) list) (vg: ANF.VarGen) (fieldIdx: int) : Result<VarEnv * (ANF.TempId * ANF.CExpr) list * ANF.VarGen, string> =
                        match fields, types with
                        | [], _ -> Ok (env, List.rev bindings, vg)
                        | (_, pat) :: rest, t :: restTypes ->
                            let (fieldVar, vg1) = ANF.freshVar vg
                            let fieldExpr = ANF.TupleGet (scrutAtom, fieldIdx)
                            let binding = (fieldVar, fieldExpr)
                            match pat with
                            | AST.PVar name ->
                                // Use the correct field type
                                let newEnv = Map.add name (fieldVar, t) env
                                collectRecordBindings rest restTypes newEnv (binding :: bindings) vg1 (fieldIdx + 1)
                            | AST.PWildcard ->
                                collectRecordBindings rest restTypes env bindings vg1 (fieldIdx + 1)
                            | AST.PInt64 _
                            | AST.PInt8Literal _
                            | AST.PInt16Literal _
                            | AST.PInt32Literal _
                            | AST.PUInt8Literal _
                            | AST.PUInt16Literal _
                            | AST.PUInt32Literal _
                            | AST.PUInt64Literal _
                            | AST.PUnit
                            | AST.PConstructor _
                            | AST.PBool _
                            | AST.PString _ | AST.PFloat _ | AST.PTuple _ | AST.PRecord _
                            | AST.PList _ | AST.PListCons _ ->
                                Error $"Nested pattern in record field not yet supported: {pat}"
                        | (_, pat) :: rest, [] ->
                            // Fallback
                            let (fieldVar, vg1) = ANF.freshVar vg
                            let fieldExpr = ANF.TupleGet (scrutAtom, fieldIdx)
                            let binding = (fieldVar, fieldExpr)
                            match pat with
                            | AST.PVar name ->
                                let newEnv = Map.add name (fieldVar, AST.TInt64) env
                                collectRecordBindings rest [] newEnv (binding :: bindings) vg1 (fieldIdx + 1)
                            | AST.PWildcard ->
                                collectRecordBindings rest [] env bindings vg1 (fieldIdx + 1)
                            | _ ->
                                Error $"Nested pattern in record field not yet supported: {pat}"
                    collectRecordBindings fieldPatterns fieldTypes currentEnv [] vg 0
                    |> Result.bind (fun (newEnv, bindings, vg1) ->
                        toANF body vg1 newEnv typeReg variantLookup funcReg moduleRegistry
                        |> Result.map (fun (bodyExpr, vg2) ->
                            let finalExpr = wrapBindings bindings bodyExpr
                            (finalExpr, vg2)))
                | AST.PList patterns ->
                    // Extract list elements from FingerTree structure
                    // FingerTree layout:
                    // SINGLE (tag 1): [node:8] where node is LEAF-tagged
                    // DEEP (tag 2): [measure:8][prefixCount:8][p0:8][p1:8][p2:8][p3:8][middle:8][suffixCount:8][s0:8][s1:8][s2:8][s3:8]
                    // LEAF (tag 5): [value:8]

                    // Get element type from list type
                    let elemType =
                        match scrutType with
                        | AST.TList t -> t
                        | _ -> AST.TInt64

                    // Helper to unwrap a LEAF node and get the value
                    let unwrapLeaf (leafTaggedPtr: ANF.Atom) (vg: ANF.VarGen) (bindings: (ANF.TempId * ANF.CExpr) list) =
                        let (leafPtrVar, vg1) = ANF.freshVar vg
                        let leafPtrExpr = ANF.Prim (ANF.BitAnd, leafTaggedPtr, ANF.IntLiteral (ANF.Int64 0xFFFFFFFFFFFFFFF8L))
                        let (valueVar, vg2) = ANF.freshVar vg1
                        let valueExpr = ANF.RawGet (ANF.Var leafPtrVar, ANF.IntLiteral (ANF.Int64 0L), None)
                        let newBindings = bindings @ [(leafPtrVar, leafPtrExpr); (valueVar, valueExpr)]
                        (ANF.Var valueVar, valueVar, newBindings, vg2)

                    // Helper to extract tuple elements from a value
                    // tupleType is the type of the tuple being destructured
                    let rec collectTupleBindings (tupPats: AST.Pattern list) (tupleAtom: ANF.Atom) (tupleType: AST.Type) (idx: int) (env: VarEnv) (bindings: (ANF.TempId * ANF.CExpr) list) (vg: ANF.VarGen) : Result<VarEnv * (ANF.TempId * ANF.CExpr) list * ANF.VarGen, string> =
                        let tupleElemTypes =
                            match tupleType with
                            | AST.TTuple types -> types
                            | _ -> List.replicate (List.length tupPats) AST.TInt64
                        match tupPats with
                        | [] -> Ok (env, bindings, vg)
                        | tupPat :: tupRest ->
                            let (elemVar, vg1) = ANF.freshVar vg
                            let elemExpr = ANF.TupleGet (tupleAtom, idx)
                            let elemBinding = (elemVar, elemExpr)
                            let elemT = if idx < List.length tupleElemTypes then List.item idx tupleElemTypes else AST.TInt64
                            match tupPat with
                            | AST.PVar name ->
                                let newEnv = Map.add name (elemVar, elemT) env
                                collectTupleBindings tupRest tupleAtom tupleType (idx + 1) newEnv (bindings @ [elemBinding]) vg1
                            | AST.PWildcard ->
                                collectTupleBindings tupRest tupleAtom tupleType (idx + 1) env bindings vg1
                            | AST.PInt64 _
                            | AST.PInt8Literal _
                            | AST.PInt16Literal _
                            | AST.PInt32Literal _
                            | AST.PUInt8Literal _
                            | AST.PUInt16Literal _
                            | AST.PUInt32Literal _
                            | AST.PUInt64Literal _
                            | AST.PUnit
                            | AST.PConstructor _
                            | AST.PBool _
                            | AST.PString _ | AST.PFloat _ | AST.PTuple _ | AST.PRecord _
                            | AST.PList _ | AST.PListCons _ ->
                                Error $"Nested pattern in tuple element not yet supported: {tupPat}"

                    let patternLen = List.length patterns
                    if patternLen = 0 then
                        // Empty pattern - no bindings needed
                        toANF body vg currentEnv typeReg variantLookup funcReg moduleRegistry
                    elif patternLen = 1 then
                        // SINGLE node: extract the single element
                        // Untag to get pointer to SINGLE structure
                        let (ptrVar, vg1) = ANF.freshVar vg
                        let ptrExpr = ANF.Prim (ANF.BitAnd, scrutAtom, ANF.IntLiteral (ANF.Int64 0xFFFFFFFFFFFFFFF8L))
                        // Get the LEAF-tagged node at offset 0
                        let (nodeVar, vg2) = ANF.freshVar vg1
                        let nodeExpr = ANF.RawGet (ANF.Var ptrVar, ANF.IntLiteral (ANF.Int64 0L), None)
                        // Unwrap the LEAF to get the value
                        let (rawValueAtom, rawValueVar, rawBindings, vg3) = unwrapLeaf (ANF.Var nodeVar) vg2 [(ptrVar, ptrExpr); (nodeVar, nodeExpr)]
                        // Wrap with TypedAtom to preserve element type in TypeMap
                        let (typedValueVar, vg3') = ANF.freshVar vg3
                        let typedValueExpr = ANF.TypedAtom (rawValueAtom, elemType)
                        let bindings = rawBindings @ [(typedValueVar, typedValueExpr)]
                        let valueVar = typedValueVar
                        let valueAtom = ANF.Var typedValueVar
                        // Bind the pattern
                        match List.head patterns with
                        | AST.PVar name ->
                            let newEnv = Map.add name (valueVar, elemType) currentEnv
                            toANF body vg3' newEnv typeReg variantLookup funcReg moduleRegistry
                            |> Result.map (fun (bodyExpr, vg4) ->
                                (wrapBindings bindings bodyExpr, vg4))
                        | AST.PWildcard ->
                            toANF body vg3' currentEnv typeReg variantLookup funcReg moduleRegistry
                            |> Result.map (fun (bodyExpr, vg4) ->
                                (wrapBindings bindings bodyExpr, vg4))
                        | AST.PTuple innerPatterns ->
                            // elemType is the list element type, use it as tuple type
                            collectTupleBindings innerPatterns valueAtom elemType 0 currentEnv bindings vg3'
                            |> Result.bind (fun (newEnv, newBindings, vg4) ->
                                toANF body vg4 newEnv typeReg variantLookup funcReg moduleRegistry
                                |> Result.map (fun (bodyExpr, vg5) ->
                                    (wrapBindings newBindings bodyExpr, vg5)))
                        | AST.PConstructor _ | AST.PList _ | AST.PListCons _ ->
                            Error "Nested pattern in list element not yet supported"
                        | _ ->
                            Error $"Unsupported pattern in single-element list: {List.head patterns}"
                    else
                        // DEEP node: extract elements from prefix and suffix
                        // Untag to get pointer to DEEP structure
                        let (ptrVar, vg1) = ANF.freshVar vg
                        let ptrExpr = ANF.Prim (ANF.BitAnd, scrutAtom, ANF.IntLiteral (ANF.Int64 0xFFFFFFFFFFFFFFF8L))
                        let initialBindings = [(ptrVar, ptrExpr)]

                        // Extract elements - first from prefix, then from suffix
                        // Prefix offsets: 16, 24, 32, 40 (p0-p3)
                        // Suffix offsets: 64, 72, 80, 88 (s0-s3)
                        let rec extractElements (pats: AST.Pattern list) (idx: int) (env: VarEnv) (bindings: (ANF.TempId * ANF.CExpr) list) (vg: ANF.VarGen) : Result<VarEnv * (ANF.TempId * ANF.CExpr) list * ANF.VarGen, string> =
                            match pats with
                            | [] -> Ok (env, bindings, vg)
                            | pat :: rest ->
                                // Calculate offset based on position
                                // First element at idx 0 is in prefix at offset 16
                                // For DEEP nodes with elements in prefix/suffix:
                                // We place first element in prefix, rest in suffix
                                let offset =
                                    if idx = 0 then 16L  // p0
                                    else 64L + (int64 (idx - 1) * 8L)  // s0, s1, s2, s3 at 64, 72, 80, 88

                                // Get the LEAF-tagged node
                                let (nodeVar, vg1) = ANF.freshVar vg
                                let nodeExpr = ANF.RawGet (ANF.Var ptrVar, ANF.IntLiteral (ANF.Int64 offset), None)
                                // Unwrap the LEAF to get the value
                                let (rawValueAtom, rawValueVar, rawBindings, vg2) = unwrapLeaf (ANF.Var nodeVar) vg1 (bindings @ [(nodeVar, nodeExpr)])
                                // Wrap with TypedAtom to preserve element type in TypeMap
                                let (typedValueVar, vg2') = ANF.freshVar vg2
                                let typedValueExpr = ANF.TypedAtom (rawValueAtom, elemType)
                                let newBindings = rawBindings @ [(typedValueVar, typedValueExpr)]
                                let valueVar = typedValueVar
                                let valueAtom = ANF.Var typedValueVar

                                match pat with
                                | AST.PVar name ->
                                    let newEnv = Map.add name (valueVar, elemType) env
                                    extractElements rest (idx + 1) newEnv newBindings vg2'
                                | AST.PWildcard ->
                                    extractElements rest (idx + 1) env newBindings vg2'
                                | AST.PTuple innerPatterns ->
                                    // elemType is the list element type, use it as tuple type
                                    collectTupleBindings innerPatterns valueAtom elemType 0 env newBindings vg2'
                                    |> Result.bind (fun (tupEnv, tupBindings, vg3) ->
                                        extractElements rest (idx + 1) tupEnv tupBindings vg3)
                                | _ ->
                                    Error $"Unsupported pattern in list element: {pat}"

                        extractElements patterns 0 currentEnv initialBindings vg1
                        |> Result.bind (fun (newEnv, bindings, vg2) ->
                            toANF body vg2 newEnv typeReg variantLookup funcReg moduleRegistry
                            |> Result.map (fun (bodyExpr, vg3) ->
                                (wrapBindings bindings bodyExpr, vg3)))
                | AST.PListCons (headPatterns, tailPattern) ->
                    // Get element type from list type
                    let elemType =
                        match scrutType with
                        | AST.TList t -> t
                        | _ -> Crash.crash $"PListCons pattern expects TList scrutinee in extractAndCompileBody, got {scrutType}"
                    // Extract head elements and bind tail using FingerTree operations
                    // Lists are FingerTrees, use headUnsafe_i64/tail_i64 for extraction
                    let rec collectListConsBindings (pats: AST.Pattern list) (listAtom: ANF.Atom) (env: VarEnv) (bindings: (ANF.TempId * ANF.CExpr) list) (vg: ANF.VarGen) : Result<VarEnv * (ANF.TempId * ANF.CExpr) list * ANF.Atom * ANF.VarGen, string> =
                        match pats with
                        | [] -> Ok (env, List.rev bindings, listAtom, vg)
                        | pat :: rest ->
                            // Extract head using FingerTree.headUnsafe_i64
                            let (rawHeadVar, vg1) = ANF.freshVar vg
                            let rawHeadExpr = ANF.Call ("Stdlib.__FingerTree.headUnsafe_i64", [listAtom])
                            let rawHeadBinding = (rawHeadVar, rawHeadExpr)
                            // Wrap with TypedAtom to preserve correct element type in TypeMap
                            let (headVar, vg1') = ANF.freshVar vg1
                            let headExpr = ANF.TypedAtom (ANF.Var rawHeadVar, elemType)
                            let headBinding = (headVar, headExpr)
                            // Extract tail using FingerTree.tail_i64
                            let (rawTailVar, vg2) = ANF.freshVar vg1'
                            let rawTailExpr = ANF.Call ("Stdlib.__FingerTree.tail_i64", [listAtom])
                            let rawTailBinding = (rawTailVar, rawTailExpr)
                            // Wrap with TypedAtom to preserve list type for tail
                            let listType = AST.TList elemType
                            let (tailVar, vg2') = ANF.freshVar vg2
                            let tailExpr = ANF.TypedAtom (ANF.Var rawTailVar, listType)
                            let tailBinding = (tailVar, tailExpr)
                            // All bindings including raw extractions
                            // Order: typedBindings first (will be reversed at line 3923), so after reversal raw bindings come before typed
                            let allBaseBindings = tailBinding :: rawTailBinding :: headBinding :: rawHeadBinding :: bindings
                            match pat with
                            | AST.PVar name ->
                                let newEnv = Map.add name (headVar, elemType) env
                                collectListConsBindings rest (ANF.Var tailVar) newEnv allBaseBindings vg2'
                            | AST.PWildcard ->
                                collectListConsBindings rest (ANF.Var tailVar) env allBaseBindings vg2'
                            | AST.PTuple innerPatterns ->
                                // For tuple patterns inside list cons, extract each tuple element and bind variables
                                // elemType is the tuple type (since list elements are tuples)
                                let tupleElemTypes =
                                    match elemType with
                                    | AST.TTuple types -> types
                                    | _ -> List.replicate (List.length innerPatterns) AST.TInt64
                                let rec collectTupleBindings (tupPats: AST.Pattern list) (types: AST.Type list) (tupleAtom: ANF.Atom) (idx: int) (env: VarEnv) (bindings: (ANF.TempId * ANF.CExpr) list) (vg: ANF.VarGen) : Result<VarEnv * (ANF.TempId * ANF.CExpr) list * ANF.VarGen, string> =
                                    match tupPats with
                                    | [] -> Ok (env, bindings, vg)
                                    | tupPat :: tupRest ->
                                        // Extract raw element with TupleGet
                                        let (rawElemVar, vg1) = ANF.freshVar vg
                                        let rawElemExpr = ANF.TupleGet (tupleAtom, idx)
                                        let rawElemBinding = (rawElemVar, rawElemExpr)
                                        let elemT = if idx < List.length types then List.item idx types else AST.TInt64
                                        // Wrap with TypedAtom to preserve correct element type
                                        let (elemVar, vg1') = ANF.freshVar vg1
                                        let elemExpr = ANF.TypedAtom (ANF.Var rawElemVar, elemT)
                                        let elemBinding = (elemVar, elemExpr)
                                        match tupPat with
                                        | AST.PVar name ->
                                            let newEnv = Map.add name (elemVar, elemT) env
                                            collectTupleBindings tupRest types tupleAtom (idx + 1) newEnv (elemBinding :: rawElemBinding :: bindings) vg1'
                                        | AST.PWildcard ->
                                            collectTupleBindings tupRest types tupleAtom (idx + 1) env (rawElemBinding :: bindings) vg1
                                        | AST.PInt64 _
                                        | AST.PInt8Literal _
                                        | AST.PInt16Literal _
                                        | AST.PInt32Literal _
                                        | AST.PUInt8Literal _
                                        | AST.PUInt16Literal _
                                        | AST.PUInt32Literal _
                                        | AST.PUInt64Literal _
                                        | AST.PUnit
                                        | AST.PConstructor _
                                        | AST.PBool _
                                        | AST.PString _ | AST.PFloat _ | AST.PTuple _ | AST.PRecord _
                                        | AST.PList _ | AST.PListCons _ ->
                                            Error $"Nested pattern in tuple element not yet supported: {tupPat}"
                                collectTupleBindings innerPatterns tupleElemTypes (ANF.Var headVar) 0 env allBaseBindings vg2'
                                |> Result.bind (fun (newEnv, newBindings, vg3) ->
                                    collectListConsBindings rest (ANF.Var tailVar) newEnv newBindings vg3)
                            | AST.PInt64 _
                            | AST.PInt8Literal _
                            | AST.PInt16Literal _
                            | AST.PInt32Literal _
                            | AST.PUInt8Literal _
                            | AST.PUInt16Literal _
                            | AST.PUInt32Literal _
                            | AST.PUInt64Literal _
                            | AST.PUnit
                            | AST.PConstructor _
                            | AST.PBool _
                            | AST.PString _ | AST.PFloat _ | AST.PRecord _
                            | AST.PList _ | AST.PListCons _ ->
                                Error $"Nested pattern in list cons element not yet supported: {pat}"
                    collectListConsBindings headPatterns scrutAtom currentEnv [] vg
                    |> Result.bind (fun (newEnv, bindings, tailAtom, vg1) ->
                        // Bind tail pattern
                        match tailPattern with
                        | AST.PVar name ->
                            let (tailVar, vg2) = ANF.freshVar vg1
                            // Tail has the same list type as the scrutinee
                            let newEnv' = Map.add name (tailVar, scrutType) newEnv
                            toANF body vg2 newEnv' typeReg variantLookup funcReg moduleRegistry
                            |> Result.map (fun (bodyExpr, vg3) ->
                                let tailBinding = (tailVar, ANF.TypedAtom (tailAtom, scrutType))
                                let allBindings = bindings @ [tailBinding]
                                let finalExpr = wrapBindings allBindings bodyExpr
                                (finalExpr, vg3))
                        | AST.PWildcard ->
                            toANF body vg1 newEnv typeReg variantLookup funcReg moduleRegistry
                            |> Result.map (fun (bodyExpr, vg2) ->
                                let finalExpr = wrapBindings bindings bodyExpr
                                (finalExpr, vg2))
                        | _ -> Error "Tail pattern in list cons must be variable or wildcard")

            // Extract pattern bindings, check guard, and compile body
            // Returns: if guard is true, execute body; otherwise execute elseExpr
            // scrutType is the type of the scrutinee for correct pattern variable typing
            and extractAndCompileBodyWithGuard (pattern: AST.Pattern) (guardExpr: AST.Expr) (body: AST.Expr) (scrutAtom: ANF.Atom) (scrutType: AST.Type) (currentEnv: VarEnv) (vg: ANF.VarGen) (elseExpr: ANF.AExpr) : Result<ANF.AExpr * ANF.VarGen, string> =
                // First, we need to extract bindings from the pattern
                // Then compile the guard with those bindings in scope
                // Then compile the body with those bindings in scope
                // Finally, generate: let <bindings> in if <guard> then <body> else <elseExpr>

                // Helper to collect pattern variable bindings (simplified version for common patterns)
                // sourceType is the type of the source being matched
                let rec collectBindings (pat: AST.Pattern) (sourceAtom: ANF.Atom) (sourceType: AST.Type) (env: VarEnv) (bindings: (ANF.TempId * ANF.CExpr) list) (vg: ANF.VarGen) : Result<VarEnv * (ANF.TempId * ANF.CExpr) list * ANF.VarGen, string> =
                    match pat with
                    | AST.PInt64 _
                    | AST.PInt8Literal _
                    | AST.PInt16Literal _
                    | AST.PInt32Literal _
                    | AST.PUInt8Literal _
                    | AST.PUInt16Literal _
                    | AST.PUInt32Literal _
                    | AST.PUInt64Literal _
                    | AST.PUnit
                    | AST.PWildcard
                    | AST.PBool _
                    | AST.PString _
                    | AST.PFloat _ ->
                        Ok (env, bindings, vg)
                    | AST.PVar name ->
                        let (tempId, vg1) = ANF.freshVar vg
                        // Use TypedAtom to preserve the correct type in TypeMap
                        let binding = (tempId, ANF.TypedAtom (sourceAtom, sourceType))
                        let newEnv = Map.add name (tempId, sourceType) env
                        Ok (newEnv, binding :: bindings, vg1)
                    | AST.PTuple innerPatterns ->
                        let elemTypes =
                            match sourceType with
                            | AST.TTuple types -> types
                            | _ -> List.replicate (List.length innerPatterns) AST.TInt64
                        let rec collectFromTuple pats types idx env bindings vg =
                            match pats, types with
                            | [], _ -> Ok (env, bindings, vg)
                            | p :: rest, t :: restTypes ->
                                let (elemVar, vg1) = ANF.freshVar vg
                                let elemExpr = ANF.TupleGet (sourceAtom, idx)
                                collectBindings p (ANF.Var elemVar) t env ((elemVar, elemExpr) :: bindings) vg1
                                |> Result.bind (fun (env', bindings', vg') ->
                                    collectFromTuple rest restTypes (idx + 1) env' bindings' vg')
                            | p :: rest, [] ->
                                let (elemVar, vg1) = ANF.freshVar vg
                                let elemExpr = ANF.TupleGet (sourceAtom, idx)
                                collectBindings p (ANF.Var elemVar) AST.TInt64 env ((elemVar, elemExpr) :: bindings) vg1
                                |> Result.bind (fun (env', bindings', vg') ->
                                    collectFromTuple rest [] (idx + 1) env' bindings' vg')
                        collectFromTuple innerPatterns elemTypes 0 env bindings vg
                    | AST.PConstructor (constructorName, payloadPattern) ->
                        let rec substituteType (subst: Map<string, AST.Type>) (typ: AST.Type) : AST.Type =
                            match typ with
                            | AST.TVar name -> Map.tryFind name subst |> Option.defaultValue typ
                            | AST.TTuple elems -> AST.TTuple (List.map (substituteType subst) elems)
                            | AST.TRecord (name, args) -> AST.TRecord (name, List.map (substituteType subst) args)
                            | AST.TList elem -> AST.TList (substituteType subst elem)
                            | AST.TDict (k, v) -> AST.TDict (substituteType subst k, substituteType subst v)
                            | AST.TSum (name, args) -> AST.TSum (name, List.map (substituteType subst) args)
                            | AST.TFunction (args, ret) -> AST.TFunction (List.map (substituteType subst) args, substituteType subst ret)
                            | _ -> typ

                        let resolvePayloadType (constructorName: string) (scrutineeType: AST.Type) : Result<AST.Type, string> =
                            match Map.tryFind constructorName variantLookup with
                            | Some (_, typeParams, _, Some payloadTypeTemplate) ->
                                let payloadType =
                                    match scrutineeType with
                                    | AST.TSum (_, typeArgs) when List.length typeParams = List.length typeArgs ->
                                        let subst = List.zip typeParams typeArgs |> Map.ofList
                                        substituteType subst payloadTypeTemplate
                                    | _ -> payloadTypeTemplate
                                Ok payloadType
                            | Some (_, _, _, None) ->
                                Error $"Constructor '{constructorName}' has no payload type"
                            | None ->
                                Error $"Unknown constructor '{constructorName}' in pattern"

                        match payloadPattern with
                        | None -> Ok (env, bindings, vg)
                        | Some innerPat ->
                            let (payloadVar, vg1) = ANF.freshVar vg
                            let payloadExpr = ANF.TupleGet (sourceAtom, 1)
                            resolvePayloadType constructorName sourceType
                            |> Result.bind (fun payloadType ->
                                collectBindings innerPat (ANF.Var payloadVar) payloadType env ((payloadVar, payloadExpr) :: bindings) vg1)
                    | AST.PRecord (_, fieldPatterns) ->
                        let fieldTypes =
                            match sourceType with
                            | AST.TRecord (recordName, _) ->
                                match Map.tryFind recordName typeReg with
                                | Some fields -> fields |> List.map snd
                                | None -> List.replicate (List.length fieldPatterns) AST.TInt64
                            | _ -> List.replicate (List.length fieldPatterns) AST.TInt64
                        let rec collectFromRecord fields types idx env bindings vg =
                            match fields, types with
                            | [], _ -> Ok (env, bindings, vg)
                            | (_, p) :: rest, t :: restTypes ->
                                let (fieldVar, vg1) = ANF.freshVar vg
                                let fieldExpr = ANF.TupleGet (sourceAtom, idx)
                                collectBindings p (ANF.Var fieldVar) t env ((fieldVar, fieldExpr) :: bindings) vg1
                                |> Result.bind (fun (env', bindings', vg') ->
                                    collectFromRecord rest restTypes (idx + 1) env' bindings' vg')
                            | (_, p) :: rest, [] ->
                                let (fieldVar, vg1) = ANF.freshVar vg
                                let fieldExpr = ANF.TupleGet (sourceAtom, idx)
                                collectBindings p (ANF.Var fieldVar) AST.TInt64 env ((fieldVar, fieldExpr) :: bindings) vg1
                                |> Result.bind (fun (env', bindings', vg') ->
                                    collectFromRecord rest [] (idx + 1) env' bindings' vg')
                        collectFromRecord fieldPatterns fieldTypes 0 env bindings vg
                    | AST.PList innerPatterns ->
                        let elemType =
                            match sourceType with
                            | AST.TList t -> t
                            | _ -> AST.TInt64
                        // For list patterns, extract head elements using FingerTree operations
                        // Use _i64 versions which work for any element type at runtime (all values are 64-bit)
                        // The correct element type is tracked in the VarEnv/TypeMap, not in the function name
                        let rec collectFromList (pats: AST.Pattern list) (currentList: ANF.Atom) (env: VarEnv) (bindings: (ANF.TempId * ANF.CExpr) list) (vg: ANF.VarGen) =
                            match pats with
                            | [] -> Ok (env, bindings, vg)
                            | p :: rest ->
                                // Lists are FingerTrees - use headUnsafe/tail to extract
                                let (headVar, vg1) = ANF.freshVar vg
                                let headExpr = ANF.Call ("Stdlib.__FingerTree.headUnsafe_i64", [currentList])
                                let headBinding = (headVar, headExpr)
                                collectBindings p (ANF.Var headVar) elemType env (headBinding :: bindings) vg1
                                |> Result.bind (fun (env', bindings', vg') ->
                                    if List.isEmpty rest then
                                        Ok (env', bindings', vg')
                                    else
                                        // Get tail for next iteration
                                        let (tailVar, vg2) = ANF.freshVar vg'
                                        let tailExpr = ANF.Call ("Stdlib.__FingerTree.tail_i64", [currentList])
                                        let tailBinding = (tailVar, tailExpr)
                                        collectFromList rest (ANF.Var tailVar) env' (tailBinding :: bindings') vg2)
                        collectFromList innerPatterns sourceAtom env bindings vg
                    | AST.PListCons (headPatterns, tailPattern) ->
                        let elemType =
                            match sourceType with
                            | AST.TList t -> t
                            | _ -> AST.TInt64
                        // Extract head elements then bind tail using FingerTree operations
                        // Use _i64 versions which work for any element type at runtime (all values are 64-bit)
                        // The correct element type is tracked in the VarEnv/TypeMap, not in the function name
                        let rec collectHeads (pats: AST.Pattern list) (currentList: ANF.Atom) (env: VarEnv) (bindings: (ANF.TempId * ANF.CExpr) list) (vg: ANF.VarGen) =
                            match pats with
                            | [] ->
                                // Bind the remaining list to tail pattern (tail has same type as source)
                                collectBindings tailPattern currentList sourceType env bindings vg
                            | p :: rest ->
                                // Lists are FingerTrees - use headUnsafe/tail to extract
                                let (headVar, vg1) = ANF.freshVar vg
                                let headExpr = ANF.Call ("Stdlib.__FingerTree.headUnsafe_i64", [currentList])
                                let headBinding = (headVar, headExpr)
                                collectBindings p (ANF.Var headVar) elemType env (headBinding :: bindings) vg1
                                |> Result.bind (fun (env', bindings', vg') ->
                                    let (tailVar, vg2) = ANF.freshVar vg'
                                    let tailExpr = ANF.Call ("Stdlib.__FingerTree.tail_i64", [currentList])
                                    let tailBinding = (tailVar, tailExpr)
                                    collectHeads rest (ANF.Var tailVar) env' (tailBinding :: bindings') vg2)
                        collectHeads headPatterns sourceAtom env bindings vg

                collectBindings pattern scrutAtom scrutType currentEnv [] vg
                |> Result.bind (fun (newEnv, bindings, vg1) ->
                    // Compile guard expression in the extended environment
                    toAtom guardExpr vg1 newEnv typeReg variantLookup funcReg moduleRegistry
                    |> Result.bind (fun (guardAtom, guardBindings, vg2) ->
                        // Compile body expression in the extended environment
                        toANF body vg2 newEnv typeReg variantLookup funcReg moduleRegistry
                        |> Result.map (fun (bodyExpr, vg3) ->
                            // Build: if guard then body else elseExpr
                            let ifExpr = ANF.If (guardAtom, bodyExpr, elseExpr)
                            // Wrap guard bindings
                            let withGuardBindings = wrapBindings guardBindings ifExpr
                            // Wrap pattern bindings (in reverse order since we accumulated in reverse)
                            let finalExpr = wrapBindings (List.rev bindings) withGuardBindings
                            (finalExpr, vg3))))

            // Build comparison expression for a pattern
            let rec buildPatternComparison (pattern: AST.Pattern) (scrutAtom: ANF.Atom) (vg: ANF.VarGen) : Result<(ANF.Atom * (ANF.TempId * ANF.CExpr) list * ANF.VarGen) option, string> =
                match pattern with
                | AST.PUnit -> Ok None  // Unit pattern always matches unit type
                | AST.PWildcard -> Ok None
                | AST.PVar _ -> Ok None
                | AST.PInt64 n ->
                    let (cmpVar, vg1) = ANF.freshVar vg
                    let cmpExpr = ANF.Prim (ANF.Eq, scrutAtom, ANF.IntLiteral (ANF.Int64 n))
                    Ok (Some (ANF.Var cmpVar, [(cmpVar, cmpExpr)], vg1))
                | AST.PInt8Literal n ->
                    let (cmpVar, vg1) = ANF.freshVar vg
                    let cmpExpr = ANF.Prim (ANF.Eq, scrutAtom, ANF.IntLiteral (ANF.Int8 n))
                    Ok (Some (ANF.Var cmpVar, [(cmpVar, cmpExpr)], vg1))
                | AST.PInt16Literal n ->
                    let (cmpVar, vg1) = ANF.freshVar vg
                    let cmpExpr = ANF.Prim (ANF.Eq, scrutAtom, ANF.IntLiteral (ANF.Int16 n))
                    Ok (Some (ANF.Var cmpVar, [(cmpVar, cmpExpr)], vg1))
                | AST.PInt32Literal n ->
                    let (cmpVar, vg1) = ANF.freshVar vg
                    let cmpExpr = ANF.Prim (ANF.Eq, scrutAtom, ANF.IntLiteral (ANF.Int32 n))
                    Ok (Some (ANF.Var cmpVar, [(cmpVar, cmpExpr)], vg1))
                | AST.PUInt8Literal n ->
                    let (cmpVar, vg1) = ANF.freshVar vg
                    let cmpExpr = ANF.Prim (ANF.Eq, scrutAtom, ANF.IntLiteral (ANF.UInt8 n))
                    Ok (Some (ANF.Var cmpVar, [(cmpVar, cmpExpr)], vg1))
                | AST.PUInt16Literal n ->
                    let (cmpVar, vg1) = ANF.freshVar vg
                    let cmpExpr = ANF.Prim (ANF.Eq, scrutAtom, ANF.IntLiteral (ANF.UInt16 n))
                    Ok (Some (ANF.Var cmpVar, [(cmpVar, cmpExpr)], vg1))
                | AST.PUInt32Literal n ->
                    let (cmpVar, vg1) = ANF.freshVar vg
                    let cmpExpr = ANF.Prim (ANF.Eq, scrutAtom, ANF.IntLiteral (ANF.UInt32 n))
                    Ok (Some (ANF.Var cmpVar, [(cmpVar, cmpExpr)], vg1))
                | AST.PUInt64Literal n ->
                    let (cmpVar, vg1) = ANF.freshVar vg
                    let cmpExpr = ANF.Prim (ANF.Eq, scrutAtom, ANF.IntLiteral (ANF.UInt64 n))
                    Ok (Some (ANF.Var cmpVar, [(cmpVar, cmpExpr)], vg1))
                | AST.PBool b ->
                    let (cmpVar, vg1) = ANF.freshVar vg
                    let cmpExpr = ANF.Prim (ANF.Eq, scrutAtom, ANF.BoolLiteral b)
                    Ok (Some (ANF.Var cmpVar, [(cmpVar, cmpExpr)], vg1))
                | AST.PString s ->
                    // String comparison - for now just compare as atoms
                    let (cmpVar, vg1) = ANF.freshVar vg
                    let cmpExpr = ANF.Prim (ANF.Eq, scrutAtom, ANF.StringLiteral s)
                    Ok (Some (ANF.Var cmpVar, [(cmpVar, cmpExpr)], vg1))
                | AST.PFloat f ->
                    let (cmpVar, vg1) = ANF.freshVar vg
                    let cmpExpr = ANF.Prim (ANF.Eq, scrutAtom, ANF.FloatLiteral f)
                    Ok (Some (ANF.Var cmpVar, [(cmpVar, cmpExpr)], vg1))
                | AST.PConstructor (variantName, payloadPattern) ->
                    match Map.tryFind variantName variantLookup with
                    | Some (_, _, tag, _) ->
                        if typeHasAnyPayload variantName then
                            // Load tag from heap (index 0), then compare
                            let (tagVar, vg1) = ANF.freshVar vg
                            let tagLoadExpr = ANF.TupleGet (scrutAtom, 0)
                            let (tagCmpVar, vg2) = ANF.freshVar vg1
                            let tagCmpExpr = ANF.Prim (ANF.Eq, ANF.Var tagVar, ANF.IntLiteral (ANF.Int64 (int64 tag)))

                            // Check if payload pattern needs comparison (e.g., Some(true) vs Some(false))
                            match payloadPattern with
                            | Some innerPattern ->
                                // Extract payload and check if inner pattern needs comparison
                                let (payloadVar, vg3) = ANF.freshVar vg2
                                let payloadLoadExpr = ANF.TupleGet (scrutAtom, 1)
                                buildPatternComparison innerPattern (ANF.Var payloadVar) vg3
                                |> Result.map (fun innerResult ->
                                    match innerResult with
                                    | None ->
                                        // Inner pattern is variable/wildcard, only need tag check
                                        Some (ANF.Var tagCmpVar, [(tagVar, tagLoadExpr); (tagCmpVar, tagCmpExpr)], vg3)
                                    | Some (innerCond, innerBindings, vg4) ->
                                        // Need to AND tag check with payload check
                                        let (andVar, vg5) = ANF.freshVar vg4
                                        let andExpr = ANF.Prim (ANF.And, ANF.Var tagCmpVar, innerCond)
                                        let allBindings = [(tagVar, tagLoadExpr); (tagCmpVar, tagCmpExpr); (payloadVar, payloadLoadExpr)] @ innerBindings @ [(andVar, andExpr)]
                                        Some (ANF.Var andVar, allBindings, vg5))
                            | None ->
                                // No payload pattern, just check tag
                                Ok (Some (ANF.Var tagCmpVar, [(tagVar, tagLoadExpr); (tagCmpVar, tagCmpExpr)], vg2))
                        else
                            // Simple enum - scrutinee IS the tag
                            let (cmpVar, vg1) = ANF.freshVar vg
                            let cmpExpr = ANF.Prim (ANF.Eq, scrutAtom, ANF.IntLiteral (ANF.Int64 (int64 tag)))
                            Ok (Some (ANF.Var cmpVar, [(cmpVar, cmpExpr)], vg1))
                    | None -> Error $"Unknown constructor in pattern: {variantName}"
                | AST.PTuple innerPatterns ->
                    // Tuple patterns with literals need to compare each element
                    let rec buildTupleComparisons (patterns: AST.Pattern list) (index: int) (vg: ANF.VarGen) (accBindings: (ANF.TempId * ANF.CExpr) list) (accConditions: ANF.Atom list) =
                        match patterns with
                        | [] ->
                            if List.isEmpty accConditions then
                                Ok None  // All variables/wildcards, no comparison needed
                            else
                                // AND together all conditions
                                let rec andAll (conds: ANF.Atom list) (vg: ANF.VarGen) (bindings: (ANF.TempId * ANF.CExpr) list) =
                                    match conds with
                                    | [] -> Error "Empty conditions list"
                                    | [single] -> Ok (single, bindings, vg)
                                    | first :: rest ->
                                        andAll rest vg bindings
                                        |> Result.map (fun (restResult, restBindings, vg1) ->
                                            let (andVar, vg2) = ANF.freshVar vg1
                                            let andExpr = ANF.Prim (ANF.And, first, restResult)
                                            (ANF.Var andVar, restBindings @ [(andVar, andExpr)], vg2))
                                andAll accConditions vg accBindings
                                |> Result.map (fun (result, bindings, vg') -> Some (result, bindings, vg'))
                        | p :: rest ->
                            // Extract element at index
                            let (elemVar, vg1) = ANF.freshVar vg
                            let elemLoad = ANF.TupleGet (scrutAtom, index)
                            let newBindings = accBindings @ [(elemVar, elemLoad)]
                            // Check if this pattern needs comparison
                            buildPatternComparison p (ANF.Var elemVar) vg1
                            |> Result.bind (fun compResult ->
                                match compResult with
                                | None ->
                                    // This element pattern doesn't need comparison (var/wildcard)
                                    buildTupleComparisons rest (index + 1) vg1 newBindings accConditions
                                | Some (cond, condBindings, vg2) ->
                                    // Add this comparison
                                    buildTupleComparisons rest (index + 1) vg2 (newBindings @ condBindings) (accConditions @ [cond]))
                    buildTupleComparisons innerPatterns 0 vg [] []
                | AST.PRecord (_, fieldPatterns) ->
                    // Record patterns with literals need to compare each field
                    let rec buildRecordComparisons (fields: (string * AST.Pattern) list) (vg: ANF.VarGen) (accBindings: (ANF.TempId * ANF.CExpr) list) (accConditions: ANF.Atom list) =
                        match fields with
                        | [] ->
                            if List.isEmpty accConditions then
                                Ok None
                            else
                                let rec andAll (conds: ANF.Atom list) (vg: ANF.VarGen) (bindings: (ANF.TempId * ANF.CExpr) list) =
                                    match conds with
                                    | [] -> Error "Empty conditions list"
                                    | [single] -> Ok (single, bindings, vg)
                                    | first :: rest ->
                                        andAll rest vg bindings
                                        |> Result.map (fun (restResult, restBindings, vg1) ->
                                            let (andVar, vg2) = ANF.freshVar vg1
                                            let andExpr = ANF.Prim (ANF.And, first, restResult)
                                            (ANF.Var andVar, restBindings @ [(andVar, andExpr)], vg2))
                                andAll accConditions vg accBindings
                                |> Result.map (fun (result, bindings, vg') -> Some (result, bindings, vg'))
                        | (fieldName, p) :: rest ->
                            // Find field index in the record type (would need type info)
                            // For now, use a simple approach - records are ordered by field definition
                            // This is a simplification; proper implementation would need type lookup
                            let fieldIndex = List.findIndex (fun (fn, _) -> fn = fieldName) fieldPatterns
                            let (elemVar, vg1) = ANF.freshVar vg
                            let elemLoad = ANF.TupleGet (scrutAtom, fieldIndex)
                            let newBindings = accBindings @ [(elemVar, elemLoad)]
                            buildPatternComparison p (ANF.Var elemVar) vg1
                            |> Result.bind (fun compResult ->
                                match compResult with
                                | None -> buildRecordComparisons rest vg1 newBindings accConditions
                                | Some (cond, condBindings, vg2) ->
                                    buildRecordComparisons rest vg2 (newBindings @ condBindings) (accConditions @ [cond]))
                    buildRecordComparisons fieldPatterns vg [] []
                | AST.PList patterns ->
                    // List pattern comparison: check length matches for FingerTree
                    // FingerTree tags: EMPTY=0, SINGLE=1, DEEP=2
                    // For [] pattern: check scrutinee == 0 (EMPTY)
                    // For [a] pattern: check tag == 1 (SINGLE)
                    // For [a, b, ...] pattern: check tag == 2 (DEEP) and measure == length
                    let patternLen = List.length patterns
                    if patternLen = 0 then
                        // Empty list pattern: check scrutinee == 0
                        let (cmpVar, vg1) = ANF.freshVar vg
                        let cmpExpr = ANF.Prim (ANF.Eq, scrutAtom, ANF.IntLiteral (ANF.Int64 0L))
                        Ok (Some (ANF.Var cmpVar, [(cmpVar, cmpExpr)], vg1))
                    elif patternLen = 1 then
                        // Single element pattern: check tag == 1 (SINGLE)
                        let (tagVar, vg1) = ANF.freshVar vg
                        let tagExpr = ANF.Prim (ANF.BitAnd, scrutAtom, ANF.IntLiteral (ANF.Int64 7L))
                        let (cmpVar, vg2) = ANF.freshVar vg1
                        let cmpExpr = ANF.Prim (ANF.Eq, ANF.Var tagVar, ANF.IntLiteral (ANF.Int64 1L))
                        Ok (Some (ANF.Var cmpVar, [(tagVar, tagExpr); (cmpVar, cmpExpr)], vg2))
                    else
                        // Multiple elements: check length == patternLen
                        // Use Stdlib.__FingerTree.length which handles EMPTY/SINGLE/DEEP safely
                        let (lengthVar, vg1) = ANF.freshVar vg
                        let lengthExpr = ANF.Call ("Stdlib.__FingerTree.length_i64", [scrutAtom])
                        let (cmpVar, vg2) = ANF.freshVar vg1
                        let cmpExpr = ANF.Prim (ANF.Eq, ANF.Var lengthVar, ANF.IntLiteral (ANF.Int64 (int64 patternLen)))
                        Ok (Some (ANF.Var cmpVar, [(lengthVar, lengthExpr); (cmpVar, cmpExpr)], vg2))
                | AST.PListCons (headPatterns, _) ->
                    // List cons pattern: [...t] matches any list, [h, ...t] needs at least one element
                    if List.isEmpty headPatterns then
                        // [...t] matches any list including empty
                        Ok None
                    else
                        // [h, ...t] or [a, b, ...t] - needs at least one element (non-nil)
                        let (cmpVar, vg1) = ANF.freshVar vg
                        let cmpExpr = ANF.Prim (ANF.Neq, scrutAtom, ANF.IntLiteral (ANF.Int64 0L))
                        Ok (Some (ANF.Var cmpVar, [(cmpVar, cmpExpr)], vg1))

            // Compile a list pattern for FingerTree with proper length validation.
            // FingerTree layout:
            // SINGLE (tag 1): [node:8] where node is LEAF-tagged
            // DEEP (tag 2): [measure:8][prefixCount:8][p0:8][p1:8][p2:8][p3:8][middle:8][suffixCount:8][s0:8][s1:8][s2:8][s3:8]
            // LEAF (tag 5): [value:8]
            // listType is the list type (TList elemType) for correct pattern variable typing
            let compileListPatternWithChecks
                (patterns: AST.Pattern list)
                (listAtom: ANF.Atom)
                (listType: AST.Type)
                (currentEnv: VarEnv)
                (body: AST.Expr)
                (elseExpr: ANF.AExpr)
                (vg: ANF.VarGen)
                : Result<ANF.AExpr * ANF.VarGen, string> =

                // Extract element type from list type
                let elemType =
                    match listType with
                    | AST.TList t -> t
                    | _ -> AST.TInt64  // Fallback

                let patternLen = List.length patterns

                // Helper to unwrap a LEAF node and get the value
                let unwrapLeaf (leafTaggedPtr: ANF.Atom) (vg: ANF.VarGen) (bindings: (ANF.TempId * ANF.CExpr) list) =
                    let (leafPtrVar, vg1) = ANF.freshVar vg
                    let leafPtrExpr = ANF.Prim (ANF.BitAnd, leafTaggedPtr, ANF.IntLiteral (ANF.Int64 0xFFFFFFFFFFFFFFF8L))
                    let (valueVar, vg2) = ANF.freshVar vg1
                    let valueExpr = ANF.RawGet (ANF.Var leafPtrVar, ANF.IntLiteral (ANF.Int64 0L), None)
                    let newBindings = bindings @ [(leafPtrVar, leafPtrExpr); (valueVar, valueExpr)]
                    (ANF.Var valueVar, valueVar, newBindings, vg2)

                // Helper to extract tuple elements from a value
                // tupleType is the type of the tuple being matched (TTuple elemTypes)
                let rec extractTupleBindings
                    (tupPats: AST.Pattern list)
                    (tupleAtom: ANF.Atom)
                    (tupleType: AST.Type)
                    (idx: int)
                    (env: VarEnv)
                    (bindings: (ANF.TempId * ANF.CExpr) list)
                    (vg: ANF.VarGen)
                    : Result<VarEnv * (ANF.TempId * ANF.CExpr) list * ANF.VarGen, string> =
                    let tupleElemTypesResult =
                        match tupleType with
                        | AST.TTuple types when List.length types >= List.length tupPats -> Ok types
                        | AST.TTuple types ->
                            Error $"Tuple pattern expects {List.length tupPats} elements but got {List.length types}"
                        | _ ->
                            Error $"Tuple pattern expects tuple elements, got {typeToString tupleType}"
                    match tupleElemTypesResult with
                    | Error err -> Error err
                    | Ok tupleElemTypes ->
                        match tupPats with
                        | [] -> Ok (env, bindings, vg)
                        | tupPat :: tupRest ->
                            let (rawElemVar, vg1) = ANF.freshVar vg
                            let rawElemExpr = ANF.TupleGet (tupleAtom, idx)
                            let rawElemBinding = (rawElemVar, rawElemExpr)
                            let elemT = List.item idx tupleElemTypes
                            // Wrap with TypedAtom to preserve correct element type in TypeMap
                            let (elemVar, vg1') = ANF.freshVar vg1
                            let elemExpr = ANF.TypedAtom (ANF.Var rawElemVar, elemT)
                            let elemBinding = (elemVar, elemExpr)
                            match tupPat with
                            | AST.PVar name ->
                                let newEnv = Map.add name (elemVar, elemT) env  // Use correct element type
                                extractTupleBindings tupRest tupleAtom tupleType (idx + 1) newEnv (bindings @ [rawElemBinding; elemBinding]) vg1'
                            | AST.PWildcard ->
                                extractTupleBindings tupRest tupleAtom tupleType (idx + 1) env (bindings @ [rawElemBinding]) vg1
                            | AST.PInt64 _
                            | AST.PInt8Literal _
                            | AST.PInt16Literal _
                            | AST.PInt32Literal _
                            | AST.PUInt8Literal _
                            | AST.PUInt16Literal _
                            | AST.PUInt32Literal _
                            | AST.PUInt64Literal _
                            | AST.PUnit
                            | AST.PConstructor _
                            | AST.PBool _
                            | AST.PString _ | AST.PFloat _ | AST.PTuple _ | AST.PRecord _
                            | AST.PList _ | AST.PListCons _ ->
                                Error $"Nested pattern in tuple element not yet supported: {tupPat}"

                if patternLen = 0 then
                    // Empty list: check scrutinee == 0 (EMPTY)
                    let (checkVar, vg1) = ANF.freshVar vg
                    let checkExpr = ANF.Prim (ANF.Eq, listAtom, ANF.IntLiteral (ANF.Int64 0L))
                    toANF body vg1 currentEnv typeReg variantLookup funcReg moduleRegistry
                    |> Result.map (fun (bodyExpr, vg2) ->
                        let ifExpr = ANF.If (ANF.Var checkVar, bodyExpr, elseExpr)
                        (ANF.Let (checkVar, checkExpr, ifExpr), vg2))
                elif patternLen = 1 then
                    // Single element: check tag == 1 (SINGLE), then extract
                    let (tagVar, vg1) = ANF.freshVar vg
                    let tagExpr = ANF.Prim (ANF.BitAnd, listAtom, ANF.IntLiteral (ANF.Int64 7L))
                    let (checkVar, vg2) = ANF.freshVar vg1
                    let checkExpr = ANF.Prim (ANF.Eq, ANF.Var tagVar, ANF.IntLiteral (ANF.Int64 1L))

                    // Untag to get pointer to SINGLE structure
                    let (ptrVar, vg3) = ANF.freshVar vg2
                    let ptrExpr = ANF.Prim (ANF.BitAnd, listAtom, ANF.IntLiteral (ANF.Int64 0xFFFFFFFFFFFFFFF8L))
                    // Get the LEAF-tagged node at offset 0
                    let (nodeVar, vg4) = ANF.freshVar vg3
                    let nodeExpr = ANF.RawGet (ANF.Var ptrVar, ANF.IntLiteral (ANF.Int64 0L), None)
                    // Unwrap the LEAF to get the value
                    let (rawValueAtom, rawValueVar, rawBindings, vg5) = unwrapLeaf (ANF.Var nodeVar) vg4 [(ptrVar, ptrExpr); (nodeVar, nodeExpr)]
                    // Wrap with TypedAtom to preserve element type in TypeMap
                    let (typedValueVar, vg5') = ANF.freshVar vg5
                    let typedValueExpr = ANF.TypedAtom (rawValueAtom, elemType)
                    let bindings = rawBindings @ [(typedValueVar, typedValueExpr)]
                    let valueVar = typedValueVar
                    let valueAtom = ANF.Var typedValueVar

                    // Bind the pattern
                    let pat = List.head patterns
                    let compileLiteralPattern (literal: ANF.SizedInt) =
                        // Literal pattern: check tag==SINGLE, extract value, check value==literal
                        // Important: bindings must come BEFORE the literal check since they define valueVar
                        let (litCheckVar, vg6) = ANF.freshVar vg5'
                        let litCheckExpr = ANF.Prim (ANF.Eq, valueAtom, ANF.IntLiteral literal)
                        toANF body vg6 currentEnv typeReg variantLookup funcReg moduleRegistry
                        |> Result.map (fun (bodyExpr, vg7) ->
                            // Structure: check tag -> extract value (bindings) -> check literal -> if match then body else else
                            // Note: We use two nested Ifs because the tag check guards the memory access in bindings
                            let ifLitExpr = ANF.If (ANF.Var litCheckVar, bodyExpr, elseExpr)
                            let withLitBinding = ANF.Let (litCheckVar, litCheckExpr, ifLitExpr)
                            // bindings must be OUTSIDE the inner If to define valueVar before litCheckExpr uses it
                            let withBindings = wrapBindings bindings withLitBinding
                            let withTagCheck = ANF.If (ANF.Var checkVar, withBindings, elseExpr)
                            (ANF.Let (tagVar, tagExpr, ANF.Let (checkVar, checkExpr, withTagCheck)), vg7))

                    match pat with
                    | AST.PVar name ->
                        let newEnv = Map.add name (valueVar, elemType) currentEnv  // Use element type
                        toANF body vg5' newEnv typeReg variantLookup funcReg moduleRegistry
                        |> Result.map (fun (bodyExpr, vg6) ->
                            let withBindings = wrapBindings bindings bodyExpr
                            let ifExpr = ANF.If (ANF.Var checkVar, withBindings, elseExpr)
                            (ANF.Let (tagVar, tagExpr, ANF.Let (checkVar, checkExpr, ifExpr)), vg6))
                    | AST.PWildcard ->
                        toANF body vg5' currentEnv typeReg variantLookup funcReg moduleRegistry
                        |> Result.map (fun (bodyExpr, vg6) ->
                            let withBindings = wrapBindings bindings bodyExpr
                            let ifExpr = ANF.If (ANF.Var checkVar, withBindings, elseExpr)
                            (ANF.Let (tagVar, tagExpr, ANF.Let (checkVar, checkExpr, ifExpr)), vg6))
                    | AST.PTuple innerPatterns ->
                        extractTupleBindings innerPatterns valueAtom elemType 0 currentEnv bindings vg5'  // Pass tuple type
                        |> Result.bind (fun (newEnv, newBindings, vg6) ->
                            toANF body vg6 newEnv typeReg variantLookup funcReg moduleRegistry
                            |> Result.map (fun (bodyExpr, vg7) ->
                                let withBindings = wrapBindings newBindings bodyExpr
                                let ifExpr = ANF.If (ANF.Var checkVar, withBindings, elseExpr)
                                (ANF.Let (tagVar, tagExpr, ANF.Let (checkVar, checkExpr, ifExpr)), vg7)))
                    | AST.PInt64 n -> compileLiteralPattern (ANF.Int64 n)
                    | AST.PInt8Literal n -> compileLiteralPattern (ANF.Int8 n)
                    | AST.PInt16Literal n -> compileLiteralPattern (ANF.Int16 n)
                    | AST.PInt32Literal n -> compileLiteralPattern (ANF.Int32 n)
                    | AST.PUInt8Literal n -> compileLiteralPattern (ANF.UInt8 n)
                    | AST.PUInt16Literal n -> compileLiteralPattern (ANF.UInt16 n)
                    | AST.PUInt32Literal n -> compileLiteralPattern (ANF.UInt32 n)
                    | AST.PUInt64Literal n -> compileLiteralPattern (ANF.UInt64 n)
                    | AST.PConstructor _ | AST.PList _ | AST.PListCons _ ->
                        Error "Nested pattern in list element not yet supported"
                    | _ ->
                        Error $"Unsupported pattern in single-element list: {pat}"
                else
                    // Multiple elements: check length == patternLen (safe for all list types)
                    let (lengthVar, vg1) = ANF.freshVar vg
                    let lengthName =
                        match elemType with
                        | AST.TFloat64 -> "Stdlib.__FingerTree.__lengthFloat"
                        | _ -> "Stdlib.__FingerTree.length_i64"
                    let lengthExpr = ANF.Call (lengthName, [listAtom])
                    let (checkVar, vg2) = ANF.freshVar vg1
                    let checkExpr = ANF.Prim (ANF.Eq, ANF.Var lengthVar, ANF.IntLiteral (ANF.Int64 (int64 patternLen)))
                    // Untag to get pointer (only used in then-branch after length check passes)
                    let (ptrVar, vg3) = ANF.freshVar vg2
                    let ptrExpr = ANF.Prim (ANF.BitAnd, listAtom, ANF.IntLiteral (ANF.Int64 0xFFFFFFFFFFFFFFF8L))

                    // Note: lengthExpr and checkExpr are safe (length handles EMPTY)
                    // ptrExpr just does bitwise and, doesn't dereference
                    let headerBindings = [(lengthVar, lengthExpr); (checkVar, checkExpr); (ptrVar, ptrExpr)]
                    let vg6 = vg3  // Keep consistent naming for the rest of the code

                    // Extract elements using getAt (handles varying prefix/suffix layouts)
                    // Returns: (env, bindings, literalChecks, vg) where literalChecks are (valueVar, expectedLit) pairs
                    let rec extractElements (pats: AST.Pattern list) (idx: int) (env: VarEnv) (bindings: (ANF.TempId * ANF.CExpr) list) (litChecks: (ANF.TempId * ANF.SizedInt) list) (vg: ANF.VarGen) : Result<VarEnv * (ANF.TempId * ANF.CExpr) list * (ANF.TempId * ANF.SizedInt) list * ANF.VarGen, string> =
                        match pats with
                        | [] -> Ok (env, bindings, litChecks, vg)
                        | pat :: rest ->
                            // Use getAt to retrieve element at this index
                            // getAt returns Option, but we know length == patternLen so it's always Some
                            // Select a type-specific wrapper to avoid defaulting to Int64 for floats.
                            // Monomorphization happens at AST level, so we must use a non-generic wrapper here.
                            let (optVar, vg1) = ANF.freshVar vg
                            let getAtName =
                                match elemType with
                                | AST.TFloat64 -> "Stdlib.List.__getAtFloat"
                                | _ -> "Stdlib.List.__getAtInt64"
                            let getAtExpr = ANF.Call (getAtName, [listAtom; ANF.IntLiteral (ANF.Int64 (int64 idx))])
                            // Unwrap the Some - getAt returns tagged value with tag 1 for Some
                            let (rawValueVar, vg2) = ANF.freshVar vg1
                            let valueType =
                                match elemType with
                                | AST.TFloat64 -> Some AST.TFloat64
                                | _ -> None
                            let rawValueExpr = ANF.RawGet (ANF.Var optVar, ANF.IntLiteral (ANF.Int64 8L), valueType)  // Some payload at offset 8
                            // Wrap with TypedAtom to preserve element type in TypeMap
                            let (typedValueVar, vg2') = ANF.freshVar vg2
                            let typedValueExpr = ANF.TypedAtom (ANF.Var rawValueVar, elemType)
                            let newBindings = bindings @ [(optVar, getAtExpr); (rawValueVar, rawValueExpr); (typedValueVar, typedValueExpr)]
                            let valueVar = typedValueVar

                            match pat with
                            | AST.PVar name ->
                                let newEnv = Map.add name (valueVar, elemType) env  // Use element type
                                extractElements rest (idx + 1) newEnv newBindings litChecks vg2'
                            | AST.PWildcard ->
                                extractElements rest (idx + 1) env newBindings litChecks vg2'
                            | AST.PTuple innerPatterns ->
                                extractTupleBindings innerPatterns (ANF.Var valueVar) elemType 0 env newBindings vg2'  // Pass tuple type
                                |> Result.bind (fun (tupEnv, tupBindings, vg3) ->
                                    extractElements rest (idx + 1) tupEnv tupBindings litChecks vg3)
                            | (AST.PInt64 _ as pat)
                            | (AST.PInt8Literal _ as pat)
                            | (AST.PInt16Literal _ as pat)
                            | (AST.PInt32Literal _ as pat)
                            | (AST.PUInt8Literal _ as pat)
                            | (AST.PUInt16Literal _ as pat)
                            | (AST.PUInt32Literal _ as pat)
                            | (AST.PUInt64Literal _ as pat) ->
                                // Track this literal check for later
                                let literal =
                                    match patternLiteralToSizedInt pat with
                                    | Some value -> value
                                    | None -> Crash.crash $"Expected integer literal pattern, got {pat}"
                                extractElements rest (idx + 1) env newBindings ((valueVar, literal) :: litChecks) vg2'
                            | _ ->
                                Error $"Unsupported pattern in list element: {pat}"

                    extractElements patterns 0 currentEnv [] [] vg6
                    |> Result.bind (fun (newEnv, elemBindings, litChecks, vg7) ->
                        toANF body vg7 newEnv typeReg variantLookup funcReg moduleRegistry
                        |> Result.map (fun (bodyExpr, vg8) ->
                            // Build the inner expression based on whether we have literal checks
                            let (innerExpr, vg9) =
                                match litChecks with
                                | [] ->
                                    // No literals - just return body
                                    (bodyExpr, vg8)
                                | checks ->
                                    // Build literal checks - AND them all together
                                    // Note: We don't AND with checkVar since we already checked length
                                    let rec buildLitOnlyChecks (remaining: (ANF.TempId * ANF.SizedInt) list) (accBindings: (ANF.TempId * ANF.CExpr) list) (prevCondVar: ANF.TempId option) (vg: ANF.VarGen) : (ANF.TempId * (ANF.TempId * ANF.CExpr) list * ANF.VarGen) =
                                        match remaining with
                                        | [] ->
                                            (Option.get prevCondVar, accBindings, vg)
                                        | (valueVar, litVal) :: rest ->
                                            let (litCheckVar, vg1) = ANF.freshVar vg
                                            let litCheckExpr = ANF.Prim (ANF.Eq, ANF.Var valueVar, ANF.IntLiteral litVal)
                                            match prevCondVar with
                                            | None ->
                                                // First check - use directly
                                                buildLitOnlyChecks rest (accBindings @ [(litCheckVar, litCheckExpr)]) (Some litCheckVar) vg1
                                            | Some prevVar ->
                                                // AND with previous check
                                                let (combinedVar, vg2) = ANF.freshVar vg1
                                                let combinedExpr = ANF.Prim (ANF.And, ANF.Var prevVar, ANF.Var litCheckVar)
                                                buildLitOnlyChecks rest (accBindings @ [(litCheckVar, litCheckExpr); (combinedVar, combinedExpr)]) (Some combinedVar) vg2
                                    let (litCondVar, litBindings, vg9') = buildLitOnlyChecks (List.rev checks) [] None vg8
                                    let litCheckedBody = ANF.If (ANF.Var litCondVar, bodyExpr, elseExpr)
                                    let withLitBindings = wrapBindings litBindings litCheckedBody
                                    (withLitBindings, vg9')
                            // Wrap with element bindings (inside length check)
                            let withElemBindings = wrapBindings elemBindings innerExpr
                            // Wrap with length check
                            let ifExpr = ANF.If (ANF.Var checkVar, withElemBindings, elseExpr)
                            let withHeader = wrapBindings headerBindings ifExpr
                            (withHeader, vg9)))

            // Compile a list cons pattern [h, ...t] for FingerTree
            // This pattern extracts head element(s) and binds the rest to tail
            // For FingerTree:
            // - SINGLE (tag 1): head is the element, tail is EMPTY
            // - DEEP (tag 2): head is prefix[0], tail requires calling FingerTree.tail
            // listType is the list type (TList elemType) for correct pattern variable typing
            let rec compileListConsPatternWithChecks
                (headPatterns: AST.Pattern list)
                (tailPattern: AST.Pattern)
                (listAtom: ANF.Atom)
                (listType: AST.Type)
                (currentEnv: VarEnv)
                (body: AST.Expr)
                (elseExpr: ANF.AExpr)
                (vg: ANF.VarGen)
                : Result<ANF.AExpr * ANF.VarGen, string> =

                // Extract element type from list type
                let elemType =
                    match listType with
                    | AST.TList t -> t
                    | _ -> AST.TInt64  // Fallback

                // Use _i64 versions which work for any element type at runtime (all values are 64-bit)
                // The correct element type is tracked in the VarEnv/TypeMap, not in the function name

                // Helper to unwrap a LEAF node and get the value
                let unwrapLeaf (leafTaggedPtr: ANF.Atom) (vg: ANF.VarGen) (bindings: (ANF.TempId * ANF.CExpr) list) =
                    let (leafPtrVar, vg1) = ANF.freshVar vg
                    let leafPtrExpr = ANF.Prim (ANF.BitAnd, leafTaggedPtr, ANF.IntLiteral (ANF.Int64 0xFFFFFFFFFFFFFFF8L))
                    let (valueVar, vg2) = ANF.freshVar vg1
                    let valueExpr = ANF.RawGet (ANF.Var leafPtrVar, ANF.IntLiteral (ANF.Int64 0L), None)
                    let newBindings = bindings @ [(leafPtrVar, leafPtrExpr); (valueVar, valueExpr)]
                    (ANF.Var valueVar, valueVar, newBindings, vg2)

                // Helper to extract tuple elements
                // tupleType is the type of the tuple being matched (TTuple elemTypes)
                let rec extractTupleBindings
                    (tupPats: AST.Pattern list)
                    (tupleAtom: ANF.Atom)
                    (tupleType: AST.Type)
                    (idx: int)
                    (env: VarEnv)
                    (bindings: (ANF.TempId * ANF.CExpr) list)
                    (vg: ANF.VarGen)
                    : Result<VarEnv * (ANF.TempId * ANF.CExpr) list * ANF.VarGen, string> =
                    // Extract element types from tuple type
                    let elemTypes =
                        match tupleType with
                        | AST.TTuple types -> types
                        | _ -> List.replicate (List.length tupPats) AST.TInt64
                    match tupPats with
                    | [] -> Ok (env, bindings, vg)
                    | tupPat :: tupRest ->
                        let (rawElemVar, vg1) = ANF.freshVar vg
                        let rawElemExpr = ANF.TupleGet (tupleAtom, idx)
                        let rawElemBinding = (rawElemVar, rawElemExpr)
                        let elemT = if idx < List.length elemTypes then List.item idx elemTypes else AST.TInt64
                        // Wrap with TypedAtom to preserve correct element type in TypeMap
                        let (elemVar, vg1') = ANF.freshVar vg1
                        let elemExpr = ANF.TypedAtom (ANF.Var rawElemVar, elemT)
                        let elemBinding = (elemVar, elemExpr)
                        match tupPat with
                        | AST.PVar name ->
                            let newEnv = Map.add name (elemVar, elemT) env
                            extractTupleBindings tupRest tupleAtom tupleType (idx + 1) newEnv (bindings @ [rawElemBinding; elemBinding]) vg1'
                        | AST.PWildcard ->
                            // Even for wildcard, we need to extract the element (for proper tuple access)
                            // but don't bind it to a name. Just add the raw binding and continue.
                            extractTupleBindings tupRest tupleAtom tupleType (idx + 1) env (bindings @ [rawElemBinding]) vg1
                        | _ ->
                            Error $"Nested pattern in tuple element not yet supported: {tupPat}"

                match headPatterns with
                | [] ->
                    // All head elements extracted - bind tail and compile body
                    match tailPattern with
                    | AST.PVar name ->
                        let (tailVar, vg1) = ANF.freshVar vg
                        let newEnv = Map.add name (tailVar, listType) currentEnv  // Use actual list type
                        toANF body vg1 newEnv typeReg variantLookup funcReg moduleRegistry
                        |> Result.map (fun (bodyExpr, vg2) ->
                            let withTail = ANF.Let (tailVar, ANF.Atom listAtom, bodyExpr)
                            (withTail, vg2))
                    | AST.PWildcard ->
                        toANF body vg currentEnv typeReg variantLookup funcReg moduleRegistry
                    | _ -> Error "Tail pattern in list cons must be variable or wildcard"

                | [singleHeadPattern] ->
                    // Single head pattern [h, ...t] - most common case
                    // Use branching based on tag to handle SINGLE vs DEEP nodes

                    // Check list is not empty
                    let (notEmptyVar, vg1) = ANF.freshVar vg
                    let notEmptyExpr = ANF.Prim (ANF.Neq, listAtom, ANF.IntLiteral (ANF.Int64 0L))

                    // Get tag
                    let (tagVar, vg2) = ANF.freshVar vg1
                    let tagExpr = ANF.Prim (ANF.BitAnd, listAtom, ANF.IntLiteral (ANF.Int64 7L))

                    // Untag to get pointer
                    let (ptrVar, vg3) = ANF.freshVar vg2
                    let ptrExpr = ANF.Prim (ANF.BitAnd, listAtom, ANF.IntLiteral (ANF.Int64 0xFFFFFFFFFFFFFFF8L))

                    // Check if SINGLE (tag 1)
                    let (isSingleVar, vg4) = ANF.freshVar vg3
                    let isSingleExpr = ANF.Prim (ANF.Eq, ANF.Var tagVar, ANF.IntLiteral (ANF.Int64 1L))

                    // notEmptyVar must be bound OUTSIDE the If since it's used as the condition
                    let condBindings = [(notEmptyVar, notEmptyExpr)]
                    let innerBindings = [(tagVar, tagExpr); (ptrVar, ptrExpr); (isSingleVar, isSingleExpr)]

                    // Compile the SINGLE branch: node at offset 0, tail = EMPTY
                    let compileSingleBranch vg =
                        let (singleNodeVar, vg1) = ANF.freshVar vg
                        let singleNodeExpr = ANF.RawGet (ANF.Var ptrVar, ANF.IntLiteral (ANF.Int64 0L), None)
                        let (headAtom, headVar, headBindings, vg2) = unwrapLeaf (ANF.Var singleNodeVar) vg1 [(singleNodeVar, singleNodeExpr)]
                        // Wrap headVar with TypedAtom to preserve correct element type in TypeMap
                        let (typedHeadVar, vg2') = ANF.freshVar vg2
                        let typedHeadExpr = ANF.TypedAtom (ANF.Var headVar, elemType)
                        let typedHeadBinding = (typedHeadVar, typedHeadExpr)
                        let headBindingsWithType = headBindings @ [typedHeadBinding]
                        let typedHeadAtom = ANF.Var typedHeadVar
                        // Tail is empty list (0 = EMPTY sentinel) - wrap with TypedAtom to preserve list type
                        let (rawTailVar, vg3) = ANF.freshVar vg2'
                        let rawTailExpr = ANF.Atom (ANF.IntLiteral (ANF.Int64 0L))  // EMPTY
                        let (tailVar, vg3') = ANF.freshVar vg3
                        let tailExpr = ANF.TypedAtom (ANF.Var rawTailVar, listType)

                        // Bind head pattern - returns (env, tupleBindings, vg, guardOpt)
                        // guardOpt is Some(var, expr) for literal patterns that need comparison
                        let headEnvResult =
                            match singleHeadPattern with
                            | AST.PVar name -> Ok (Map.add name (typedHeadVar, elemType) currentEnv, [], vg3', None)  // Use typed head var with element type
                            | AST.PWildcard -> Ok (currentEnv, [], vg3', None)
                            | AST.PTuple innerPatterns ->
                                extractTupleBindings innerPatterns typedHeadAtom elemType 0 currentEnv [] vg3'  // Pass tuple type
                                |> Result.map (fun (env, bindings, vg') -> (env, bindings, vg', None))
                            | (AST.PInt64 _ as pat)
                            | (AST.PInt8Literal _ as pat)
                            | (AST.PInt16Literal _ as pat)
                            | (AST.PInt32Literal _ as pat)
                            | (AST.PUInt8Literal _ as pat)
                            | (AST.PUInt16Literal _ as pat)
                            | (AST.PUInt32Literal _ as pat)
                            | (AST.PUInt64Literal _ as pat) ->
                                // Compare head value to literal - guard check
                                let (guardVar, vg4) = ANF.freshVar vg3'
                                let literal =
                                    match patternLiteralToSizedInt pat with
                                    | Some value -> value
                                    | None -> Crash.crash $"Expected integer literal pattern, got {pat}"
                                let guardExpr = ANF.Prim (ANF.Eq, ANF.Var typedHeadVar, ANF.IntLiteral literal)
                                Ok (currentEnv, [], vg4, Some (guardVar, guardExpr))
                            | AST.PConstructor _ ->
                                Error "Nested pattern in list cons element not yet supported"
                            | _ -> Error $"Unsupported head pattern in list cons: {singleHeadPattern}"

                        headEnvResult
                        |> Result.bind (fun (envWithHead, tupleBindings, vg4, guardOpt) ->
                            let tailEnvResult =
                                match tailPattern with
                                | AST.PVar name -> Ok (Map.add name (tailVar, listType) envWithHead, vg4)  // Use actual list type
                                | AST.PWildcard -> Ok (envWithHead, vg4)
                                | _ -> Error "Tail pattern must be variable or wildcard"

                            tailEnvResult
                            |> Result.bind (fun (finalEnv, vg5) ->
                                toANF body vg5 finalEnv typeReg variantLookup funcReg moduleRegistry
                                |> Result.map (fun (bodyExpr, vg6) ->
                                    let withTupleBindings = wrapBindings tupleBindings bodyExpr
                                    let withTypedTail = ANF.Let (tailVar, tailExpr, withTupleBindings)
                                    let withTail = ANF.Let (rawTailVar, rawTailExpr, withTypedTail)
                                    // If there's a guard (literal pattern), add check AFTER head bindings
                                    // because guardExpr uses headVar which is defined in headBindings
                                    let withGuard =
                                        match guardOpt with
                                        | Some (guardVar, guardExpr) ->
                                            // headBindingsWithType -> guardVar -> if guard then body else elseExpr
                                            let ifGuard = ANF.If (ANF.Var guardVar, withTail, elseExpr)
                                            let withGuardBinding = ANF.Let (guardVar, guardExpr, ifGuard)
                                            wrapBindings headBindingsWithType withGuardBinding
                                        | None -> wrapBindings headBindingsWithType withTail
                                    (withGuard, vg6))))

                    // Compile the DEEP branch: node at offset 16 (prefix[0])
                    // For tail, call Stdlib.__FingerTree.tail to properly compute the tail
                    let compileDeepBranch vg =
                        let (deepNodeVar, vg1) = ANF.freshVar vg
                        let deepNodeExpr = ANF.RawGet (ANF.Var ptrVar, ANF.IntLiteral (ANF.Int64 16L), None)
                        let (headAtom, headVar, headBindings, vg2) = unwrapLeaf (ANF.Var deepNodeVar) vg1 [(deepNodeVar, deepNodeExpr)]
                        // Wrap headVar with TypedAtom to preserve correct element type in TypeMap
                        let (typedHeadVar, vg2') = ANF.freshVar vg2
                        let typedHeadExpr = ANF.TypedAtom (ANF.Var headVar, elemType)
                        let typedHeadBinding = (typedHeadVar, typedHeadExpr)
                        let headBindingsWithType = headBindings @ [typedHeadBinding]
                        let typedHeadAtom = ANF.Var typedHeadVar

                        // Call Stdlib.__FingerTree.tail to get the tail
                        let (tailResultVar, vg3) = ANF.freshVar vg2'
                        let tailCallExpr = ANF.Call ("Stdlib.__FingerTree.tail_i64", [listAtom])
                        // Wrap with TypedAtom to preserve correct list type in TypeMap
                        let (typedTailVar, vg3') = ANF.freshVar vg3
                        let typedTailExpr = ANF.TypedAtom (ANF.Var tailResultVar, listType)

                        let tailBindings = [(tailResultVar, tailCallExpr); (typedTailVar, typedTailExpr)]

                        // Bind head pattern - returns (env, tupleBindings, vg, guardOpt)
                        let headEnvResult =
                            match singleHeadPattern with
                            | AST.PVar name -> Ok (Map.add name (typedHeadVar, elemType) currentEnv, [], vg3', None)  // Use typed head var with element type
                            | AST.PWildcard -> Ok (currentEnv, [], vg3', None)
                            | AST.PTuple innerPatterns ->
                                extractTupleBindings innerPatterns typedHeadAtom elemType 0 currentEnv [] vg3'  // Pass tuple type
                                |> Result.map (fun (env, bindings, vg') -> (env, bindings, vg', None))
                            | (AST.PInt64 _ as pat)
                            | (AST.PInt8Literal _ as pat)
                            | (AST.PInt16Literal _ as pat)
                            | (AST.PInt32Literal _ as pat)
                            | (AST.PUInt8Literal _ as pat)
                            | (AST.PUInt16Literal _ as pat)
                            | (AST.PUInt32Literal _ as pat)
                            | (AST.PUInt64Literal _ as pat) ->
                                // Compare head value to literal - guard check
                                let (guardVar, vg4) = ANF.freshVar vg3'
                                let literal =
                                    match patternLiteralToSizedInt pat with
                                    | Some value -> value
                                    | None -> Crash.crash $"Expected integer literal pattern, got {pat}"
                                let guardExpr = ANF.Prim (ANF.Eq, ANF.Var typedHeadVar, ANF.IntLiteral literal)
                                Ok (currentEnv, [], vg4, Some (guardVar, guardExpr))
                            | AST.PConstructor _ ->
                                Error "Nested pattern in list cons element not yet supported"
                            | _ -> Error $"Unsupported head pattern in list cons: {singleHeadPattern}"

                        headEnvResult
                        |> Result.bind (fun (envWithHead, tupleBindings, vg4, guardOpt) ->
                            let tailEnvResult =
                                match tailPattern with
                                | AST.PVar name -> Ok (Map.add name (typedTailVar, listType) envWithHead, vg4)  // Use typed tail var with correct list type
                                | AST.PWildcard -> Ok (envWithHead, vg4)
                                | _ -> Error "Tail pattern must be variable or wildcard"

                            tailEnvResult
                            |> Result.bind (fun (finalEnv, vg5) ->
                                toANF body vg5 finalEnv typeReg variantLookup funcReg moduleRegistry
                                |> Result.map (fun (bodyExpr, vg6) ->
                                    let withTupleBindings = wrapBindings tupleBindings bodyExpr
                                    let withTailBinding = wrapBindings tailBindings withTupleBindings
                                    // If there's a guard (literal pattern), add check AFTER head bindings
                                    // because guardExpr uses headVar which is defined in headBindingsWithType
                                    let withGuard =
                                        match guardOpt with
                                        | Some (guardVar, guardExpr) ->
                                            // headBindingsWithType -> guardVar -> if guard then body else elseExpr
                                            let ifGuard = ANF.If (ANF.Var guardVar, withTailBinding, elseExpr)
                                            let withGuardBinding = ANF.Let (guardVar, guardExpr, ifGuard)
                                            wrapBindings headBindingsWithType withGuardBinding
                                        | None -> wrapBindings headBindingsWithType withTailBinding
                                    (withGuard, vg6))))

                    // Build the combined expression with branching
                    compileSingleBranch vg4
                    |> Result.bind (fun (singleBranchExpr, vg5) ->
                        compileDeepBranch vg5
                        |> Result.map (fun (deepBranchExpr, vg6) ->
                            // If SINGLE then singleBranch else deepBranch
                            let tagBranchExpr = ANF.If (ANF.Var isSingleVar, singleBranchExpr, deepBranchExpr)
                            // Wrap inner bindings (tag, ptr, isSingle) around the tag branch
                            let withInnerBindings = wrapBindings innerBindings tagBranchExpr
                            // If not empty then execute inner bindings + branch else elseExpr
                            let ifExpr = ANF.If (ANF.Var notEmptyVar, withInnerBindings, elseExpr)
                            // Bind notEmptyVar BEFORE the If
                            let finalExpr = wrapBindings condBindings ifExpr
                            (finalExpr, vg6)))

                | _ ->
                    // Multiple head patterns [a, b, ...t]
                    // Check length >= number of head patterns
                    let numHeads = List.length headPatterns
                    let (lengthVar, vg1) = ANF.freshVar vg
                    let lengthName =
                        match elemType with
                        | AST.TFloat64 -> "Stdlib.__FingerTree.__lengthFloat"
                        | _ -> "Stdlib.__FingerTree.length_i64"
                    let lengthExpr = ANF.Call (lengthName, [listAtom])
                    let (lengthCheckVar, vg2) = ANF.freshVar vg1
                    let lengthCheckExpr = ANF.Prim (ANF.Gte, ANF.Var lengthVar, ANF.IntLiteral (ANF.Int64 (int64 numHeads)))

                    // Extract head elements and final tail using head/tail calls
                    // Use _i64 versions which work for any element type at runtime (all values are 64-bit)
                    // The correct element type is tracked in the VarEnv/TypeMap, not in the function name
                    let rec extractElements (pats: AST.Pattern list) (currentListVar: ANF.TempId) (env: VarEnv) (bindings: (ANF.TempId * ANF.CExpr) list) (vg: ANF.VarGen) : Result<VarEnv * (ANF.TempId * ANF.CExpr) list * ANF.TempId * ANF.VarGen, string> =
                        match pats with
                        | [] ->
                            // No more head patterns, currentListVar is the tail
                            Ok (env, bindings, currentListVar, vg)
                        | pat :: rest ->
                            // Call head to get current element
                            let (headResultVar, vg1) = ANF.freshVar vg
                            let headCallExpr = ANF.Call ("Stdlib.__FingerTree.headUnsafe_i64", [ANF.Var currentListVar])
                            // Call tail to get rest
                            let (tailResultVar, vg2) = ANF.freshVar vg1
                            let tailCallExpr = ANF.Call ("Stdlib.__FingerTree.tail_i64", [ANF.Var currentListVar])
                            let newBindings = bindings @ [(headResultVar, headCallExpr); (tailResultVar, tailCallExpr)]

                            match pat with
                            | AST.PVar name ->
                                let newEnv = Map.add name (headResultVar, elemType) env  // Use element type
                                extractElements rest tailResultVar newEnv newBindings vg2
                            | AST.PWildcard ->
                                extractElements rest tailResultVar env newBindings vg2
                            | AST.PInt64 _
                            | AST.PInt8Literal _
                            | AST.PInt16Literal _
                            | AST.PInt32Literal _
                            | AST.PUInt8Literal _
                            | AST.PUInt16Literal _
                            | AST.PUInt32Literal _
                            | AST.PUInt64Literal _ ->
                                Error "Nested pattern in list cons element not yet supported"
                            | AST.PConstructor _ ->
                                Error "Nested pattern in list cons element not yet supported"
                            | _ ->
                                Error $"Unsupported head pattern in multi-element list cons: {pat}"

                    // Get initial list variable
                    let (initialListVar, vg3) = ANF.freshVar vg2
                    let initialListExpr = ANF.Atom listAtom

                    extractElements headPatterns initialListVar currentEnv [(initialListVar, initialListExpr)] vg3
                    |> Result.bind (fun (envAfterHeads, headBindings, finalTailVar, vg4) ->
                        // Bind tail pattern
                        let tailEnvResult =
                            match tailPattern with
                            | AST.PVar name -> Ok (Map.add name (finalTailVar, listType) envAfterHeads, vg4)  // Use actual list type
                            | AST.PWildcard -> Ok (envAfterHeads, vg4)
                            | _ -> Error "Tail pattern must be variable or wildcard"

                        tailEnvResult
                        |> Result.bind (fun (finalEnv, vg5) ->
                            toANF body vg5 finalEnv typeReg variantLookup funcReg moduleRegistry
                            |> Result.map (fun (bodyExpr, vg6) ->
                                // Wrap bindings around body
                                let withHeadBindings = wrapBindings headBindings bodyExpr
                                // Check length condition
                                let ifExpr = ANF.If (ANF.Var lengthCheckVar, withHeadBindings, elseExpr)
                                let withLengthCheck = ANF.Let (lengthCheckVar, lengthCheckExpr, ifExpr)
                                let finalExpr = ANF.Let (lengthVar, lengthExpr, withLengthCheck)
                                (finalExpr, vg6))))

            // Build OR of multiple pattern conditions for pattern grouping
            // Returns: combined condition atom, all bindings, updated vargen
            let buildPatternGroupComparison (patterns: AST.Pattern list) (scrutAtom: ANF.Atom) (vg: ANF.VarGen) : Result<(ANF.Atom * (ANF.TempId * ANF.CExpr) list * ANF.VarGen) option, string> =
                match patterns with
                | [] -> Ok None
                | [single] -> buildPatternComparison single scrutAtom vg
                | multiple ->
                    // Build comparison for each pattern, then OR them together
                    let rec buildOr (pats: AST.Pattern list) (accCondOpt: ANF.Atom option) (accBindings: (ANF.TempId * ANF.CExpr) list) (vg: ANF.VarGen) : Result<(ANF.Atom * (ANF.TempId * ANF.CExpr) list * ANF.VarGen) option, string> =
                        match pats with
                        | [] ->
                            match accCondOpt with
                            | None -> Ok None
                            | Some cond -> Ok (Some (cond, accBindings, vg))
                        | pat :: rest ->
                            buildPatternComparison pat scrutAtom vg
                            |> Result.bind (fun cmpOpt ->
                                match cmpOpt with
                                | None ->
                                    // Pattern always matches (wildcard/var) - the whole group always matches
                                    Ok None
                                | Some (condAtom, bindings, vg1) ->
                                    let (newCondOpt, newBindings, vg2) =
                                        match accCondOpt with
                                        | None ->
                                            // First condition
                                            (Some condAtom, bindings @ accBindings, vg1)
                                        | Some accCond ->
                                            // OR with previous conditions
                                            // Put bindings in dependency order: comparison bindings first, OR at end
                                            // (foldBack makes first binding outermost, so dependencies must come first)
                                            let (orVar, vg') = ANF.freshVar vg1
                                            let orExpr = ANF.Prim (ANF.Or, accCond, condAtom)
                                            (Some (ANF.Var orVar), accBindings @ bindings @ [(orVar, orExpr)], vg')
                                    buildOr rest newCondOpt newBindings vg2)
                    buildOr multiple None [] vg

            // Build the if-else chain from cases
            let rec buildChain (remaining: AST.MatchCase list) (vg: ANF.VarGen) : Result<ANF.AExpr * ANF.VarGen, string> =
                match remaining with
                | [] ->
                    // No cases left - shouldn't happen if we have wildcard/var
                    Error "Non-exhaustive pattern match"
                | [mc] ->
                    // Last case - for most patterns just compile the body
                    // For now, use first pattern (pattern grouping not yet fully supported)
                    let pattern = AST.NonEmptyList.head mc.Patterns
                    let body = mc.Body
                    // For non-empty list patterns, we still need proper length checking
                    match pattern with
                    | AST.PList (_ :: _ as listPatterns) ->
                        // Explicit list pattern like [a, b] as the only/last case with no wildcard
                        // This is non-exhaustive - reject at compile time
                        Error "Non-exhaustive pattern match: list pattern requires a catch-all case"
                    | AST.PListCons (headPatterns, tailPattern) ->
                        // List cons pattern as last case
                        // Fallback should crash at runtime since it's unreachable code
                        // Dereference null pointer (address 0) to trigger SIGSEGV
                        let (crashVar, vg') = ANF.freshVar vg
                        let crashExpr = ANF.RawGet (ANF.IntLiteral (ANF.Int64 0L), ANF.IntLiteral (ANF.Int64 0L), None)
                        let fallbackExpr = ANF.Let (crashVar, crashExpr, ANF.Return (ANF.Var crashVar))
                        compileListConsPatternWithChecks headPatterns tailPattern scrutineeAtom' scrutType env body fallbackExpr vg'
                    | _ ->
                        // Other patterns - original behavior
                        // Handle guard if present
                        match mc.Guard with
                        | None ->
                            extractAndCompileBody pattern body scrutineeAtom' scrutType env vg
                        | Some guardExpr ->
                            // With guard: compile pattern match with guard check
                            // Fallback should crash at runtime since it's unreachable code
                            // Dereference null pointer (address 0) to trigger SIGSEGV
                            let (crashVar, vg') = ANF.freshVar vg
                            let crashExpr = ANF.RawGet (ANF.IntLiteral (ANF.Int64 0L), ANF.IntLiteral (ANF.Int64 0L), None)
                            let fallbackExpr = ANF.Let (crashVar, crashExpr, ANF.Return (ANF.Var crashVar))
                            extractAndCompileBodyWithGuard pattern guardExpr body scrutineeAtom' scrutType env vg' fallbackExpr
                | mc :: rest ->
                    // For pattern grouping, use first pattern for bindings but OR all patterns for comparison
                    let firstPattern = AST.NonEmptyList.head mc.Patterns
                    let body = mc.Body
                    if patternAlwaysMatches firstPattern then
                        // Wildcard or var - matches everything, but may still need guard
                        match mc.Guard with
                        | None ->
                            extractAndCompileBody firstPattern body scrutineeAtom' scrutType env vg
                        | Some guardExpr ->
                            // Wildcard with guard - still need to check guard, fall through if false
                            buildChain rest vg
                            |> Result.bind (fun (elseExpr, vg1) ->
                                extractAndCompileBodyWithGuard firstPattern guardExpr body scrutineeAtom' scrutType env vg1 elseExpr)
                    else
                        // Non-empty list patterns need special handling with interleaved checks
                        match firstPattern with
                        | AST.PList (_ :: _ as listPatterns) ->
                            // Build the else branch first (rest of cases)
                            buildChain rest vg
                            |> Result.bind (fun (elseExpr, vg1) ->
                                // Use the new interleaved check-and-extract function
                                compileListPatternWithChecks listPatterns scrutineeAtom' scrutType env body elseExpr vg1)
                        | AST.PListCons (headPatterns, tailPattern) ->
                            // List cons pattern - needs interleaved checks
                            buildChain rest vg
                            |> Result.bind (fun (elseExpr, vg1) ->
                                compileListConsPatternWithChecks headPatterns tailPattern scrutineeAtom' scrutType env body elseExpr vg1)
                        | _ ->
                            // Use pattern grouping: OR all patterns in the group
                            buildPatternGroupComparison (AST.NonEmptyList.toList mc.Patterns) scrutineeAtom' vg
                            |> Result.bind (fun cmpOpt ->
                                match cmpOpt with
                                | None ->
                                    // Pattern always matches
                                    match mc.Guard with
                                    | None ->
                                        extractAndCompileBody firstPattern body scrutineeAtom' scrutType env vg
                                    | Some guardExpr ->
                                        buildChain rest vg
                                        |> Result.bind (fun (elseExpr, vg1) ->
                                            extractAndCompileBodyWithGuard firstPattern guardExpr body scrutineeAtom' scrutType env vg1 elseExpr)
                                | Some (condAtom, bindings, vg1) ->
                                    match mc.Guard with
                                    | None ->
                                        extractAndCompileBody firstPattern body scrutineeAtom' scrutType env vg1
                                        |> Result.bind (fun (thenExpr, vg2) ->
                                            buildChain rest vg2
                                            |> Result.map (fun (elseExpr, vg3) ->
                                                let ifExpr = ANF.If (condAtom, thenExpr, elseExpr)
                                                let finalExpr = wrapBindings bindings ifExpr
                                                (finalExpr, vg3)))
                                    | Some guardExpr ->
                                        // Pattern match + guard: if pattern matches, bind, check guard
                                        buildChain rest vg1
                                        |> Result.bind (fun (elseExpr, vg2) ->
                                            extractAndCompileBodyWithGuard firstPattern guardExpr body scrutineeAtom' scrutType env vg2 elseExpr
                                            |> Result.map (fun (guardedBody, vg3) ->
                                                let ifExpr = ANF.If (condAtom, guardedBody, elseExpr)
                                                let finalExpr = wrapBindings bindings ifExpr
                                                (finalExpr, vg3))))

            buildChain cases varGen1'
            |> Result.map (fun (chainExpr, varGen2) ->
                let exprWithBindings = wrapBindings scrutineeBindings' chainExpr
                (exprWithBindings, varGen2)))

    | AST.InterpolatedString parts ->
        // Desugar interpolated string to StringConcat chain
        // $"Hello {name}!" → "Hello " ++ name ++ "!"
        let partToExpr (part: AST.StringPart) : AST.Expr =
            match part with
            | AST.StringText s -> AST.StringLiteral s
            | AST.StringExpr e -> e
        match parts with
        | [] ->
            // Empty interpolated string → empty string
            Ok (ANF.Return (ANF.StringLiteral ""), varGen)
        | [single] ->
            // Single part → convert directly
            toANF (partToExpr single) varGen env typeReg variantLookup funcReg moduleRegistry
        | first :: rest ->
            // Multiple parts → fold with StringConcat
            let desugared =
                rest
                |> List.fold (fun acc part ->
                    AST.BinOp (AST.StringConcat, acc, partToExpr part))
                    (partToExpr first)
            toANF desugared varGen env typeReg variantLookup funcReg moduleRegistry

    | AST.Lambda (_parameters, _body) ->
        // Lambda in expression position - closures not yet fully implemented
        Error "Lambda expressions (closures) are not yet fully implemented"

    | AST.Apply (func, args) ->
        // Apply a function expression to arguments
        // For now, only support immediate application of lambdas
        match func with
        | AST.Lambda (parameters, body) ->
            // Immediate application: ((x: int) => x + 1)(5) becomes let x = 5 in x + 1
            if List.length args <> List.length parameters then
                Error $"Lambda expects {List.length parameters} arguments, got {List.length args}"
            else
                // Build nested let bindings: let p1 = arg1 in let p2 = arg2 in ... body
                let rec buildLets (ps: (string * AST.Type) list) (as': AST.Expr list) : AST.Expr =
                    match ps, as' with
                    | [], [] -> body
                    | (pName, _) :: restPs, argExpr :: restAs ->
                        AST.Let (pName, argExpr, buildLets restPs restAs)
                    | _ -> body  // Should not happen due to length check
                let desugared = buildLets parameters args
                toANF desugared varGen env typeReg variantLookup funcReg moduleRegistry
        | AST.Var name ->
            // Calling a variable that might hold a closure
            match Map.tryFind name env with
            | Some (tempId, _) ->
                // Variable exists - treat as closure call
                let rec convertArgs (remaining: AST.Expr list) (vg: ANF.VarGen) (acc: (ANF.Atom * (ANF.TempId * ANF.CExpr) list) list) =
                    match remaining with
                    | [] -> Ok (List.rev acc, vg)
                    | arg :: rest ->
                        toAtom arg vg env typeReg variantLookup funcReg moduleRegistry
                        |> Result.bind (fun (argAtom, argBindings, vg') ->
                            convertArgs rest vg' ((argAtom, argBindings) :: acc))
                convertArgs args varGen []
                |> Result.bind (fun (argResults, varGen1) ->
                    let argAtoms = argResults |> List.map fst
                    let allBindings = argResults |> List.collect snd
                    // Generate closure call
                    let (resultId, varGen2) = ANF.freshVar varGen1
                    let closureCall = ANF.ClosureCall (ANF.Var tempId, argAtoms)
                    let finalBindings = allBindings @ [(resultId, closureCall)]
                    Ok (ANF.Return (ANF.Var resultId), varGen2)
                    |> Result.map (fun (expr, vg) ->
                        (wrapBindings finalBindings expr, vg)))
            | None ->
                Error $"Cannot apply variable '{name}' as function - variable not in scope"

        | AST.Apply (_, _) ->
            // Nested application: ((x) => (y) => ...)(a)(b)(c)...
            // Flatten all nested applies first, then desugar from innermost out
            let rec flattenApplies expr argLists =
                match expr with
                | AST.Apply (innerFunc, innerArgs) -> flattenApplies innerFunc (innerArgs :: argLists)
                | other -> (other, argLists)

            let (baseFunc, allArgLists) = flattenApplies func [args]
            // allArgLists is a list of arg lists, from innermost to outermost
            // e.g., for f(1)(2)(3), we get ([1], [2], [3])

            match baseFunc with
            | AST.Lambda _ ->
                // Desugar all nested lambda applications at once
                let rec desugaAll (currentFunc: AST.Expr) (remainingArgLists: AST.Expr list list) : AST.Expr =
                    match remainingArgLists with
                    | [] -> currentFunc
                    | currentArgs :: restArgLists ->
                        match currentFunc with
                        | AST.Lambda (lambdaParams, body) ->
                            if List.length currentArgs <> List.length lambdaParams then
                                // Will error later, just wrap in Apply for now
                                desugaAll (AST.Apply (currentFunc, currentArgs)) restArgLists
                            else
                                // Desugar: let p1 = a1 in let p2 = a2 in ... body
                                let rec buildLets (ps: (string * AST.Type) list) (as': AST.Expr list) : AST.Expr =
                                    match ps, as' with
                                    | [], [] -> body
                                    | (pName, _) :: restPs, argExpr :: restAs ->
                                        AST.Let (pName, argExpr, buildLets restPs restAs)
                                    | _ -> body
                                let desugared = buildLets lambdaParams currentArgs
                                desugaAll desugared restArgLists
                        | AST.Let (name, value, innerBody) ->
                            // Float let out: Apply(let x = v in body, args) → let x = v in Apply(body, args)
                            AST.Let (name, value, desugaAll innerBody (currentArgs :: restArgLists))
                        | _ ->
                            // Non-lambda function - wrap remaining in Apply
                            let applied = AST.Apply (currentFunc, currentArgs)
                            desugaAll applied restArgLists

                let desugared = desugaAll baseFunc allArgLists
                toANF desugared varGen env typeReg variantLookup funcReg moduleRegistry

            | _ ->
                // Base function is not a lambda - use toAtom which handles nested applies
                // Reconstruct the full nested apply, then delegate to toAtom
                let rec applyAll (currentExpr: AST.Expr) (remainingArgLists: AST.Expr list list) : AST.Expr =
                    match remainingArgLists with
                    | [] -> currentExpr
                    | currentArgs :: rest -> applyAll (AST.Apply (currentExpr, currentArgs)) rest
                let fullApply = applyAll baseFunc allArgLists

                toAtom fullApply varGen env typeReg variantLookup funcReg moduleRegistry
                |> Result.map (fun (resultAtom, bindings, vg) ->
                    (wrapBindings bindings (ANF.Return resultAtom), vg))

        | AST.Let (letName, letValue, letBody) ->
            // Apply(let x = v in body, args) → let x = v in Apply(body, args)
            // Float the let binding out
            toANF (AST.Let (letName, letValue, AST.Apply (letBody, args))) varGen env typeReg variantLookup funcReg moduleRegistry

        | AST.Closure (funcName, captures) ->
            // Closure being called directly - convert to ClosureCall
            // First, convert captures to atoms
            let rec convertCaptures (caps: AST.Expr list) (vg: ANF.VarGen) (acc: (ANF.Atom * (ANF.TempId * ANF.CExpr) list) list) =
                match caps with
                | [] -> Ok (List.rev acc, vg)
                | cap :: rest ->
                    toAtom cap vg env typeReg variantLookup funcReg moduleRegistry
                    |> Result.bind (fun (capAtom, capBindings, vg') ->
                        convertCaptures rest vg' ((capAtom, capBindings) :: acc))
            convertCaptures captures varGen []
            |> Result.bind (fun (captureResults, varGen1) ->
                let captureAtoms = captureResults |> List.map fst
                let captureBindings = captureResults |> List.collect snd
                // Allocate closure
                let (closureId, varGen2) = ANF.freshVar varGen1
                let closureAlloc = ANF.ClosureAlloc (funcName, captureAtoms)
                // Convert args
                let rec convertArgs (remaining: AST.Expr list) (vg: ANF.VarGen) (acc: (ANF.Atom * (ANF.TempId * ANF.CExpr) list) list) =
                    match remaining with
                    | [] -> Ok (List.rev acc, vg)
                    | arg :: rest ->
                        toAtom arg vg env typeReg variantLookup funcReg moduleRegistry
                        |> Result.bind (fun (argAtom, argBindings, vg') ->
                            convertArgs rest vg' ((argAtom, argBindings) :: acc))
                convertArgs args varGen2 []
                |> Result.bind (fun (argResults, varGen3) ->
                    let argAtoms = argResults |> List.map fst
                    let argBindings = argResults |> List.collect snd
                    // Generate closure call
                    let (resultId, varGen4) = ANF.freshVar varGen3
                    let closureCall = ANF.ClosureCall (ANF.Var closureId, argAtoms)
                    let allBindings = captureBindings @ [(closureId, closureAlloc)] @ argBindings @ [(resultId, closureCall)]
                    Ok (wrapBindings allBindings (ANF.Return (ANF.Var resultId)), varGen4)))

        | _ ->
            // Other complex function expressions not yet supported
            Error $"Complex function application not yet fully implemented: {func}"

/// Convert an AST expression to an atom, introducing let bindings as needed
and toAtom (expr: AST.Expr) (varGen: ANF.VarGen) (env: VarEnv) (typeReg: TypeRegistry) (variantLookup: VariantLookup) (funcReg: FunctionRegistry) (moduleRegistry: AST.ModuleRegistry) : Result<ANF.Atom * (ANF.TempId * ANF.CExpr) list * ANF.VarGen, string> =
    match expr with
    | AST.UnitLiteral ->
        Ok (ANF.UnitLiteral, [], varGen)

    | AST.Int64Literal n ->
        Ok (ANF.IntLiteral (ANF.Int64 n), [], varGen)

    | AST.Int8Literal n ->
        Ok (ANF.IntLiteral (ANF.Int8 n), [], varGen)

    | AST.Int16Literal n ->
        Ok (ANF.IntLiteral (ANF.Int16 n), [], varGen)

    | AST.Int32Literal n ->
        Ok (ANF.IntLiteral (ANF.Int32 n), [], varGen)

    | AST.UInt8Literal n ->
        Ok (ANF.IntLiteral (ANF.UInt8 n), [], varGen)

    | AST.UInt16Literal n ->
        Ok (ANF.IntLiteral (ANF.UInt16 n), [], varGen)

    | AST.UInt32Literal n ->
        Ok (ANF.IntLiteral (ANF.UInt32 n), [], varGen)

    | AST.UInt64Literal n ->
        Ok (ANF.IntLiteral (ANF.UInt64 n), [], varGen)

    | AST.BoolLiteral b ->
        Ok (ANF.BoolLiteral b, [], varGen)

    | AST.StringLiteral s ->
        Ok (ANF.StringLiteral s, [], varGen)

    | AST.CharLiteral s ->
        // Char literal uses same representation as string
        Ok (ANF.StringLiteral s, [], varGen)

    | AST.FloatLiteral f ->
        Ok (ANF.FloatLiteral f, [], varGen)

    | AST.Var name ->
        // Variable reference: look up in environment
        match Map.tryFind name env with
        | Some (tempId, _) -> Ok (ANF.Var tempId, [], varGen)
        | None ->
            // Check if it's a module function (e.g., Stdlib.Int64.add)
            match Stdlib.tryGetFunctionWithFallback moduleRegistry name with
            | Some (_, _) ->
                // Module function reference - wrap in closure for uniform calling convention
                // Note: name should already be resolved by the type checker
                let (closureId, varGen') = ANF.freshVar varGen
                let closureAlloc = ANF.ClosureAlloc (name, [])
                Ok (ANF.Var closureId, [(closureId, closureAlloc)], varGen')
            | None ->
                // Check if it's a function reference (function name used as value)
                if Map.containsKey name funcReg then
                    // Wrap in closure for uniform calling convention
                    let (closureId, varGen') = ANF.freshVar varGen
                    let closureAlloc = ANF.ClosureAlloc (name, [])
                    Ok (ANF.Var closureId, [(closureId, closureAlloc)], varGen')
                else
                    Error $"Undefined variable: {name}"

    | AST.FuncRef name ->
        // Explicit function reference - wrap in closure for uniform calling convention
        let (closureId, varGen') = ANF.freshVar varGen
        let closureAlloc = ANF.ClosureAlloc (name, [])
        Ok (ANF.Var closureId, [(closureId, closureAlloc)], varGen')

    | AST.Closure (funcName, captures) ->
        // Closure in atom position: convert captures and create ClosureAlloc binding
        let rec convertCaptures (caps: AST.Expr list) (vg: ANF.VarGen) (acc: (ANF.Atom * (ANF.TempId * ANF.CExpr) list) list) =
            match caps with
            | [] -> Ok (List.rev acc, vg)
            | cap :: rest ->
                toAtom cap vg env typeReg variantLookup funcReg moduleRegistry
                |> Result.bind (fun (capAtom, capBindings, vg') ->
                    convertCaptures rest vg' ((capAtom, capBindings) :: acc))
        convertCaptures captures varGen []
        |> Result.map (fun (captureResults, varGen1) ->
            let captureAtoms = captureResults |> List.map fst
            let allBindings = captureResults |> List.collect snd
            // Create binding for ClosureAlloc
            let (closureId, varGen2) = ANF.freshVar varGen1
            let closureAlloc = ANF.ClosureAlloc (funcName, captureAtoms)
            (ANF.Var closureId, allBindings @ [(closureId, closureAlloc)], varGen2))

    | AST.Let (name, value, body) ->
        // Let binding in atom position: need to evaluate and return the body as an atom
        // Infer the type of the value for type-directed field lookup
        let typeEnv = typeEnvFromVarEnv env
        inferType value typeEnv typeReg variantLookup funcReg moduleRegistry
        |> Result.bind (fun valueType ->
            toAtom value varGen env typeReg variantLookup funcReg moduleRegistry |> Result.bind (fun (valueAtom, valueBindings, varGen1) ->
                let (tempId, varGen2) = ANF.freshVar varGen1
                let env' = Map.add name (tempId, valueType) env
                toAtom body varGen2 env' typeReg variantLookup funcReg moduleRegistry |> Result.map (fun (bodyAtom, bodyBindings, varGen3) ->
                    // All bindings: valueBindings + binding tempId to value + bodyBindings
                    let allBindings = valueBindings @ [(tempId, ANF.Atom valueAtom)] @ bodyBindings
                    (bodyAtom, allBindings, varGen3))))

    | AST.UnaryOp (AST.Neg, innerExpr) ->
        // Unary negation: use operand type to select float vs integer path
        let typeEnv = typeEnvFromVarEnv env
        inferType innerExpr typeEnv typeReg variantLookup funcReg moduleRegistry
        |> Result.bind (fun innerType ->
            match innerType with
            | AST.TFloat64 ->
                match innerExpr with
                | AST.FloatLiteral f ->
                    // Constant-fold negative float literals at compile time
                    Ok (ANF.FloatLiteral (-f), [], varGen)
                | _ ->
                    toAtom innerExpr varGen env typeReg variantLookup funcReg moduleRegistry
                    |> Result.map (fun (innerAtom, innerBindings, varGen1) ->
                        let (tempVar, varGen2) = ANF.freshVar varGen1
                        let cexpr = ANF.FloatNeg innerAtom
                        let allBindings = innerBindings @ [(tempVar, cexpr)]
                        (ANF.Var tempVar, allBindings, varGen2))
            | AST.TInt64 ->
                match innerExpr with
                | AST.Int64Literal n when n = System.Int64.MinValue ->
                    // The lexer stores INT64_MIN as a sentinel for "9223372036854775808"
                    // When negated, it should remain INT64_MIN (mathematically correct)
                    Ok (ANF.IntLiteral (ANF.Int64 System.Int64.MinValue), [], varGen)
                | _ ->
                    let zeroExpr = AST.Int64Literal 0L
                    toAtom (AST.BinOp (AST.Sub, zeroExpr, innerExpr)) varGen env typeReg variantLookup funcReg moduleRegistry
            | AST.TInt32 ->
                let zeroExpr = AST.Int32Literal 0l
                toAtom (AST.BinOp (AST.Sub, zeroExpr, innerExpr)) varGen env typeReg variantLookup funcReg moduleRegistry
            | AST.TInt16 ->
                let zeroExpr = AST.Int16Literal 0s
                toAtom (AST.BinOp (AST.Sub, zeroExpr, innerExpr)) varGen env typeReg variantLookup funcReg moduleRegistry
            | AST.TInt8 ->
                let zeroExpr = AST.Int8Literal 0y
                toAtom (AST.BinOp (AST.Sub, zeroExpr, innerExpr)) varGen env typeReg variantLookup funcReg moduleRegistry
            | _ ->
                Error $"Negation requires numeric operand, got {innerType}")

    | AST.UnaryOp (AST.Not, innerExpr) ->
        // Boolean not: convert operand to atom, create binding
        toAtom innerExpr varGen env typeReg variantLookup funcReg moduleRegistry |> Result.map (fun (innerAtom, innerBindings, varGen1) ->
            // Create the operation
            let (tempVar, varGen2) = ANF.freshVar varGen1
            let cexpr = ANF.UnaryPrim (ANF.Not, innerAtom)

            // Return the temp variable as atom, plus all bindings
            let allBindings = innerBindings @ [(tempVar, cexpr)]
            (ANF.Var tempVar, allBindings, varGen2))

    | AST.UnaryOp (AST.BitNot, innerExpr) ->
        // Bitwise NOT: convert operand to atom, create binding
        toAtom innerExpr varGen env typeReg variantLookup funcReg moduleRegistry |> Result.map (fun (innerAtom, innerBindings, varGen1) ->
            // Create the operation
            let (tempVar, varGen2) = ANF.freshVar varGen1
            let cexpr = ANF.UnaryPrim (ANF.BitNot, innerAtom)

            // Return the temp variable as atom, plus all bindings
            let allBindings = innerBindings @ [(tempVar, cexpr)]
            (ANF.Var tempVar, allBindings, varGen2))

    | AST.BinOp (op, left, right) ->
        // Complex expression: convert operands to atoms, create binding
        toAtom left varGen env typeReg variantLookup funcReg moduleRegistry |> Result.bind (fun (leftAtom, leftBindings, varGen1) ->
            toAtom right varGen1 env typeReg variantLookup funcReg moduleRegistry |> Result.bind (fun (rightAtom, rightBindings, varGen2) ->
                // Check if this is an equality comparison on compound types
                let typeEnv = typeEnvFromVarEnv env
                match op with
                | AST.Eq | AST.Neq ->
                    match inferType left typeEnv typeReg variantLookup funcReg moduleRegistry with
                    | Ok operandType when isCompoundType operandType ->
                        // Generate structural equality
                        let (eqBindings, eqResultAtom, varGen3) =
                            generateStructuralEquality leftAtom rightAtom operandType varGen2 typeReg variantLookup
                        // For Neq, negate the result
                        let (finalAtom, finalBindings, varGen4) =
                            if op = AST.Neq then
                                let (negVar, vg) = ANF.freshVar varGen3
                                let negExpr = ANF.UnaryPrim (ANF.Not, eqResultAtom)
                                (ANF.Var negVar, eqBindings @ [(negVar, negExpr)], vg)
                            else
                                (eqResultAtom, eqBindings, varGen3)
                        let allBindings = leftBindings @ rightBindings @ finalBindings
                        Ok (finalAtom, allBindings, varGen4)
                    | Ok AST.TString ->
                        // String equality - call __string_eq
                        let (tempVar, varGen3) = ANF.freshVar varGen2
                        let cexpr = ANF.Call ("__string_eq", [leftAtom; rightAtom])
                        // For Neq, negate the result
                        let (finalAtom, finalBindings, varGen4) =
                            if op = AST.Neq then
                                let (negVar, vg) = ANF.freshVar varGen3
                                let negExpr = ANF.UnaryPrim (ANF.Not, ANF.Var tempVar)
                                (ANF.Var negVar, [(tempVar, cexpr); (negVar, negExpr)], vg)
                            else
                                (ANF.Var tempVar, [(tempVar, cexpr)], varGen3)
                        let allBindings = leftBindings @ rightBindings @ finalBindings
                        Ok (finalAtom, allBindings, varGen4)
                    | _ ->
                        // Primitive type - simple comparison
                        let (tempVar, varGen3) = ANF.freshVar varGen2
                        let cexpr = ANF.Prim (convertBinOp op, leftAtom, rightAtom)
                        let allBindings = leftBindings @ rightBindings @ [(tempVar, cexpr)]
                        Ok (ANF.Var tempVar, allBindings, varGen3)
                | AST.StringConcat ->
                    let (tempVar, varGen3) = ANF.freshVar varGen2
                    let cexpr = ANF.StringConcat (leftAtom, rightAtom)
                    let allBindings = leftBindings @ rightBindings @ [(tempVar, cexpr)]
                    Ok (ANF.Var tempVar, allBindings, varGen3)
                // Arithmetic, bitwise, and comparison operators - use simple primitive
                | AST.Add | AST.Sub | AST.Mul | AST.Div | AST.Mod
                | AST.Shl | AST.Shr | AST.BitAnd | AST.BitOr | AST.BitXor
                | AST.Lt | AST.Gt | AST.Lte | AST.Gte
                | AST.And | AST.Or ->
                    let (tempVar, varGen3) = ANF.freshVar varGen2
                    let cexpr = ANF.Prim (convertBinOp op, leftAtom, rightAtom)
                    let allBindings = leftBindings @ rightBindings @ [(tempVar, cexpr)]
                    Ok (ANF.Var tempVar, allBindings, varGen3)))

    | AST.If (condExpr, thenExpr, elseExpr) ->
        // If expression in atom position: convert all parts to atoms, create IfValue
        toAtom condExpr varGen env typeReg variantLookup funcReg moduleRegistry |> Result.bind (fun (condAtom, condBindings, varGen1) ->
            toAtom thenExpr varGen1 env typeReg variantLookup funcReg moduleRegistry |> Result.bind (fun (thenAtom, thenBindings, varGen2) ->
                toAtom elseExpr varGen2 env typeReg variantLookup funcReg moduleRegistry |> Result.bind (fun (elseAtom, elseBindings, varGen3) ->
                    // Create a temporary for the result
                    let (tempVar, varGen4) = ANF.freshVar varGen3
                    // Create an IfValue CExpr
                    let ifCExpr = ANF.IfValue (condAtom, thenAtom, elseAtom)
                    // Return temp as atom with all bindings
                    let allBindings = condBindings @ thenBindings @ elseBindings @ [(tempVar, ifCExpr)]
                    Ok (ANF.Var tempVar, allBindings, varGen4))))

    | AST.Call (funcName, args) ->
        if isBuiltinUnwrapName funcName then
            Error "Internal error: Builtin.unwrap should be lowered via toANF, not toAtom"
        else
            // Function call in atom position: convert all arguments to atoms
            let rec convertArgs (argExprs: AST.Expr list) (vg: ANF.VarGen) (accAtoms: ANF.Atom list) (accBindings: (ANF.TempId * ANF.CExpr) list) : Result<ANF.Atom list * (ANF.TempId * ANF.CExpr) list * ANF.VarGen, string> =
                match argExprs with
                | [] -> Ok (List.rev accAtoms, accBindings, vg)
                | arg :: rest ->
                    toAtom arg vg env typeReg variantLookup funcReg moduleRegistry
                    |> Result.bind (fun (argAtom, argBindings, vg') ->
                        convertArgs rest vg' (argAtom :: accAtoms) (accBindings @ argBindings))

            convertArgs args varGen [] []
            |> Result.bind (fun (argAtoms, argBindings, varGen1) ->
                // Create a temporary for the call result
                let (tempVar, varGen2) = ANF.freshVar varGen1
                // Check if funcName is a variable (indirect call) or a defined function (direct call)
                match Map.tryFind funcName env with
                | Some (tempId, AST.TFunction (_, _)) ->
                    // Variable with function type - use closure call
                    // All function values are now closures (even non-capturing ones)
                    let callCExpr = ANF.ClosureCall (ANF.Var tempId, argAtoms)
                    let allBindings = argBindings @ [(tempVar, callCExpr)]
                    Ok (ANF.Var tempVar, allBindings, varGen2)
                | Some (_, varType) ->
                    // Variable exists but is not a function type
                    Error $"Cannot call '{funcName}' - it has type {varType}, not a function type"
                | None ->
                    // Not a variable - check if it's a file intrinsic first
                    match tryFileIntrinsic funcName argAtoms with
                    | Some intrinsicExpr ->
                        // File I/O intrinsic call
                        let allBindings = argBindings @ [(tempVar, intrinsicExpr)]
                        Ok (ANF.Var tempVar, allBindings, varGen2)
                    | None ->
                        // Check if it's a raw memory intrinsic
                        match tryRawMemoryIntrinsic funcName argAtoms with
                        | Some intrinsicExpr ->
                            // Raw memory intrinsic call
                            let allBindings = argBindings @ [(tempVar, intrinsicExpr)]
                            Ok (ANF.Var tempVar, allBindings, varGen2)
                        | None ->
                            // Check if it's a Float intrinsic
                            match tryFloatIntrinsic funcName argAtoms with
                            | Some intrinsicExpr ->
                                // Float intrinsic call
                                let allBindings = argBindings @ [(tempVar, intrinsicExpr)]
                                Ok (ANF.Var tempVar, allBindings, varGen2)
                            | None ->
                                // Check if it's a constant-fold intrinsic (Platform, Path)
                                match tryConstantFoldIntrinsic funcName argAtoms with
                                | Some intrinsicExpr ->
                                    // Constant-folded intrinsic
                                    let allBindings = argBindings @ [(tempVar, intrinsicExpr)]
                                    Ok (ANF.Var tempVar, allBindings, varGen2)
                                | None ->
                                    // Check if it's a random intrinsic
                                    match tryRandomIntrinsic funcName argAtoms with
                                    | Some intrinsicExpr ->
                                        // Random intrinsic call
                                        let allBindings = argBindings @ [(tempVar, intrinsicExpr)]
                                        Ok (ANF.Var tempVar, allBindings, varGen2)
                                    | None ->
                                        // Check if it's a date intrinsic
                                        match tryDateIntrinsic funcName argAtoms with
                                        | Some intrinsicExpr ->
                                            // Date intrinsic call
                                            let allBindings = argBindings @ [(tempVar, intrinsicExpr)]
                                            Ok (ANF.Var tempVar, allBindings, varGen2)
                                        | None ->
                                            // Assume it's a defined function (direct call)
                                            let callCExpr = ANF.Call (funcName, argAtoms)
                                            let allBindings = argBindings @ [(tempVar, callCExpr)]
                                            Ok (ANF.Var tempVar, allBindings, varGen2))

    | AST.TypeApp (_, _, _) ->
        // Placeholder: Generic instantiation not yet implemented
        Error "TypeApp (generic instantiation) not yet implemented in toAtom"

    | AST.TupleLiteral elements ->
        // Convert all elements to atoms
        let rec convertElements (elems: AST.Expr list) (vg: ANF.VarGen) (accAtoms: ANF.Atom list) (accBindings: (ANF.TempId * ANF.CExpr) list) : Result<ANF.Atom list * (ANF.TempId * ANF.CExpr) list * ANF.VarGen, string> =
            match elems with
            | [] -> Ok (List.rev accAtoms, accBindings, vg)
            | elem :: rest ->
                toAtom elem vg env typeReg variantLookup funcReg moduleRegistry
                |> Result.bind (fun (elemAtom, elemBindings, vg') ->
                    convertElements rest vg' (elemAtom :: accAtoms) (accBindings @ elemBindings))

        convertElements elements varGen [] []
        |> Result.map (fun (elemAtoms, elemBindings, varGen1) ->
            // Create a temporary for the tuple
            let (tempVar, varGen2) = ANF.freshVar varGen1
            let tupleCExpr = ANF.TupleAlloc elemAtoms
            // Return temp as atom with all bindings
            let allBindings = elemBindings @ [(tempVar, tupleCExpr)]
            (ANF.Var tempVar, allBindings, varGen2))

    | AST.TupleAccess (tupleExpr, index) ->
        // Convert tuple to atom and create TupleGet
        toAtom tupleExpr varGen env typeReg variantLookup funcReg moduleRegistry
        |> Result.map (fun (tupleAtom, tupleBindings, varGen1) ->
            let (tempVar, varGen2) = ANF.freshVar varGen1
            let getCExpr = ANF.TupleGet (tupleAtom, index)
            // Return temp as atom with all bindings
            let allBindings = tupleBindings @ [(tempVar, getCExpr)]
            (ANF.Var tempVar, allBindings, varGen2))

    | AST.RecordLiteral (typeName, fields) ->
        // Records are compiled like tuples
        let fieldOrder =
            if typeName = "" then
                fields |> List.map fst
            else
                match Map.tryFind typeName typeReg with
                | Some typeFields -> typeFields |> List.map fst
                | None -> fields |> List.map fst

        let fieldMap = Map.ofList fields
        let orderedValues =
            fieldOrder
            |> List.choose (fun fname -> Map.tryFind fname fieldMap)

        // Reuse tuple handling
        toAtom (AST.TupleLiteral orderedValues) varGen env typeReg variantLookup funcReg moduleRegistry

    | AST.RecordUpdate (recordExpr, updates) ->
        // Desugar to RecordLiteral: build new record with updated fields
        let typeEnv = typeEnvFromVarEnv env
        inferType recordExpr typeEnv typeReg variantLookup funcReg moduleRegistry
        |> Result.bind (fun recordType ->
            match recordType with
            | AST.TRecord (typeName, _) ->
                match Map.tryFind typeName typeReg with
                | Some typeFields ->
                    let updateMap = Map.ofList updates
                    let newFields =
                        typeFields
                        |> List.map (fun (fname, _) ->
                            match Map.tryFind fname updateMap with
                            | Some updateExpr -> (fname, updateExpr)
                            | None -> (fname, AST.RecordAccess (recordExpr, fname)))
                    toAtom (AST.RecordLiteral (typeName, newFields)) varGen env typeReg variantLookup funcReg moduleRegistry
                | None -> Error $"Unknown record type: {typeName}"
            | _ -> Error "Cannot use record update syntax on non-record type")

    | AST.RecordAccess (recordExpr, fieldName) ->
        // Records are compiled like tuples - field access becomes TupleGet
        // Use type-directed lookup: infer the record type, then find field index
        let typeEnv = typeEnvFromVarEnv env
        inferType recordExpr typeEnv typeReg variantLookup funcReg moduleRegistry
        |> Result.bind (fun recordType ->
            match recordType with
            | AST.TRecord (typeName, _) ->
                // Look up field index in the specific record type
                match Map.tryFind typeName typeReg with
                | Some fields ->
                    match List.tryFindIndex (fun (name, _) -> name = fieldName) fields with
                    | Some index ->
                        toAtom recordExpr varGen env typeReg variantLookup funcReg moduleRegistry
                        |> Result.bind (fun (recordAtom, recordBindings, varGen1) ->
                            let (tempVar, varGen2) = ANF.freshVar varGen1
                            let getCExpr = ANF.TupleGet (recordAtom, index)
                            let allBindings = recordBindings @ [(tempVar, getCExpr)]
                            Ok (ANF.Var tempVar, allBindings, varGen2))
                    | None ->
                        Error $"Record type '{typeName}' has no field '{fieldName}'"
                | None ->
                    Error $"Unknown record type: {typeName}"
            | _ ->
                Error $"Cannot access field '{fieldName}' on non-record type")

    | AST.Constructor (_, variantName, payload) ->
        match Map.tryFind variantName variantLookup with
        | None ->
            Error $"Unknown constructor: {variantName}"
        | Some (typeName, _, tag, _) ->
            // Check if ANY variant in this type has a payload
            // Note: We get typeName from variantLookup, not from AST (which may be empty)
            let typeHasPayloadVariants =
                variantLookup
                |> Map.exists (fun _ (tName, _, _, pType) -> tName = typeName && pType.IsSome)

            match payload with
            | None when not typeHasPayloadVariants ->
                // Pure enum type: return tag as an integer (no bindings needed)
                Ok (ANF.IntLiteral (ANF.Int64 (int64 tag)), [], varGen)
            | None ->
                // No payload but type has other variants with payloads
                // Heap-allocate as [tag, 0] for uniform 2-element structure
                // This enables consistent structural equality comparison
                let tagAtom = ANF.IntLiteral (ANF.Int64 (int64 tag))
                let dummyPayload = ANF.IntLiteral (ANF.Int64 0L)
                let (tempVar, varGen1) = ANF.freshVar varGen
                let tupleCExpr = ANF.TupleAlloc [tagAtom; dummyPayload]
                Ok (ANF.Var tempVar, [(tempVar, tupleCExpr)], varGen1)
            | Some payloadExpr ->
                // Variant with payload: allocate [tag, payload] on heap
                toAtom payloadExpr varGen env typeReg variantLookup funcReg moduleRegistry
                |> Result.map (fun (payloadAtom, payloadBindings, varGen1) ->
                    let tagAtom = ANF.IntLiteral (ANF.Int64 (int64 tag))
                    // Create TupleAlloc [tag, payload] and bind to fresh variable
                    let (tempVar, varGen2) = ANF.freshVar varGen1
                    let tupleCExpr = ANF.TupleAlloc [tagAtom; payloadAtom]
                    let allBindings = payloadBindings @ [(tempVar, tupleCExpr)]
                    (ANF.Var tempVar, allBindings, varGen2))

    | AST.ListLiteral elements ->
        // Compile list literal as FingerTree in atom position
        // Tags: EMPTY=0, SINGLE=1, DEEP=2, NODE2=3, NODE3=4, LEAF=5
        // DEEP layout: [measure:8][prefixCount:8][p0:8][p1:8][p2:8][p3:8][middle:8][suffixCount:8][s0:8][s1:8][s2:8][s3:8]

        // Increment refcount for heap elements stored in leaves
        let addLeafInc (elemAtom: ANF.Atom) (elemType: AST.Type) (vg: ANF.VarGen) (bindings: (ANF.TempId * ANF.CExpr) list) =
            match elemAtom with
            | ANF.Var _ when ANF.isHeapType elemType ->
                let size = ANF.payloadSize elemType typeReg
                let (incVar, vg1) = ANF.freshVar vg
                let incExpr = ANF.RefCountInc (elemAtom, size)
                (vg1, bindings @ [(incVar, incExpr)])
            | _ ->
                (vg, bindings)

        // Helper to create a LEAF node wrapping an element
        let allocLeaf (elemAtom: ANF.Atom) (elemType: AST.Type) (vg: ANF.VarGen) (bindings: (ANF.TempId * ANF.CExpr) list) =
            let (ptrVar, vg1) = ANF.freshVar vg
            let (setVar, vg2) = ANF.freshVar vg1
            let allocExpr = ANF.RawAlloc (ANF.IntLiteral (ANF.Int64 8L))
            let setExpr = ANF.RawSet (ANF.Var ptrVar, ANF.IntLiteral (ANF.Int64 0L), elemAtom, None)
            let (vg3, bindings3) = addLeafInc elemAtom elemType vg2 (bindings @ [(ptrVar, allocExpr); (setVar, setExpr)])
            let (taggedVar, vg4) = ANF.freshVar vg3
            let tagExpr = ANF.Prim (ANF.BitOr, ANF.Var ptrVar, ANF.IntLiteral (ANF.Int64 5L))  // tag 5 = LEAF
            let newBindings = bindings3 @ [(taggedVar, tagExpr)]
            (ANF.Var taggedVar, newBindings, vg4)

        // Helper to create a SINGLE node containing a TreeNode
        let allocSingle (nodeAtom: ANF.Atom) (vg: ANF.VarGen) (bindings: (ANF.TempId * ANF.CExpr) list) =
            let (ptrVar, vg1) = ANF.freshVar vg
            let (setVar, vg2) = ANF.freshVar vg1
            let (taggedVar, vg3) = ANF.freshVar vg2
            let allocExpr = ANF.RawAlloc (ANF.IntLiteral (ANF.Int64 8L))
            let setExpr = ANF.RawSet (ANF.Var ptrVar, ANF.IntLiteral (ANF.Int64 0L), nodeAtom, None)
            let tagExpr = ANF.Prim (ANF.BitOr, ANF.Var ptrVar, ANF.IntLiteral (ANF.Int64 1L))  // tag 1 = SINGLE
            let newBindings = bindings @ [(ptrVar, allocExpr); (setVar, setExpr); (taggedVar, tagExpr)]
            (ANF.Var taggedVar, newBindings, vg3)

        // Helper to create a DEEP node
        let allocDeep (measure: int) (prefixNodes: ANF.Atom list) (middle: ANF.Atom) (suffixNodes: ANF.Atom list) (vg: ANF.VarGen) (bindings: (ANF.TempId * ANF.CExpr) list) =
            let prefixCount = List.length prefixNodes
            let suffixCount = List.length suffixNodes
            let (ptrVar, vg1) = ANF.freshVar vg
            let allocExpr = ANF.RawAlloc (ANF.IntLiteral (ANF.Int64 96L))  // 12 fields * 8 bytes

            // Build all the set operations
            let setAt offset value vg bindings =
                let (setVar, vg') = ANF.freshVar vg
                let setExpr = ANF.RawSet (ANF.Var ptrVar, ANF.IntLiteral (ANF.Int64 (int64 offset)), value, None)
                (vg', bindings @ [(setVar, setExpr)])

            let (vg2, bindings2) = setAt 0 (ANF.IntLiteral (ANF.Int64 (int64 measure))) vg1 (bindings @ [(ptrVar, allocExpr)])
            let (vg3, bindings3) = setAt 8 (ANF.IntLiteral (ANF.Int64 (int64 prefixCount))) vg2 bindings2

            // Set prefix nodes (p0-p3 at offsets 16, 24, 32, 40)
            let rec setPrefix nodes offset vg bindings =
                match nodes with
                | [] -> (vg, bindings)
                | n :: rest ->
                    let (vg', bindings') = setAt offset n vg bindings
                    setPrefix rest (offset + 8) vg' bindings'
            let (vg4, bindings4) = setPrefix prefixNodes 16 vg3 bindings3

            // Set middle at offset 48 (type-uniform: another FingerTree of nodes)
            let (vg5, bindings5) = setAt 48 middle vg4 bindings4

            // Set suffix count at offset 56
            let (vg6, bindings6) = setAt 56 (ANF.IntLiteral (ANF.Int64 (int64 suffixCount))) vg5 bindings5

            // Set suffix nodes (s0-s3 at offsets 64, 72, 80, 88)
            let (vg7, bindings7) = setPrefix suffixNodes 64 vg6 bindings6

            // Tag with DEEP (2)
            let (taggedVar, vg8) = ANF.freshVar vg7
            let tagExpr = ANF.Prim (ANF.BitOr, ANF.Var ptrVar, ANF.IntLiteral (ANF.Int64 2L))
            (ANF.Var taggedVar, bindings7 @ [(taggedVar, tagExpr)], vg8)

        // Build FingerTree nodes for middle spines without using pushBack.
        let emptyTree = ANF.IntLiteral (ANF.Int64 0L)

        let nodeAtom (node: ANF.Atom, _measure: int) = node
        let nodeMeasure (_node: ANF.Atom, measure: int) = measure

        // Helper to create a NODE2 (tag 3): [child0:8][child1:8][measure:8]
        let allocNode2 (left: ANF.Atom * int) (right: ANF.Atom * int) (vg: ANF.VarGen) (bindings: (ANF.TempId * ANF.CExpr) list) =
            let (ptrVar, vg1) = ANF.freshVar vg
            let allocExpr = ANF.RawAlloc (ANF.IntLiteral (ANF.Int64 24L))
            let (set0Var, vg2) = ANF.freshVar vg1
            let set0Expr = ANF.RawSet (ANF.Var ptrVar, ANF.IntLiteral (ANF.Int64 0L), nodeAtom left, None)
            let (set1Var, vg3) = ANF.freshVar vg2
            let set1Expr = ANF.RawSet (ANF.Var ptrVar, ANF.IntLiteral (ANF.Int64 8L), nodeAtom right, None)
            let measure = nodeMeasure left + nodeMeasure right
            let (set2Var, vg4) = ANF.freshVar vg3
            let set2Expr = ANF.RawSet (ANF.Var ptrVar, ANF.IntLiteral (ANF.Int64 16L), ANF.IntLiteral (ANF.Int64 (int64 measure)), None)
            let (taggedVar, vg5) = ANF.freshVar vg4
            let tagExpr = ANF.Prim (ANF.BitOr, ANF.Var ptrVar, ANF.IntLiteral (ANF.Int64 3L))  // tag 3 = NODE2
            let newBindings = bindings @ [(ptrVar, allocExpr); (set0Var, set0Expr); (set1Var, set1Expr); (set2Var, set2Expr); (taggedVar, tagExpr)]
            ((ANF.Var taggedVar, measure), newBindings, vg5)

        // Helper to create a NODE3 (tag 4): [child0:8][child1:8][child2:8][measure:8]
        let allocNode3 (first: ANF.Atom * int) (second: ANF.Atom * int) (third: ANF.Atom * int) (vg: ANF.VarGen) (bindings: (ANF.TempId * ANF.CExpr) list) =
            let (ptrVar, vg1) = ANF.freshVar vg
            let allocExpr = ANF.RawAlloc (ANF.IntLiteral (ANF.Int64 32L))
            let (set0Var, vg2) = ANF.freshVar vg1
            let set0Expr = ANF.RawSet (ANF.Var ptrVar, ANF.IntLiteral (ANF.Int64 0L), nodeAtom first, None)
            let (set1Var, vg3) = ANF.freshVar vg2
            let set1Expr = ANF.RawSet (ANF.Var ptrVar, ANF.IntLiteral (ANF.Int64 8L), nodeAtom second, None)
            let (set2Var, vg4) = ANF.freshVar vg3
            let set2Expr = ANF.RawSet (ANF.Var ptrVar, ANF.IntLiteral (ANF.Int64 16L), nodeAtom third, None)
            let measure = nodeMeasure first + nodeMeasure second + nodeMeasure third
            let (set3Var, vg5) = ANF.freshVar vg4
            let set3Expr = ANF.RawSet (ANF.Var ptrVar, ANF.IntLiteral (ANF.Int64 24L), ANF.IntLiteral (ANF.Int64 (int64 measure)), None)
            let (taggedVar, vg6) = ANF.freshVar vg5
            let tagExpr = ANF.Prim (ANF.BitOr, ANF.Var ptrVar, ANF.IntLiteral (ANF.Int64 4L))  // tag 4 = NODE3
            let newBindings =
                bindings @ [(ptrVar, allocExpr); (set0Var, set0Expr); (set1Var, set1Expr); (set2Var, set2Expr); (set3Var, set3Expr); (taggedVar, tagExpr)]
            ((ANF.Var taggedVar, measure), newBindings, vg6)

        let splitAt count nodes =
            let rec loop remaining acc rest =
                match remaining, rest with
                | 0, _ -> Ok (List.rev acc, rest)
                | _, [] -> Error "List literal: not enough nodes for split"
                | n, x :: xs -> loop (n - 1) (x :: acc) xs
            loop count [] nodes

        let groupSizes nodeCount =
            if nodeCount < 2 then
                Error "List literal: middle spine needs at least 2 nodes"
            else
                match nodeCount % 3 with
                | 0 -> Ok (List.replicate (nodeCount / 3) 3)
                | 1 ->
                    if nodeCount < 4 then
                        Error "List literal: invalid middle spine size"
                    else
                        Ok (2 :: 2 :: List.replicate ((nodeCount - 4) / 3) 3)
                | _ ->
                    Ok (2 :: List.replicate ((nodeCount - 2) / 3) 3)

        let rec buildGroupedNodes sizes nodes vg bindings acc =
            match sizes with
            | [] -> Ok (List.rev acc, bindings, vg)
            | size :: rest ->
                splitAt size nodes
                |> Result.bind (fun (group, remaining) ->
                    match size, group with
                    | 2, [a; b] ->
                        let (nodeInfo, bindings1, vg1) = allocNode2 a b vg bindings
                        buildGroupedNodes rest remaining vg1 bindings1 (nodeInfo :: acc)
                    | 3, [a; b; c] ->
                        let (nodeInfo, bindings1, vg1) = allocNode3 a b c vg bindings
                        buildGroupedNodes rest remaining vg1 bindings1 (nodeInfo :: acc)
                    | _ ->
                        Error $"List literal: unexpected group size {size}")

        let rec buildTree (nodes: (ANF.Atom * int) list) (vg: ANF.VarGen) (bindings: (ANF.TempId * ANF.CExpr) list) =
            let nodeCount = List.length nodes
            match nodes with
            | [] -> Ok (emptyTree, bindings, vg)
            | [single] ->
                let (resultAtom, resultBindings, vg1) = allocSingle (nodeAtom single) vg bindings
                Ok (resultAtom, resultBindings, vg1)
            | first :: rest when nodeCount <= 5 ->
                let totalMeasure = nodes |> List.sumBy nodeMeasure
                let prefixNodes = [nodeAtom first]
                let suffixNodes = rest |> List.map nodeAtom
                let (resultAtom, resultBindings, vg1) = allocDeep totalMeasure prefixNodes emptyTree suffixNodes vg bindings
                Ok (resultAtom, resultBindings, vg1)
            | _ ->
                splitAt 2 nodes
                |> Result.bind (fun (prefixNodes, rest) ->
                    let restLength = List.length rest
                    let middleCount = restLength - 2
                    splitAt middleCount rest
                    |> Result.bind (fun (middleNodes, suffixNodes) ->
                        groupSizes (List.length middleNodes)
                        |> Result.bind (fun sizes ->
                            buildGroupedNodes sizes middleNodes vg bindings []
                            |> Result.bind (fun (groupedMiddle, bindings1, vg1) ->
                                buildTree groupedMiddle vg1 bindings1
                                |> Result.map (fun (middleTree, bindings2, vg2) ->
                                    let totalMeasure = nodes |> List.sumBy nodeMeasure
                                    let prefixAtoms = prefixNodes |> List.map nodeAtom
                                    let suffixAtoms = suffixNodes |> List.map nodeAtom
                                    let (resultAtom, resultBindings, vg3) =
                                        allocDeep totalMeasure prefixAtoms middleTree suffixAtoms vg2 bindings2
                                    (resultAtom, resultBindings, vg3))))))

        if List.isEmpty elements then
            // Empty list is EMPTY (represented as 0)
            Ok (ANF.IntLiteral (ANF.Int64 0L), [], varGen)
        else
            let typeEnv = typeEnvFromVarEnv env

            // Convert all elements to atoms first
            let rec convertElements (elems: AST.Expr list) (vg: ANF.VarGen) (acc: (ANF.Atom * AST.Type * (ANF.TempId * ANF.CExpr) list) list) =
                match elems with
                | [] -> Ok (List.rev acc, vg)
                | e :: rest ->
                    inferType e typeEnv typeReg variantLookup funcReg moduleRegistry
                    |> Result.bind (fun elemType ->
                        toAtom e vg env typeReg variantLookup funcReg moduleRegistry
                        |> Result.bind (fun (atom, bindings, vg') ->
                            convertElements rest vg' ((atom, elemType, bindings) :: acc)))

            convertElements elements varGen []
            |> Result.bind (fun (atomsWithBindings, varGen1) ->
                // Flatten all element bindings
                let elemBindings = atomsWithBindings |> List.collect (fun (_, _, bindings) -> bindings)
                let elemAtoms = atomsWithBindings |> List.map (fun (atom, elemType, _) -> (atom, elemType))

                // Create LEAF nodes for all elements
                let rec createLeaves (atoms: (ANF.Atom * AST.Type) list) (vg: ANF.VarGen) (bindings: (ANF.TempId * ANF.CExpr) list) (acc: ANF.Atom list) =
                    match atoms with
                    | [] -> (List.rev acc, bindings, vg)
                    | (a, elemType) :: rest ->
                        let (leafAtom, bindings', vg') = allocLeaf a elemType vg bindings
                        createLeaves rest vg' bindings' (leafAtom :: acc)

                let (leafAtoms, leafBindings, varGen2) = createLeaves elemAtoms varGen1 elemBindings []
                let leafNodes = leafAtoms |> List.map (fun atom -> (atom, 1))

                buildTree leafNodes varGen2 leafBindings)

    | AST.ListCons (headElements, tail) ->
        // Compile list cons in atom position: [a, b, ...tail] prepends elements to tail
        // Use Stdlib.__FingerTree.push to prepend each element
        toAtom tail varGen env typeReg variantLookup funcReg moduleRegistry
        |> Result.bind (fun (tailAtom, tailBindings, varGen1) ->
            // Build list by prepending elements from right to left
            // [a, b, ...tail] means push(push(tail, b), a)
            let rec buildList (elems: AST.Expr list) (vg: ANF.VarGen) (currentList: ANF.Atom) (allBindings: (ANF.TempId * ANF.CExpr) list) : Result<ANF.Atom * (ANF.TempId * ANF.CExpr) list * ANF.VarGen, string> =
                match elems with
                | [] -> Ok (currentList, allBindings, vg)
                | elem :: rest ->
                    // First build the rest of the list, then prepend this element
                    buildList rest vg currentList allBindings
                    |> Result.bind (fun (restList, restBindings, vg1) ->
                        toAtom elem vg1 env typeReg variantLookup funcReg moduleRegistry
                        |> Result.map (fun (elemAtom, elemBindings, vg2) ->
                            let (pushVar, vg3) = ANF.freshVar vg2
                            // Call Stdlib.__FingerTree.push to prepend element
                            let pushExpr = ANF.Call ("Stdlib.__FingerTree.push_i64", [restList; elemAtom])
                            let newBindings = restBindings @ elemBindings @ [(pushVar, pushExpr)]
                            (ANF.Var pushVar, newBindings, vg3)))

            if List.isEmpty headElements then
                Ok (tailAtom, tailBindings, varGen1)
            else
                buildList headElements varGen1 tailAtom tailBindings)

    | AST.InterpolatedString parts ->
        // Desugar interpolated string to StringConcat chain
        let partToExpr (part: AST.StringPart) : AST.Expr =
            match part with
            | AST.StringText s -> AST.StringLiteral s
            | AST.StringExpr e -> e
        match parts with
        | [] ->
            // Empty interpolated string → empty string
            Ok (ANF.StringLiteral "", [], varGen)
        | [single] ->
            // Single part → convert directly
            toAtom (partToExpr single) varGen env typeReg variantLookup funcReg moduleRegistry
        | first :: rest ->
            // Multiple parts → desugar to StringConcat and convert
            let desugared =
                rest
                |> List.fold (fun acc part ->
                    AST.BinOp (AST.StringConcat, acc, partToExpr part))
                    (partToExpr first)
            toAtom desugared varGen env typeReg variantLookup funcReg moduleRegistry

    | AST.Match (scrutinee, cases) ->
        // Match in atom position - compile and extract result
        toANF (AST.Match (scrutinee, cases)) varGen env typeReg variantLookup funcReg moduleRegistry
        |> Result.bind (fun (matchExpr, varGen1) ->
            // The match compiles to an if-else chain that returns a value
            // We need to extract that value into a temp variable
            // For now, just return an error - complex match in atom position needs more work
            Error "Match expressions in atom position not yet supported (use let binding)")

    | AST.Lambda (_parameters, _body) ->
        // Lambda in atom position - closures not yet fully implemented
        Error "Lambda expressions (closures) are not yet fully implemented"

    | AST.Apply (func, args) ->
        // Apply in atom position - convert via toANF and extract result
        match func with
        | AST.Lambda (parameters, body) ->
            // Immediate application: desugar to let bindings
            if List.length args <> List.length parameters then
                Error $"Lambda expects {List.length parameters} arguments, got {List.length args}"
            else
                let rec buildLets (ps: (string * AST.Type) list) (as': AST.Expr list) : AST.Expr =
                    match ps, as' with
                    | [], [] -> body
                    | (pName, _) :: restPs, argExpr :: restAs ->
                        AST.Let (pName, argExpr, buildLets restPs restAs)
                    | _ -> body
                let desugared = buildLets parameters args
                toAtom desugared varGen env typeReg variantLookup funcReg moduleRegistry

        | AST.Apply (innerFunc, innerArgs) ->
            // Nested application in atom position: ((x) => (y) => ...)(a)(b)
            match innerFunc with
            | AST.Lambda (innerParams, innerBody) ->
                if List.length innerArgs <> List.length innerParams then
                    Error $"Inner lambda expects {List.length innerParams} arguments, got {List.length innerArgs}"
                else
                    let rec buildLets (ps: (string * AST.Type) list) (as': AST.Expr list) : AST.Expr =
                        match ps, as' with
                        | [], [] -> innerBody
                        | (pName, _) :: restPs, argExpr :: restAs ->
                            AST.Let (pName, argExpr, buildLets restPs restAs)
                        | _ -> innerBody
                    let desugaredInner = buildLets innerParams innerArgs
                    toAtom (AST.Apply (desugaredInner, args)) varGen env typeReg variantLookup funcReg moduleRegistry
            | _ ->
                // Inner is complex - evaluate inner, then call as closure
                toAtom (AST.Apply (innerFunc, innerArgs)) varGen env typeReg variantLookup funcReg moduleRegistry
                |> Result.bind (fun (closureAtom, closureBindings, varGen1) ->
                    let rec convertArgs (remaining: AST.Expr list) (vg: ANF.VarGen) (acc: (ANF.Atom * (ANF.TempId * ANF.CExpr) list) list) =
                        match remaining with
                        | [] -> Ok (List.rev acc, vg)
                        | arg :: rest ->
                            toAtom arg vg env typeReg variantLookup funcReg moduleRegistry
                            |> Result.bind (fun (argAtom, argBindings, vg') ->
                                convertArgs rest vg' ((argAtom, argBindings) :: acc))
                    convertArgs args varGen1 []
                    |> Result.bind (fun (argResults, varGen2) ->
                        let argAtoms = argResults |> List.map fst
                        let argBindings = argResults |> List.collect snd
                        let (resultId, varGen3) = ANF.freshVar varGen2
                        let closureCall = ANF.ClosureCall (closureAtom, argAtoms)
                        let allBindings = closureBindings @ argBindings @ [(resultId, closureCall)]
                        Ok (ANF.Var resultId, allBindings, varGen3)))

        | AST.Let (letName, letValue, letBody) ->
            // Apply(let x = v in body, args) in atom position
            // Float the let out and recurse
            toAtom (AST.Let (letName, letValue, AST.Apply (letBody, args))) varGen env typeReg variantLookup funcReg moduleRegistry

        | AST.Var name ->
            // Variable call in atom position - treat as closure call
            match Map.tryFind name env with
            | Some (tempId, _) ->
                let rec convertArgs (remaining: AST.Expr list) (vg: ANF.VarGen) (acc: (ANF.Atom * (ANF.TempId * ANF.CExpr) list) list) =
                    match remaining with
                    | [] -> Ok (List.rev acc, vg)
                    | arg :: rest ->
                        toAtom arg vg env typeReg variantLookup funcReg moduleRegistry
                        |> Result.bind (fun (argAtom, argBindings, vg') ->
                            convertArgs rest vg' ((argAtom, argBindings) :: acc))
                convertArgs args varGen []
                |> Result.bind (fun (argResults, varGen1) ->
                    let argAtoms = argResults |> List.map fst
                    let allBindings = argResults |> List.collect snd
                    let (resultId, varGen2) = ANF.freshVar varGen1
                    let closureCall = ANF.ClosureCall (ANF.Var tempId, argAtoms)
                    let finalBindings = allBindings @ [(resultId, closureCall)]
                    Ok (ANF.Var resultId, finalBindings, varGen2))
            | None ->
                Error $"Cannot apply variable '{name}' as function in atom position - variable not in scope"

        | AST.Closure (funcName, captures) ->
            // Closure call in atom position
            let rec convertCaptures (caps: AST.Expr list) (vg: ANF.VarGen) (acc: (ANF.Atom * (ANF.TempId * ANF.CExpr) list) list) =
                match caps with
                | [] -> Ok (List.rev acc, vg)
                | cap :: rest ->
                    toAtom cap vg env typeReg variantLookup funcReg moduleRegistry
                    |> Result.bind (fun (capAtom, capBindings, vg') ->
                        convertCaptures rest vg' ((capAtom, capBindings) :: acc))
            convertCaptures captures varGen []
            |> Result.bind (fun (captureResults, varGen1) ->
                let captureAtoms = captureResults |> List.map fst
                let captureBindings = captureResults |> List.collect snd
                let (closureId, varGen2) = ANF.freshVar varGen1
                let closureAlloc = ANF.ClosureAlloc (funcName, captureAtoms)
                let rec convertArgs (remaining: AST.Expr list) (vg: ANF.VarGen) (acc: (ANF.Atom * (ANF.TempId * ANF.CExpr) list) list) =
                    match remaining with
                    | [] -> Ok (List.rev acc, vg)
                    | arg :: rest ->
                        toAtom arg vg env typeReg variantLookup funcReg moduleRegistry
                        |> Result.bind (fun (argAtom, argBindings, vg') ->
                            convertArgs rest vg' ((argAtom, argBindings) :: acc))
                convertArgs args varGen2 []
                |> Result.bind (fun (argResults, varGen3) ->
                    let argAtoms = argResults |> List.map fst
                    let argBindings = argResults |> List.collect snd
                    let (resultId, varGen4) = ANF.freshVar varGen3
                    let closureCall = ANF.ClosureCall (ANF.Var closureId, argAtoms)
                    let allBindings = captureBindings @ [(closureId, closureAlloc)] @ argBindings @ [(resultId, closureCall)]
                    Ok (ANF.Var resultId, allBindings, varGen4)))

        | _ ->
            Error $"Complex function application in atom position not yet supported: {func}"

/// Replace Return sites in an AExpr with a continuation expression.
and bindReturns (expr: ANF.AExpr) (k: ANF.Atom -> ANF.AExpr) : ANF.AExpr =
    match expr with
    | ANF.Return atom ->
        k atom
    | ANF.Let (id, cexpr, rest) ->
        ANF.Let (id, cexpr, bindReturns rest k)
    | ANF.If (cond, thenBranch, elseBranch) ->
        ANF.If (cond, bindReturns thenBranch k, bindReturns elseBranch k)

/// Convert an expression to an ANF expression that returns a stable atom variable.
/// Falls back to full toANF when the expression cannot be lowered directly to toAtom.
and toANFBoundAtom
    (expr: AST.Expr)
    (varGen: ANF.VarGen)
    (env: VarEnv)
    (typeReg: TypeRegistry)
    (variantLookup: VariantLookup)
    (funcReg: FunctionRegistry)
    (moduleRegistry: AST.ModuleRegistry)
    : Result<ANF.AExpr * ANF.Atom * ANF.VarGen, string> =
    match toAtom expr varGen env typeReg variantLookup funcReg moduleRegistry with
    | Ok (atom, bindings, vg1) ->
        // Keep existing atom lowering behavior unchanged when toAtom succeeds:
        // do not introduce extra temp ids in the common path.
        Ok (wrapBindings bindings (ANF.Return atom), atom, vg1)
    | Error _ ->
        let (boundVar, vg1) = ANF.freshVar varGen
        toANF expr vg1 env typeReg variantLookup funcReg moduleRegistry
        |> Result.map (fun (exprA, vg2) ->
            let boundExpr =
                bindReturns exprA (fun atom ->
                    ANF.Let (boundVar, ANF.Atom atom, ANF.Return (ANF.Var boundVar)))
            (boundExpr, ANF.Var boundVar, vg2))

/// Wrap let bindings around an expression
and wrapBindings (bindings: (ANF.TempId * ANF.CExpr) list) (expr: ANF.AExpr) : ANF.AExpr =
    List.foldBack (fun (var, cexpr) acc -> ANF.Let (var, cexpr, acc)) bindings expr

/// Convert a function definition to ANF
/// VarGen is passed in and out to maintain globally unique TempIds across functions
/// (needed for TypeMap which maps TempId -> Type across the whole program)
let convertFunction (funcDef: AST.FunctionDef) (varGen: ANF.VarGen) (typeReg: TypeRegistry) (variantLookup: VariantLookup) (funcReg: FunctionRegistry) (moduleRegistry: AST.ModuleRegistry) : Result<ANF.Function * ANF.VarGen, string> =
    // Allocate TempIds for parameters, bundled with their types
    let (typedParams, varGen1) =
        funcDef.Params
        |> List.fold (fun (acc, vg) (_, typ) ->
            let (tempId, vg') = ANF.freshVar vg
            (acc @ [{ ANF.TypedParam.Id = tempId; Type = typ }], vg')) ([], varGen)

    // Build environment mapping param names to (TempId, Type)
    let paramEnv : VarEnv =
        List.zip funcDef.Params typedParams
        |> List.map (fun ((name, _), typedParam) -> (name, (typedParam.Id, typedParam.Type)))
        |> Map.ofList

    // Convert body
    toANF funcDef.Body varGen1 paramEnv typeReg variantLookup funcReg moduleRegistry
    |> Result.map (fun (body, varGen2) ->
        ({ Name = funcDef.Name; TypedParams = typedParams; ReturnType = funcDef.ReturnType; Body = body }, varGen2))

/// Result type that includes registries needed for later passes
type ConversionResult = {
    Program: ANF.Program
    TypeReg: TypeRegistry
    VariantLookup: VariantLookup
    FuncReg: FunctionRegistry
    FuncParams: Map<string, (string * AST.Type) list>  // Function name -> param list with types
    ModuleRegistry: AST.ModuleRegistry
}

/// Result type for user-only ANF conversion (functions not merged with stdlib)
/// Used for compiling user code separately from the prebuilt stdlib
type UserOnlyResult = {
    UserFunctions: ANF.Function list   // Only user functions, not merged with stdlib
    MainExpr: ANF.AExpr                // User's main expression
    TypeReg: TypeRegistry              // Merged registries (for lookups)
    VariantLookup: VariantLookup
    FuncReg: FunctionRegistry
    LocalReturnTypes: Map<string, AST.Type>
    FuncParams: Map<string, (string * AST.Type) list>
    ModuleRegistry: AST.ModuleRegistry
}

/// Registry bundle used during ANF conversion
type Registries = {
    TypeReg: TypeRegistry
    VariantLookup: VariantLookup
    FuncReg: FunctionRegistry
    FuncParams: Map<string, (string * AST.Type) list>
    ModuleRegistry: AST.ModuleRegistry
}

/// Split program into type defs, function defs, and a single expression
let splitTopLevels (program: AST.Program) : Result<AST.TypeDef list * AST.FunctionDef list * AST.Expr, string> =
    let (AST.Program topLevels) = program
    let typeDefs =
        topLevels
        |> List.choose (function AST.TypeDef t -> Some t | _ -> None)
    let functions =
        topLevels
        |> List.choose (function AST.FunctionDef f -> Some f | _ -> None)
    let expressions =
        topLevels
        |> List.choose (function AST.Expression e -> Some e | _ -> None)

    let hasMainFunc = functions |> List.exists (fun f -> f.Name = "main")
    let hasStartFunc = functions |> List.exists (fun f -> f.Name = "_start")
    if hasMainFunc then
        Error "Function name 'main' is reserved"
    elif hasStartFunc then
        Error "Function name '_start' is reserved"
    else
        match expressions with
        | [expr] -> Ok (typeDefs, functions, expr)
        | [] -> Error "Program must have a main expression"
        | _ -> Error "Multiple top-level expressions not allowed"

/// Build alias registry from type definitions
let buildAliasRegistry (typeDefs: AST.TypeDef list) : AliasRegistry =
    typeDefs
    |> List.choose (function
        | AST.TypeAlias (name, typeParams, targetType) -> Some (name, (typeParams, targetType))
        | _ -> None)
    |> Map.ofList

/// Resolve type aliases inside function definitions
let resolveAliasesInFunctions (aliasReg: AliasRegistry) (functions: AST.FunctionDef list) : AST.FunctionDef list =
    functions |> List.map (resolveAliasesInFunction aliasReg)

/// Build registries from type and function definitions
let buildRegistries
    (moduleRegistry: AST.ModuleRegistry)
    (typeDefs: AST.TypeDef list)
    (aliasReg: AliasRegistry)
    (functions: AST.FunctionDef list)
    : Registries =
    let typeRegBase : TypeRegistry =
        typeDefs
        |> List.choose (function
            | AST.RecordDef (name, _typeParams, fields) -> Some (name, fields)
            | _ -> None)
        |> Map.ofList

    let variantLookup : VariantLookup =
        typeDefs
        |> List.choose (function
            | AST.SumTypeDef (typeName, typeParams, variants) ->
                Some (typeName, typeParams, variants)
            | _ -> None)
        |> List.collect (fun (typeName, typeParams, variants) ->
            variants
            |> List.mapi (fun idx variant -> (variant.Name, (typeName, typeParams, idx, variant.Payload))))
        |> Map.ofList

    let typeReg = expandTypeRegWithAliases typeRegBase aliasReg

    let funcReg : FunctionRegistry =
        functions
        |> List.map (fun f ->
            let paramTypes = f.Params |> List.map snd
            let funcType = AST.TFunction (paramTypes, f.ReturnType)
            (f.Name, funcType))
        |> Map.ofList

    let userFuncParams : Map<string, (string * AST.Type) list> =
        functions
        |> List.map (fun f -> (f.Name, f.Params))
        |> Map.ofList

    let moduleFuncParams : Map<string, (string * AST.Type) list> =
        moduleRegistry
        |> Map.toList
        |> List.map (fun (qualifiedName, moduleFunc) ->
            let paramList = moduleFunc.ParamTypes |> List.mapi (fun i t -> ($"arg{i}", t))
            (qualifiedName, paramList))
        |> Map.ofList

    let funcParams =
        Map.fold (fun acc k v -> Map.add k v acc) userFuncParams moduleFuncParams

    {
        TypeReg = typeReg
        VariantLookup = variantLookup
        FuncReg = funcReg
        FuncParams = funcParams
        ModuleRegistry = moduleRegistry
    }

/// Merge registries with overlay taking precedence (module registry stays from base)
let mergeRegistries (baseRegs: Registries) (overlay: Registries) : Registries =
    let mergeMaps m1 m2 = Map.fold (fun acc k v -> Map.add k v acc) m1 m2
    {
        TypeReg = mergeMaps baseRegs.TypeReg overlay.TypeReg
        VariantLookup = mergeMaps baseRegs.VariantLookup overlay.VariantLookup
        FuncReg = mergeMaps baseRegs.FuncReg overlay.FuncReg
        FuncParams = mergeMaps baseRegs.FuncParams overlay.FuncParams
        ModuleRegistry = baseRegs.ModuleRegistry
    }

/// Convert functions to ANF, returning updated VarGen
let convertFunctions
    (registries: Registries)
    (varGen: ANF.VarGen)
    (functions: AST.FunctionDef list)
    : Result<ANF.Function list * ANF.VarGen, string> =
    let rec loop funcs vg acc =
        match funcs with
        | [] -> Ok (List.rev acc, vg)
        | func :: rest ->
            convertFunction func vg registries.TypeReg registries.VariantLookup registries.FuncReg registries.ModuleRegistry
            |> Result.bind (fun (anfFunc, vg') ->
                loop rest vg' (anfFunc :: acc))
    loop functions varGen []

/// Convert an expression to ANF with the given VarGen
let convertExprToAnf
    (registries: Registries)
    (varGen: ANF.VarGen)
    (expr: AST.Expr)
    : Result<ANF.AExpr * ANF.VarGen, string> =
    let emptyEnv : VarEnv = Map.empty
    toANF expr varGen emptyEnv registries.TypeReg registries.VariantLookup registries.FuncReg registries.ModuleRegistry

/// Synthesize an entrypoint function from a main expression
let synthesizeEntryFunction (name: string) (returnType: AST.Type) (body: ANF.AExpr) : ANF.Function =
    { Name = name; TypedParams = []; ReturnType = returnType; Body = body }
