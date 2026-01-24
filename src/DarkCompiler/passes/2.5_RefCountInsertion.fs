// 2.5_RefCountInsertion.fs - Reference Count Insertion Pass
//
// Inserts RefCountInc and RefCountDec operations into ANF.
//
// Design decisions:
// - Borrowed calling convention: callers retain ownership, callees borrow
// - Decrement when heap values go out of scope (end of Let body)
// - Don't decrement returned values (ownership transfers to caller)
// - Increment when extracting heap values from tuples (they become shared)
//
// The pass uses type inference from the conversion result to determine
// which TempIds hold heap-allocated values.
//
// Heap types (reference counted): Tuples, Records, Sum types, Lists, Dicts, Strings
// Stack types (NOT RC'd): Integers, Booleans, Float64, RawPtr
//
// See docs/features/reference-counting.md for detailed documentation.

module RefCountInsertion

open ANF
open AST_to_ANF

/// Type context for inferring types during RC insertion
type TypeContext = {
    TypeReg: TypeRegistry
    VariantLookup: VariantLookup
    FuncReg: FunctionRegistry
    FuncParams: Map<string, (string * AST.Type) list>
    /// Maps TempId -> Type for values we've seen
    TempTypes: Map<TempId, AST.Type>
    /// Maps TempId -> function name for closures (to resolve closure call return types)
    ClosureFuncs: Map<TempId, string>
}

/// Create initial context from conversion result
let createContext (result: ConversionResult) : TypeContext =
    { TypeReg = result.TypeReg
      VariantLookup = result.VariantLookup
      FuncReg = result.FuncReg
      FuncParams = result.FuncParams
      TempTypes = Map.empty
      ClosureFuncs = Map.empty }

/// Add a TempId -> Type mapping to context
let addType (ctx: TypeContext) (tempId: TempId) (typ: AST.Type) : TypeContext =
    { ctx with TempTypes = Map.add tempId typ ctx.TempTypes }

/// Add a closure TempId -> function name mapping to context
let addClosureFunc (ctx: TypeContext) (tempId: TempId) (funcName: string) : TypeContext =
    { ctx with ClosureFuncs = Map.add tempId funcName ctx.ClosureFuncs }

/// Try to get the function name of a closure from its TempId
let tryGetClosureFunc (ctx: TypeContext) (atom: Atom) : string option =
    match atom with
    | Var tid -> Map.tryFind tid ctx.ClosureFuncs
    | _ -> None

/// Try to get the type of a TempId
let tryGetType (ctx: TypeContext) (tempId: TempId) : AST.Type option =
    Map.tryFind tempId ctx.TempTypes

/// Try to get a function's return type from the function registry
let tryGetFuncReturnTypeFromReg (ctx: TypeContext) (funcName: string) : AST.Type option =
    match Map.tryFind funcName ctx.FuncReg with
    | Some (AST.TFunction (_, retType)) -> Some retType
    | Some otherType -> Some otherType
    | None -> None

/// Infer the type of an atom (best-effort)
let inferAtomType (ctx: TypeContext) (atom: Atom) : AST.Type option =
    match atom with
    | UnitLiteral -> Some AST.TUnit
    | IntLiteral _ -> Some AST.TInt64
    | BoolLiteral _ -> Some AST.TBool
    | StringLiteral _ -> Some AST.TString
    | FloatLiteral _ -> Some AST.TFloat64
    | Var tid -> tryGetType ctx tid
    | FuncRef funcName -> Map.tryFind funcName ctx.FuncReg

/// Infer the type of a CExpr in the given context
let inferCExprType (ctx: TypeContext) (cexpr: CExpr) : AST.Type option =
    match cexpr with
    | Atom (UnitLiteral) -> Some AST.TUnit
    | Atom (IntLiteral _) -> Some AST.TInt64
    | Atom (BoolLiteral _) -> Some AST.TBool
    | Atom (StringLiteral _) -> Some AST.TString
    | Atom (FloatLiteral _) -> Some AST.TFloat64
    | Atom (Var tid) -> tryGetType ctx tid
    | Atom (FuncRef funcName) -> Map.tryFind funcName ctx.FuncReg
    | TypedAtom (_, typ) -> Some typ  // Use the explicit type annotation
    | Prim (op, left, right) ->
        // Binary ops return int or bool depending on op
        match op with
        | Add | Sub | Mul | Div ->
            let leftType = inferAtomType ctx left
            let rightType = inferAtomType ctx right
            match leftType, rightType with
            | Some AST.TFloat64, _ -> Some AST.TFloat64
            | _, Some AST.TFloat64 -> Some AST.TFloat64
            | Some _, _ -> Some AST.TInt64
            | _, Some _ -> Some AST.TInt64
            | None, None -> None
        | Mod | Shl | Shr | BitAnd | BitOr | BitXor -> Some AST.TInt64
        | Eq | Neq | Lt | Gt | Lte | Gte | And | Or -> Some AST.TBool
    | UnaryPrim (op, atom) ->
        match op with
        | Neg ->
            match inferAtomType ctx atom with
            | Some AST.TFloat64 -> Some AST.TFloat64
            | Some _ -> Some AST.TInt64
            | None -> None
        | Not -> Some AST.TBool
        | BitNot -> Some AST.TInt64
    // Float intrinsics
    | FloatSqrt _ -> Some AST.TFloat64
    | FloatAbs _ -> Some AST.TFloat64
    | FloatNeg _ -> Some AST.TFloat64
    | Int64ToFloat _ -> Some AST.TFloat64
    | FloatToInt64 _ -> Some AST.TInt64
    | FloatToBits _ -> Some AST.TUInt64
    | FloatToString _ -> Some AST.TString
    | RandomInt64 -> Some AST.TInt64
    | DateNow -> Some AST.TInt64
    | IfValue (_, thenAtom, _) ->
        // Type is the type of the branches (should be the same)
        match thenAtom with
        | Var tid -> tryGetType ctx tid
        | UnitLiteral -> Some AST.TUnit
        | IntLiteral _ -> Some AST.TInt64
        | BoolLiteral _ -> Some AST.TBool
        | StringLiteral _ -> Some AST.TString
        | FloatLiteral _ -> Some AST.TFloat64
        | FuncRef funcName -> Map.tryFind funcName ctx.FuncReg
    | Call (funcName, args) ->
        // Return type from function registry (with special-case inference for stdlib list/tuple helpers)
        match funcName, args with
        | name, [listAtom; _] when name.StartsWith("Stdlib.List.getAt") || name.StartsWith("Stdlib.__FingerTree.getAt") ->
            match tryGetFuncReturnTypeFromReg ctx funcName with
            | Some retType -> Some retType
            | None ->
                match inferAtomType ctx listAtom with
                | Some (AST.TList elemType) ->
                    Some (AST.TSum ("Stdlib.Option.Option", [elemType]))
                | _ -> None
        | name, [listAtom] when name.StartsWith("Stdlib.List.head") || name.StartsWith("Stdlib.__FingerTree.head") ->
            match tryGetFuncReturnTypeFromReg ctx funcName with
            | Some retType -> Some retType
            | None ->
                match inferAtomType ctx listAtom with
                | Some (AST.TList elemType) ->
                    Some (AST.TSum ("Stdlib.Option.Option", [elemType]))
                | _ -> None
        | name, [listAtom] when name.StartsWith("Stdlib.List.tail") ->
            match tryGetFuncReturnTypeFromReg ctx funcName with
            | Some retType -> Some retType
            | None ->
                match inferAtomType ctx listAtom with
                | Some (AST.TList elemType) ->
                    Some (AST.TSum ("Stdlib.Option.Option", [AST.TList elemType]))
                | _ -> None
        | name, [tupleAtom] when name.StartsWith("Stdlib.Tuple2.first") ->
            match tryGetFuncReturnTypeFromReg ctx funcName with
            | Some retType -> Some retType
            | None ->
                match inferAtomType ctx tupleAtom with
                | Some (AST.TTuple (firstType :: _)) -> Some firstType
                | _ -> None
        | name, [tupleAtom] when name.StartsWith("Stdlib.Tuple2.second") ->
            match tryGetFuncReturnTypeFromReg ctx funcName with
            | Some retType -> Some retType
            | None ->
                match inferAtomType ctx tupleAtom with
                | Some (AST.TTuple (_ :: secondType :: _)) -> Some secondType
                | _ -> None
        | _ ->
            Map.tryFind funcName ctx.FuncReg
    | TailCall (funcName, _) ->
        // Tail calls have same return type as regular calls
        Map.tryFind funcName ctx.FuncReg
    | IndirectCall (funcAtom, _) ->
        // Look up the function's type to get its return type
        match funcAtom with
        | Var tid ->
            match tryGetType ctx tid with
            | Some (AST.TFunction (_, retType)) -> Some retType
            | _ -> None
        | _ -> None
    | IndirectTailCall (funcAtom, _) ->
        // Same as IndirectCall
        match funcAtom with
        | Var tid ->
            match tryGetType ctx tid with
            | Some (AST.TFunction (_, retType)) -> Some retType
            | _ -> None
        | _ -> None
    | ClosureAlloc (_, captures) ->
        // Closure is a tuple-like structure: (func_ptr, cap1, cap2, ...)
        // Return a tuple type for ref counting purposes
        let captureTypes = captures |> List.map (inferAtomType ctx)
        let concreteTypes =
            captureTypes
            |> List.map (function
                | Some typ -> typ
                | None ->
                    Crash.crash "RefCountInsertion: could not infer closure capture types")
        Some (AST.TTuple (AST.TInt64 :: concreteTypes))
    | ClosureCall (closureAtom, _) ->
        // Try to find the closure's function name and look up return type
        match tryGetClosureFunc ctx closureAtom with
        | Some funcName -> Map.tryFind funcName ctx.FuncReg
        | None ->
            // Fallback: infer from closure's type (TFunction)
            match closureAtom with
            | Var tid ->
                match tryGetType ctx tid with
                | Some (AST.TFunction (_, retType)) -> Some retType
                | _ -> None
            | _ -> None
    | ClosureTailCall (closureAtom, _) ->
        // Same as ClosureCall
        match tryGetClosureFunc ctx closureAtom with
        | Some funcName -> Map.tryFind funcName ctx.FuncReg
        | None ->
            match closureAtom with
            | Var tid ->
                match tryGetType ctx tid with
                | Some (AST.TFunction (_, retType)) -> Some retType
                | _ -> None
            | _ -> None
    | TupleAlloc elems ->
        // Infer element types and create TTuple
        let elemTypes =
            elems
            |> List.map (function
                | UnitLiteral -> AST.TUnit
                | IntLiteral _ -> AST.TInt64
                | BoolLiteral _ -> AST.TBool
                | StringLiteral _ -> AST.TString
                | FloatLiteral _ -> AST.TFloat64
                | Var tid ->
                    match tryGetType ctx tid with
                    | Some t -> t
                    | None -> Crash.crash $"RefCountInsertion: Type not found for temp {tid} in TupleAlloc"
                | FuncRef funcName ->
                    match Map.tryFind funcName ctx.FuncReg with
                    | Some t -> t
                    | None -> Crash.crash $"RefCountInsertion: Type not found for function {funcName} in TupleAlloc")
        Some (AST.TTuple elemTypes)
    | TupleGet (tupleAtom, index) ->
        // Get element type from tuple type
        match tupleAtom with
        | Var tid ->
            match tryGetType ctx tid with
            | Some (AST.TTuple elemTypes) when index < List.length elemTypes ->
                Some (List.item index elemTypes)
            | Some (AST.TRecord (typeName, _)) ->
                // Record fields - look up field type
                match Map.tryFind typeName ctx.TypeReg with
                | Some fields when index < List.length fields ->
                    Some (snd (List.item index fields))
                | _ -> None
            | Some (AST.TList elemType) ->
                // List Cons cells are (tag, head, tail) - index 1 is head, index 2 is tail
                match index with
                | 0 -> Some AST.TInt64  // tag
                | 1 -> Some elemType    // head element
                | 2 -> Some (AST.TList elemType)  // tail is same list type
                | _ -> None
            | Some (AST.TSum (_typeName, typeArgs)) ->
                // Sum type layout: [tag:8][payload:8]
                // index 0 = tag (Int64), index 1 = payload
                match index with
                | 0 -> Some AST.TInt64  // tag
                | 1 ->
                    // Payload type depends on variant, but for simple cases like Option<T>,
                    // the payload type is the first type argument
                    match typeArgs with
                    | [singleType] -> Some singleType
                    | _ -> None
                | _ -> None
            | Some (AST.TFunction _) ->
                // Closures are typed as TFunction but laid out as tuples:
                // [func_ptr:8][cap1:8][cap2:8]...
                // Index 0 is the function pointer (Int64), rest are captures
                Some AST.TInt64  // All closure slots are pointer-sized
            | _ -> None
        | _ -> None
    | StringConcat (_, _) -> Some AST.TString  // String concatenation returns a string
    | RefCountInc (_, _) -> Some AST.TUnit
    | RefCountDec (_, _) -> Some AST.TUnit
    | Print (_, valueType) -> Some valueType  // Print returns the type it prints
    | FileReadText _ -> Some (AST.TSum ("Stdlib.Result.Result", [AST.TString; AST.TString]))  // Result<String, String>
    | FileExists _ -> Some AST.TBool  // Bool
    | FileWriteText _ -> Some (AST.TSum ("Stdlib.Result.Result", [AST.TUnit; AST.TString]))  // Result<Unit, String>
    | FileAppendText _ -> Some (AST.TSum ("Stdlib.Result.Result", [AST.TUnit; AST.TString]))  // Result<Unit, String>
    | FileDelete _ -> Some (AST.TSum ("Stdlib.Result.Result", [AST.TUnit; AST.TString]))  // Result<Unit, String>
    | FileSetExecutable _ -> Some (AST.TSum ("Stdlib.Result.Result", [AST.TUnit; AST.TString]))  // Result<Unit, String>
    | FileWriteFromPtr _ -> Some AST.TBool  // Returns Bool (success/failure)
    // Raw memory intrinsics (no ref counting - manually managed)
    | RawAlloc _ -> Some AST.TRawPtr  // Returns raw pointer
    | RawFree _ -> Some AST.TUnit  // Returns unit
    | RawGet (_, _, valueType) -> valueType
    | RawGetByte _ -> Some AST.TInt64  // Returns 1-byte value (zero-extended)
    | RawSet _ -> Some AST.TUnit  // Returns unit
    | RawSetByte _ -> Some AST.TUnit  // Returns unit
    // String refcount intrinsics
    | RefCountIncString _ -> Some AST.TUnit  // Returns unit
    | RefCountDecString _ -> Some AST.TUnit  // Returns unit

/// Return analysis annotation for AExpr nodes
type ReturnAnnotatedExpr =
    | RReturn of Atom * Set<TempId>
    | RLet of TempId * CExpr * ReturnAnnotatedExpr * Set<TempId>
    | RIf of Atom * ReturnAnnotatedExpr * ReturnAnnotatedExpr * Set<TempId>

/// Get the set of returned TempIds for a return-annotated expression
let returnedSet (expr: ReturnAnnotatedExpr) : Set<TempId> =
    match expr with
    | RReturn (_, returned) -> returned
    | RLet (_, _, _, returned) -> returned
    | RIf (_, _, _, returned) -> returned

/// Collect the alias chain for a TempId (includes the TempId itself)
let rec collectAliasChain (aliases: Map<TempId, TempId>) (tempId: TempId) : Set<TempId> =
    match Map.tryFind tempId aliases with
    | Some nextId -> Set.add tempId (collectAliasChain aliases nextId)
    | None -> Set.singleton tempId

/// Analyze return values and track alias chains in a single pass
let rec analyzeReturns (aliases: Map<TempId, TempId>) (expr: AExpr) : ReturnAnnotatedExpr =
    match expr with
    | Return atom ->
        let returned =
            match atom with
            | Var tid -> collectAliasChain aliases tid
            | _ -> Set.empty
        RReturn (atom, returned)
    | Let (tempId, cexpr, body) ->
        let aliases' =
            match cexpr with
            | Atom (Var sourceId) -> Map.add tempId sourceId aliases
            | _ -> aliases
        let bodyInfo = analyzeReturns aliases' body
        RLet (tempId, cexpr, bodyInfo, returnedSet bodyInfo)
    | If (cond, thenBranch, elseBranch) ->
        let thenInfo = analyzeReturns aliases thenBranch
        let elseInfo = analyzeReturns aliases elseBranch
        let returned = Set.union (returnedSet thenInfo) (returnedSet elseInfo)
        RIf (cond, thenInfo, elseInfo, returned)

/// Check if a CExpr is a borrowing/aliasing operation
/// Borrowed/aliased values should NOT get their own RefCountDec - the original value owns the memory
let isBorrowingExpr (cexpr: CExpr) : bool =
    match cexpr with
    | TupleGet _ -> true           // Extracts pointer from tuple/list - borrowed from parent
    | Atom (Var _) -> true         // Alias/copy of existing variable - don't double-dec
    | TypedAtom (Var _, _) -> true // TypedAtom wrapping a variable - also borrowed
    | Call (funcName, _) ->
        // List element extraction functions return borrowed references
        funcName = "Stdlib.__FingerTree.headUnsafe_i64" ||
        funcName = "Stdlib.__FingerTree.head_i64" ||
        funcName = "Stdlib.__FingerTree.head"
    | _ -> false

/// Insert RefCountInc for returned parameters at a Return node
let insertParamIncsAtReturn
    (paramIncs: (TempId * int) list)
    (returned: Set<TempId>)
    (expr: AExpr)
    (varGen: VarGen)
    (types: Map<TempId, AST.Type>)
    : AExpr * VarGen * Map<TempId, AST.Type> =
    let active =
        paramIncs
        |> List.filter (fun (tempId, _) -> Set.contains tempId returned)
    List.foldBack
        (fun (tempId, size) (accExpr, accVarGen, accTypes) ->
            let (dummyId, varGen') = freshVar accVarGen
            let incExpr = RefCountInc (Var tempId, size)
            let accExpr' = Let (dummyId, incExpr, accExpr)
            (accExpr', varGen', Map.add dummyId AST.TUnit accTypes))
        active
        (expr, varGen, types)

/// Insert RefCountDec operations before a Return using the current dec stack
let insertReturnDecs
    (returnDecs: (TempId * int) list)
    (expr: AExpr)
    (varGen: VarGen)
    (types: Map<TempId, AST.Type>)
    : AExpr * VarGen * Map<TempId, AST.Type> =
    let decsInOrder = List.rev returnDecs
    List.fold
        (fun (accExpr, accVarGen, accTypes) (tempId, size) ->
            let (dummyId, varGen') = freshVar accVarGen
            let decExpr = RefCountDec (Var tempId, size)
            let accExpr' = Let (dummyId, decExpr, accExpr)
            (accExpr', varGen', Map.add dummyId AST.TUnit accTypes))
        (expr, varGen, types)
        decsInOrder

/// Stored state for rebuilding a Let while unwinding an expression spine
type LetFrame = {
    TempId: TempId
    CExpr: CExpr
    Ctx: TypeContext
    MaybeType: AST.Type option
    BodyReturned: Set<TempId>
    BindingTypes: Map<TempId, AST.Type>
}

/// Apply a single Let frame around an expression (uses current varGen/types)
let applyLetFrame
    (frame: LetFrame)
    (expr: AExpr, varGen: VarGen, types: Map<TempId, AST.Type>)
    : AExpr * VarGen * Map<TempId, AST.Type> =
    let (incBindings, varGen1) =
        match frame.CExpr with
        | TupleAlloc elems ->
            let rec collectIncs (atoms: Atom list) (vg: VarGen) (acc: (TempId * CExpr) list) =
                match atoms with
                | [] -> (List.rev acc, vg)
                | atom :: rest ->
                    match atom with
                    | Var tid ->
                        match tryGetType frame.Ctx tid with
                        | Some t when isHeapType t ->
                            let size = payloadSize t frame.Ctx.TypeReg
                            let (dummyId, vg') = freshVar vg
                            collectIncs rest vg' ((dummyId, RefCountInc (Var tid, size)) :: acc)
                        | _ ->
                            collectIncs rest vg acc
                    | _ ->
                        collectIncs rest vg acc
            collectIncs elems varGen []
        | _ -> ([], varGen)

    let typesWithIncs =
        incBindings
        |> List.fold (fun m (tid, _) -> Map.add tid AST.TUnit m) types

    let (returnIncBinding, varGen2, typesWithReturnInc) =
        match frame.MaybeType with
        | Some t when isHeapType t && Set.contains frame.TempId frame.BodyReturned && isBorrowingExpr frame.CExpr ->
            let size = payloadSize t frame.Ctx.TypeReg
            let (incId, vg) = freshVar varGen1
            let incExpr = RefCountInc (Var frame.TempId, size)
            ([(incId, incExpr)], vg, Map.add incId AST.TUnit typesWithIncs)
        | _ ->
            ([], varGen1, typesWithIncs)

    let bodyWithReturnInc = wrapBindings returnIncBinding expr
    let letExpr = Let (frame.TempId, frame.CExpr, bodyWithReturnInc)
    let exprWithIncs = wrapBindings incBindings letExpr
    let mergedTypes = Map.fold (fun m k v -> Map.add k v m) frame.BindingTypes typesWithReturnInc
    (exprWithIncs, varGen2, mergedTypes)

/// Apply a stack of Let frames (innermost-first)
let applyLetFrames
    (frames: LetFrame list)
    (expr: AExpr, varGen: VarGen, types: Map<TempId, AST.Type>)
    : AExpr * VarGen * Map<TempId, AST.Type> =
    List.fold (fun state frame -> applyLetFrame frame state) (expr, varGen, types) frames

/// Insert reference counting operations using return analysis and a dec stack
/// Returns (transformed expr, varGen, types defined in this subtree)
let rec insertRCWithAnalysis
    (ctx: TypeContext)
    (expr: ReturnAnnotatedExpr)
    (varGen: VarGen)
    (returnDecs: (TempId * int) list)
    (paramIncs: (TempId * int) list)
    : AExpr * VarGen * Map<TempId, AST.Type> =
    let rec descend
        (ctx: TypeContext)
        (expr: ReturnAnnotatedExpr)
        (varGen: VarGen)
        (returnDecs: (TempId * int) list)
        (frames: LetFrame list)
        : AExpr * VarGen * Map<TempId, AST.Type> =
        match expr with
        | RReturn (atom, returned) ->
            let baseExpr = Return atom
            let (withParamIncs, varGen1, types1) = insertParamIncsAtReturn paramIncs returned baseExpr varGen Map.empty
            let (withDecs, varGen2, types2) = insertReturnDecs returnDecs withParamIncs varGen1 types1
            applyLetFrames frames (withDecs, varGen2, types2)

        | RIf (cond, thenBranch, elseBranch, _) ->
            let (thenBranch', varGen1, types1) = insertRCWithAnalysis ctx thenBranch varGen returnDecs paramIncs
            let (elseBranch', varGen2, types2) = insertRCWithAnalysis ctx elseBranch varGen1 returnDecs paramIncs
            let mergedTypes = Map.fold (fun m k v -> Map.add k v m) types1 types2
            applyLetFrames frames (If (cond, thenBranch', elseBranch'), varGen2, mergedTypes)

        | RLet (tempId, cexpr, bodyInfo, _) ->
            // First, infer the type of this binding and add to context
            let maybeType = inferCExprType ctx cexpr
            // Use a TypedAtom alias in the body to preserve the intended payload type
            // when TupleGet cannot infer it from a multi-parameter sum.
            let inferredType =
                match maybeType with
                | Some t -> t
                | None ->
                    match cexpr, bodyInfo with
                    | TupleGet _, RLet (_, TypedAtom (Var sourceId, aliasType), _, _) when sourceId = tempId ->
                        aliasType
                    | _ ->
                        // Fallback for cases where inference is still incomplete.
                        // TODO: remove this once all CExprs are fully typed.
                        AST.TInt64
            let ctx' = addType ctx tempId inferredType
            let ctxWithAliases =
                match cexpr with
                | TypedAtom (Var sourceId, aliasType) -> addType ctx' sourceId aliasType
                | _ -> ctx'

            let bindingTypes =
                match cexpr with
                | TypedAtom (Var sourceId, aliasType) ->
                    Map.empty |> Map.add tempId inferredType |> Map.add sourceId aliasType
                | _ ->
                    Map.add tempId inferredType Map.empty

            // Track closure function names for later ClosureCall type resolution
            let ctx'' =
                match cexpr with
                | ClosureAlloc (funcName, _) -> addClosureFunc ctxWithAliases tempId funcName
                | _ -> ctxWithAliases

            let bodyReturned = returnedSet bodyInfo
            let returnDecs' =
                match maybeType with
                | Some t when isHeapType t && not (Set.contains tempId bodyReturned) && not (isBorrowingExpr cexpr) ->
                    let size = payloadSize t ctx'.TypeReg
                    (tempId, size) :: returnDecs
                | _ -> returnDecs

            let frame = {
                TempId = tempId
                CExpr = cexpr
                Ctx = ctx
                MaybeType = maybeType
                BodyReturned = bodyReturned
                BindingTypes = bindingTypes
            }

            // Process the body iteratively, then rebuild on the way back out
            descend ctx'' bodyInfo varGen returnDecs' (frame :: frames)

    descend ctx expr varGen returnDecs []

/// Insert reference counting operations into an AExpr
/// Returns (transformed expr, varGen, accumulated TempTypes)
let insertRC (ctx: TypeContext) (expr: AExpr) (varGen: VarGen) : AExpr * VarGen * Map<TempId, AST.Type> =
    let analyzed = analyzeReturns Map.empty expr
    insertRCWithAnalysis ctx analyzed varGen [] []

/// Insert RC operations into a function
/// Returns (transformed function, varGen, accumulated TempTypes)
let insertRCInFunction (ctx: TypeContext) (func: Function) (varGen: VarGen) : Function * VarGen * Map<TempId, AST.Type> =
    // Add parameter types to context (types are now bundled in TypedParams)
    let ctx' =
        func.TypedParams
        |> List.fold (fun c tp -> addType c tp.Id tp.Type) ctx

    let bodyInfo = analyzeReturns Map.empty func.Body
    let paramIncs =
        func.TypedParams
        |> List.choose (fun param ->
            match param.Type with
            | AST.TDict _ -> None  // Dict pointers are tagged; RC ops expect raw pointers.
            | _ when isHeapType param.Type ->
                let size = payloadSize param.Type ctx'.TypeReg
                Some (param.Id, size)
            | _ -> None)

    // Process function body with return analysis
    let (body', varGen', accTypes) = insertRCWithAnalysis ctx' bodyInfo varGen [] paramIncs
    let paramTypes =
        func.TypedParams
        |> List.fold (fun m tp -> Map.add tp.Id tp.Type m) Map.empty
    let mergedTypes = Map.fold (fun m k v -> Map.add k v m) paramTypes accTypes
    ({ func with Body = body' }, varGen', mergedTypes)

// ============================================================================
// TypeMap Completeness Verification
// ============================================================================

/// Collect all TempIds defined in an AExpr (from Let bindings)
let rec collectDefinedTempIds (expr: AExpr) : Set<TempId> =
    match expr with
    | Return _ -> Set.empty
    | Let (tempId, _, body) ->
        Set.add tempId (collectDefinedTempIds body)
    | If (_, thenBranch, elseBranch) ->
        Set.union (collectDefinedTempIds thenBranch) (collectDefinedTempIds elseBranch)

/// Collect all TempIds defined in a function (params + body)
let collectFunctionTempIds (func: Function) : Set<TempId> =
    let paramIds = func.TypedParams |> List.map (fun tp -> tp.Id) |> Set.ofList
    Set.union paramIds (collectDefinedTempIds func.Body)

/// Collect all TempIds defined in a program
let collectProgramTempIds (ANF.Program (functions, mainExpr)) : Set<TempId> =
    let funcIds = functions |> List.map collectFunctionTempIds |> Set.unionMany
    Set.union funcIds (collectDefinedTempIds mainExpr)

/// Verify that all defined TempIds have types in the TypeMap
/// Returns a list of TempIds that are missing from the TypeMap
let verifyTypeMapCompleteness (program: ANF.Program) (typeMap: ANF.TypeMap) : TempId list =
    let definedIds = collectProgramTempIds program
    let typedIds = typeMap |> Map.keys |> Set.ofSeq
    let missing = Set.difference definedIds typedIds
    Set.toList missing

/// Insert RC operations into a program
/// Returns (ANF.Program, TypeMap) where TypeMap contains all TempId -> Type mappings
let insertRCInProgram (result: ConversionResult) : Result<ANF.Program * ANF.TypeMap, string> =
    let ctx = createContext result
    let (ANF.Program (functions, mainExpr)) = result.Program
    let varGen = VarGen 1000  // Start high to avoid conflicts

    // Process all functions, accumulating types
    let rec processFuncs (funcs: Function list) (vg: VarGen) (accFuncs: Function list) (accTypes: Map<TempId, AST.Type>) : Function list * VarGen * Map<TempId, AST.Type> =
        match funcs with
        | [] -> (List.rev accFuncs, vg, accTypes)
        | f :: rest ->
            let (f', vg', types) = insertRCInFunction ctx f vg
            // Merge accumulated types
            let mergedTypes = Map.fold (fun m k v -> Map.add k v m) accTypes types
            processFuncs rest vg' (f' :: accFuncs) mergedTypes

    let (functions', varGen1, typesFromFuncs) = processFuncs functions varGen [] Map.empty

    // Process main expression
    let (mainExpr', _, typesFromMain) = insertRC ctx mainExpr varGen1

    // Final merged TypeMap
    let finalTypeMap = Map.fold (fun m k v -> Map.add k v m) typesFromFuncs typesFromMain

    // Verify TypeMap completeness - all defined TempIds should have types
    let program' = ANF.Program (functions', mainExpr')
    let missingTypes = verifyTypeMapCompleteness program' finalTypeMap
    if not (List.isEmpty missingTypes) then
        let missingStr = missingTypes |> List.map (fun (TempId n) -> $"t{n}") |> String.concat ", "
        Crash.crash $"RefCountInsertion: TypeMap incomplete - missing types for: {missingStr}"

    Ok (program', finalTypeMap)
