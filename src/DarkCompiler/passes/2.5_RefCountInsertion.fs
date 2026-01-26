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
open System.Diagnostics

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

/// Cached CExpr type inference results
type CExprTypeCache = Map<CExpr, AST.Type option>

/// Timing counter for RC insertion profiling
type TimingCounter = {
    Ms: float
    Calls: int
}

/// Fine-grained timing breakdown for RC insertion
type RcTiming = {
    AnalyzeReturns: TimingCounter
    CollectAliasChain: TimingCounter
    InsertRC: TimingCounter
    InferCExprType: TimingCounter
    PayloadSize: TimingCounter
    InsertParamIncs: TimingCounter
    InsertReturnDecs: TimingCounter
    ApplyLetFrame: TimingCounter
    ApplyLetFrames: TimingCounter
    MergeTypes: TimingCounter
    VerifyTypeMap: TimingCounter
}

let emptyTimingCounter : TimingCounter = { Ms = 0.0; Calls = 0 }

let emptyRcTiming : RcTiming = {
    AnalyzeReturns = emptyTimingCounter
    CollectAliasChain = emptyTimingCounter
    InsertRC = emptyTimingCounter
    InferCExprType = emptyTimingCounter
    PayloadSize = emptyTimingCounter
    InsertParamIncs = emptyTimingCounter
    InsertReturnDecs = emptyTimingCounter
    ApplyLetFrame = emptyTimingCounter
    ApplyLetFrames = emptyTimingCounter
    MergeTypes = emptyTimingCounter
    VerifyTypeMap = emptyTimingCounter
}

let emptyCExprTypeCache : CExprTypeCache = Map.empty

let private addTiming (elapsedMs: float) (counter: TimingCounter) : TimingCounter =
    { Ms = counter.Ms + elapsedMs; Calls = counter.Calls + 1 }

let private addAnalyzeReturns elapsed timing =
    { timing with AnalyzeReturns = addTiming elapsed timing.AnalyzeReturns }

let private addCollectAliasChain elapsed timing =
    { timing with CollectAliasChain = addTiming elapsed timing.CollectAliasChain }

let private addInsertRC elapsed timing =
    { timing with InsertRC = addTiming elapsed timing.InsertRC }

let private addInferCExprType elapsed timing =
    { timing with InferCExprType = addTiming elapsed timing.InferCExprType }

let private addPayloadSize elapsed timing =
    { timing with PayloadSize = addTiming elapsed timing.PayloadSize }

let private addInsertParamIncs elapsed timing =
    { timing with InsertParamIncs = addTiming elapsed timing.InsertParamIncs }

let private addInsertReturnDecs elapsed timing =
    { timing with InsertReturnDecs = addTiming elapsed timing.InsertReturnDecs }

let private addApplyLetFrame elapsed timing =
    { timing with ApplyLetFrame = addTiming elapsed timing.ApplyLetFrame }

let private addApplyLetFrames elapsed timing =
    { timing with ApplyLetFrames = addTiming elapsed timing.ApplyLetFrames }

let private addVerifyTypeMap elapsed timing =
    { timing with VerifyTypeMap = addTiming elapsed timing.VerifyTypeMap }

let private timeWithTiming
    (swOpt: Stopwatch option)
    (update: float -> RcTiming -> RcTiming)
    (f: RcTiming -> 'a * RcTiming)
    (timing: RcTiming)
    : 'a * RcTiming =
    match swOpt with
    | None -> f timing
    | Some sw ->
        let start = sw.Elapsed.TotalMilliseconds
        let (result, timing1) = f timing
        let elapsed = sw.Elapsed.TotalMilliseconds - start
        (result, update elapsed timing1)

let private timeWith
    (swOpt: Stopwatch option)
    (update: float -> RcTiming -> RcTiming)
    (f: unit -> 'a)
    (timing: RcTiming)
    : 'a * RcTiming =
    timeWithTiming swOpt update (fun t -> (f (), t)) timing

/// Create initial context from conversion result
let createContext (result: ConversionResult) : TypeContext =
    { TypeReg = result.TypeReg
      VariantLookup = result.VariantLookup
      FuncReg = result.FuncReg
      FuncParams = result.FuncParams
      TempTypes = Map.empty
      ClosureFuncs = Map.empty }

let private withTempTypes (ctx: TypeContext) (types: Map<TempId, AST.Type>) : TypeContext =
    { ctx with TempTypes = types }

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
let rec analyzeReturns
    (swOpt: Stopwatch option)
    (timing: RcTiming)
    (aliases: Map<TempId, TempId>)
    (expr: AExpr)
    : ReturnAnnotatedExpr * RcTiming =
    match expr with
    | Return atom ->
        let (returned, timing1) =
            match atom with
            | Var tid -> timeWith swOpt addCollectAliasChain (fun () -> collectAliasChain aliases tid) timing
            | _ -> (Set.empty, timing)
        (RReturn (atom, returned), timing1)
    | Let (tempId, cexpr, body) ->
        let aliases' =
            match cexpr with
            | Atom (Var sourceId) -> Map.add tempId sourceId aliases
            | _ -> aliases
        let (bodyInfo, timing1) = analyzeReturns swOpt timing aliases' body
        (RLet (tempId, cexpr, bodyInfo, returnedSet bodyInfo), timing1)
    | If (cond, thenBranch, elseBranch) ->
        let (thenInfo, timing1) = analyzeReturns swOpt timing aliases thenBranch
        let (elseInfo, timing2) = analyzeReturns swOpt timing1 aliases elseBranch
        let returned = Set.union (returnedSet thenInfo) (returnedSet elseInfo)
        (RIf (cond, thenInfo, elseInfo, returned), timing2)

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
    TupleIncTargets: (TempId * int) list
    ReturnIncSize: int option
}

let private splitTimingResult
    (expr: AExpr)
    (varGen: VarGen)
    (types: Map<TempId, AST.Type>)
    (timing: RcTiming)
    : (AExpr * VarGen * Map<TempId, AST.Type>) * RcTiming =
    ((expr, varGen, types), timing)

/// Apply a single Let frame around an expression (uses current varGen/types)
let applyLetFrame
    (frame: LetFrame)
    (expr: AExpr, varGen: VarGen, types: Map<TempId, AST.Type>)
    (timing: RcTiming)
    : AExpr * VarGen * Map<TempId, AST.Type> * RcTiming =
    let (incBindingsRev, varGen1) =
        frame.TupleIncTargets
        |> List.fold (fun (acc, vg) (tid, size) ->
            let (dummyId, vg') = freshVar vg
            ((dummyId, RefCountInc (Var tid, size)) :: acc, vg')) ([], varGen)
    let incBindings = List.rev incBindingsRev

    let typesWithIncs =
        incBindings
        |> List.fold (fun m (tid, _) -> Map.add tid AST.TUnit m) types

    let (returnIncBinding, varGen2, typesWithReturnInc) =
        match frame.ReturnIncSize with
        | Some size ->
            let (incId, vg) = freshVar varGen1
            let incExpr = RefCountInc (Var frame.TempId, size)
            ([(incId, incExpr)], vg, Map.add incId AST.TUnit typesWithIncs)
        | None ->
            ([], varGen1, typesWithIncs)

    let bodyWithReturnInc = wrapBindings returnIncBinding expr
    let letExpr = Let (frame.TempId, frame.CExpr, bodyWithReturnInc)
    let exprWithIncs = wrapBindings incBindings letExpr
    (exprWithIncs, varGen2, typesWithReturnInc, timing)

/// Apply a stack of Let frames (innermost-first)
let applyLetFrames
    (swOpt: Stopwatch option)
    (frames: LetFrame list)
    (expr: AExpr, varGen: VarGen, types: Map<TempId, AST.Type>)
    (timing: RcTiming)
    : AExpr * VarGen * Map<TempId, AST.Type> * RcTiming =
    let folder
        ((accExpr, accVarGen, accTypes, accTiming): AExpr * VarGen * Map<TempId, AST.Type> * RcTiming)
        (frame: LetFrame)
        : AExpr * VarGen * Map<TempId, AST.Type> * RcTiming =
        let (result, timing1) =
            timeWithTiming swOpt addApplyLetFrame
                (fun t ->
                    let (expr', varGen', types', timing') = applyLetFrame frame (accExpr, accVarGen, accTypes) t
                    splitTimingResult expr' varGen' types' timing')
                accTiming
        let (expr', varGen', types') = result
        (expr', varGen', types', timing1)
    List.fold folder (expr, varGen, types, timing) frames

/// Insert reference counting operations using return analysis and a dec stack
/// Returns (transformed expr, varGen, types defined in this subtree)
let rec insertRCWithAnalysis
    (ctx: TypeContext)
    (expr: ReturnAnnotatedExpr)
    (varGen: VarGen)
    (returnDecs: (TempId * int) list)
    (paramIncs: (TempId * int) list)
    (swOpt: Stopwatch option)
    (timing: RcTiming)
    (types: Map<TempId, AST.Type>)
    (typeCache: CExprTypeCache)
    : AExpr * VarGen * Map<TempId, AST.Type> * CExprTypeCache * RcTiming =
    let ctxWithTypes = withTempTypes ctx types
    let rec descend
        (ctx: TypeContext)
        (expr: ReturnAnnotatedExpr)
        (varGen: VarGen)
        (returnDecs: (TempId * int) list)
        (frames: LetFrame list)
        (timing: RcTiming)
        (types: Map<TempId, AST.Type>)
        (typeCache: CExprTypeCache)
        : AExpr * VarGen * Map<TempId, AST.Type> * CExprTypeCache * RcTiming =
        match expr with
        | RReturn (atom, returned) ->
            let baseExpr = Return atom
            let (withParamIncsResult, timing1) =
                timeWith swOpt addInsertParamIncs (fun () -> insertParamIncsAtReturn paramIncs returned baseExpr varGen types) timing
            let (withParamIncs, varGen1, types1) = withParamIncsResult
            let (withDecsResult, timing2) =
                timeWith swOpt addInsertReturnDecs (fun () -> insertReturnDecs returnDecs withParamIncs varGen1 types1) timing1
            let (withDecs, varGen2, types2) = withDecsResult
            let (result, timing3) =
                timeWithTiming swOpt addApplyLetFrames
                    (fun t ->
                        let (expr', varGen', types', timing') = applyLetFrames swOpt frames (withDecs, varGen2, types2) t
                        splitTimingResult expr' varGen' types' timing')
                    timing2
            let (finalExpr, finalVarGen, finalTypes) = result
            (finalExpr, finalVarGen, finalTypes, typeCache, timing3)

        | RIf (cond, thenBranch, elseBranch, _) ->
            let (thenBranch', varGen1, types1, typeCache1, timing1) =
                insertRCWithAnalysis ctx thenBranch varGen returnDecs paramIncs swOpt timing types typeCache
            let (elseBranch', varGen2, types2, typeCache2, timing2) =
                insertRCWithAnalysis ctx elseBranch varGen1 returnDecs paramIncs swOpt timing1 types1 typeCache1
            let (result, timing4) =
                timeWithTiming swOpt addApplyLetFrames
                    (fun t ->
                        let (expr', varGen', types', timing') =
                            applyLetFrames swOpt frames (If (cond, thenBranch', elseBranch'), varGen2, types2) t
                        splitTimingResult expr' varGen' types' timing')
                    timing2
            let (finalExpr, finalVarGen, finalTypes) = result
            (finalExpr, finalVarGen, finalTypes, typeCache2, timing4)

        | RLet (tempId, cexpr, bodyInfo, _) ->
            // First, infer the type of this binding and add to context
            let (maybeType, typeCache1, timing1) =
                match Map.tryFind cexpr typeCache with
                | Some cached -> (cached, typeCache, timing)
                | None ->
                    let (inferred, timing1) =
                        timeWith swOpt addInferCExprType (fun () -> inferCExprType ctx cexpr) timing
                    (inferred, Map.add cexpr inferred typeCache, timing1)
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

            let typesWithBinding =
                match cexpr with
                | TypedAtom (Var sourceId, aliasType) ->
                    types |> Map.add tempId inferredType |> Map.add sourceId aliasType
                | _ ->
                    Map.add tempId inferredType types

            let ctxWithTypes = withTempTypes ctx typesWithBinding

            // Track closure function names for later ClosureCall type resolution
            let ctx'' =
                match cexpr with
                | ClosureAlloc (funcName, _) -> addClosureFunc ctxWithTypes tempId funcName
                | _ -> ctxWithTypes

            let bodyReturned = returnedSet bodyInfo
            let (returnDecs', timing2) =
                match maybeType with
                | Some t when isHeapType t && not (Set.contains tempId bodyReturned) && not (isBorrowingExpr cexpr) ->
                    let (size, timing2) =
                        timeWith swOpt addPayloadSize (fun () -> payloadSize t ctx.TypeReg) timing1
                    ((tempId, size) :: returnDecs, timing2)
                | _ -> (returnDecs, timing1)

            let (tupleIncTargets, timing3) =
                match cexpr with
                | TupleAlloc elems ->
                    elems
                    |> List.fold (fun (acc, timingAcc) atom ->
                        match atom with
                        | Var tid ->
                            match tryGetType ctx tid with
                            | Some t when isHeapType t ->
                                let (size, timing1) =
                                    timeWith swOpt addPayloadSize (fun () -> payloadSize t ctx.TypeReg) timingAcc
                                ((tid, size) :: acc, timing1)
                            | _ -> (acc, timingAcc)
                        | _ -> (acc, timingAcc)
                    ) ([], timing2)
                    |> fun (acc, timingAcc) -> (List.rev acc, timingAcc)
                | _ -> ([], timing2)

            let (returnIncSize, timing4) =
                match maybeType with
                | Some t when isHeapType t && Set.contains tempId bodyReturned && isBorrowingExpr cexpr ->
                    let (size, timing4) =
                        timeWith swOpt addPayloadSize (fun () -> payloadSize t ctx.TypeReg) timing3
                    (Some size, timing4)
                | _ -> (None, timing3)

            let frame = {
                TempId = tempId
                CExpr = cexpr
                TupleIncTargets = tupleIncTargets
                ReturnIncSize = returnIncSize
            }

            // Process the body iteratively, then rebuild on the way back out
            descend ctx'' bodyInfo varGen returnDecs' (frame :: frames) timing4 typesWithBinding typeCache1

    descend ctxWithTypes expr varGen returnDecs [] timing types typeCache

/// Insert reference counting operations into an AExpr with optional timing
/// Returns (transformed expr, varGen, accumulated TempTypes, timing)
let private insertRCInternal
    (ctx: TypeContext)
    (expr: AExpr)
    (varGen: VarGen)
    (swOpt: Stopwatch option)
    (timing: RcTiming)
    (types: Map<TempId, AST.Type>)
    (typeCache: CExprTypeCache)
    : AExpr * VarGen * Map<TempId, AST.Type> * CExprTypeCache * RcTiming =
    let ctxWithTypes = withTempTypes ctx types
    let (analyzed, timing1) =
        timeWithTiming swOpt addAnalyzeReturns (fun t -> analyzeReturns swOpt t Map.empty expr) timing
    let (result, timing2) =
        timeWithTiming swOpt addInsertRC
            (fun t ->
                let (expr', varGen', types', typeCache', timing') =
                    insertRCWithAnalysis ctxWithTypes analyzed varGen [] [] swOpt t types typeCache
                ((expr', varGen', types', typeCache'), timing'))
            timing1
    let (expr', varGen', types', typeCache') = result
    (expr', varGen', types', typeCache', timing2)

/// Insert reference counting operations into an AExpr
/// Returns (transformed expr, varGen, accumulated TempTypes)
let insertRC (ctx: TypeContext) (expr: AExpr) (varGen: VarGen) : AExpr * VarGen * Map<TempId, AST.Type> =
    let (expr', varGen', types', _typeCache, _timing) =
        insertRCInternal ctx expr varGen None emptyRcTiming Map.empty emptyCExprTypeCache
    (expr', varGen', types')

/// Insert RC operations into a function with optional timing
/// Returns (transformed function, varGen, accumulated TempTypes, timing)
let private insertRCInFunctionInternal
    (ctx: TypeContext)
    (func: Function)
    (varGen: VarGen)
    (swOpt: Stopwatch option)
    (timing: RcTiming)
    (types: Map<TempId, AST.Type>)
    (typeCache: CExprTypeCache)
    : Function * VarGen * Map<TempId, AST.Type> * CExprTypeCache * RcTiming =
    let typesWithParams =
        func.TypedParams
        |> List.fold (fun m tp -> Map.add tp.Id tp.Type m) types
    let ctxWithParams = withTempTypes ctx typesWithParams

    let (bodyInfo, timing1) =
        timeWithTiming swOpt addAnalyzeReturns (fun t -> analyzeReturns swOpt t Map.empty func.Body) timing
    let (paramIncsRev, timing2) =
        func.TypedParams
        |> List.fold (fun (acc, timingAcc) param ->
            match param.Type with
            | AST.TDict _ -> (acc, timingAcc)  // Dict pointers are tagged; RC ops expect raw pointers.
            | _ when isHeapType param.Type ->
                let (size, timing1) =
                    timeWith swOpt addPayloadSize (fun () -> payloadSize param.Type ctxWithParams.TypeReg) timingAcc
                ((param.Id, size) :: acc, timing1)
            | _ -> (acc, timingAcc)
        ) ([], timing1)
    let paramIncs = List.rev paramIncsRev

    // Process function body with return analysis
    let (bodyResult, timing3) =
        timeWithTiming swOpt addInsertRC
            (fun t ->
                let (expr', varGen', types', typeCache', timing') =
                    insertRCWithAnalysis ctxWithParams bodyInfo varGen [] paramIncs swOpt t typesWithParams typeCache
                ((expr', varGen', types', typeCache'), timing'))
            timing2
    let (body', varGen', accTypes, typeCache') = bodyResult
    ({ func with Body = body' }, varGen', accTypes, typeCache', timing3)

/// Insert RC operations into a function
/// Returns (transformed function, varGen, accumulated TempTypes)
let insertRCInFunction (ctx: TypeContext) (func: Function) (varGen: VarGen) : Function * VarGen * Map<TempId, AST.Type> =
    let (func', varGen', types', _typeCache, _timing) =
        insertRCInFunctionInternal ctx func varGen None emptyRcTiming Map.empty emptyCExprTypeCache
    (func', varGen', types')

// ============================================================================
// TypeMap Completeness Verification
// ============================================================================

let private isTempMissing (typeMap: ANF.TypeMap) (tempId: TempId) : bool =
    not (Map.containsKey tempId typeMap)

let rec collectMissingTempIdsInExpr
    (typeMap: ANF.TypeMap)
    (expr: AExpr)
    (acc: TempId list)
    : TempId list =
    match expr with
    | Return _ -> acc
    | Let (tempId, _, body) ->
        let acc' = if isTempMissing typeMap tempId then tempId :: acc else acc
        collectMissingTempIdsInExpr typeMap body acc'
    | If (_, thenBranch, elseBranch) ->
        let acc' = collectMissingTempIdsInExpr typeMap thenBranch acc
        collectMissingTempIdsInExpr typeMap elseBranch acc'

let collectMissingTempIdsInFunction
    (typeMap: ANF.TypeMap)
    (func: Function)
    (acc: TempId list)
    : TempId list =
    let acc' =
        func.TypedParams
        |> List.fold (fun acc tp -> if isTempMissing typeMap tp.Id then tp.Id :: acc else acc) acc
    collectMissingTempIdsInExpr typeMap func.Body acc'

/// Verify that all defined TempIds have types in the TypeMap
/// Returns a list of TempIds that are missing from the TypeMap
let verifyTypeMapCompleteness (program: ANF.Program) (typeMap: ANF.TypeMap) : TempId list =
    let (ANF.Program (functions, mainExpr)) = program
    let missing =
        functions
        |> List.fold (fun acc func -> collectMissingTempIdsInFunction typeMap func acc) []
        |> collectMissingTempIdsInExpr typeMap mainExpr
    List.rev missing

/// Insert RC operations into a program with optional timing
/// Returns (ANF.Program, TypeMap, timing) where TypeMap contains all TempId -> Type mappings
let private insertRCInProgramInternal
    (result: ConversionResult)
    (swOpt: Stopwatch option)
    (timing: RcTiming)
    : Result<ANF.Program * ANF.TypeMap * RcTiming, string> =
    let ctx = createContext result
    let (ANF.Program (functions, mainExpr)) = result.Program
    let varGen = VarGen 1000  // Start high to avoid conflicts

    // Process all functions, accumulating types
    let rec processFuncs
        (funcs: Function list)
        (vg: VarGen)
        (accFuncs: Function list)
        (accTypes: Map<TempId, AST.Type>)
        (accTypeCache: CExprTypeCache)
        (timing: RcTiming)
        : Function list * VarGen * Map<TempId, AST.Type> * CExprTypeCache * RcTiming =
        match funcs with
        | [] -> (List.rev accFuncs, vg, accTypes, accTypeCache, timing)
        | f :: rest ->
            let (f', vg', types, typeCache, timing1) =
                insertRCInFunctionInternal ctx f vg swOpt timing accTypes accTypeCache
            processFuncs rest vg' (f' :: accFuncs) types typeCache timing1

    let (functions', varGen1, typesFromFuncs, typeCacheFromFuncs, timing1) =
        processFuncs functions varGen [] Map.empty emptyCExprTypeCache timing

    // Process main expression
    let (mainExpr', _, finalTypeMap, _typeCache, timing2) =
        insertRCInternal ctx mainExpr varGen1 swOpt timing1 typesFromFuncs typeCacheFromFuncs

    // Verify TypeMap completeness - all defined TempIds should have types
    let program' = ANF.Program (functions', mainExpr')
    let (missingTypes, timing3) =
        timeWith swOpt addVerifyTypeMap (fun () -> verifyTypeMapCompleteness program' finalTypeMap) timing2
    if not (List.isEmpty missingTypes) then
        let missingStr = missingTypes |> List.map (fun (TempId n) -> $"t{n}") |> String.concat ", "
        Crash.crash $"RefCountInsertion: TypeMap incomplete - missing types for: {missingStr}"

    Ok (program', finalTypeMap, timing3)

/// Insert RC operations into a program
/// Returns (ANF.Program, TypeMap) where TypeMap contains all TempId -> Type mappings
let insertRCInProgram (result: ConversionResult) : Result<ANF.Program * ANF.TypeMap, string> =
    insertRCInProgramInternal result None emptyRcTiming
    |> Result.map (fun (program, typeMap, _timing) -> (program, typeMap))

/// Insert RC operations into a program with fine-grained timing
/// Returns (ANF.Program, TypeMap, RcTiming)
let insertRCInProgramWithTiming (result: ConversionResult) : Result<ANF.Program * ANF.TypeMap * RcTiming, string> =
    let sw = Stopwatch.StartNew()
    insertRCInProgramInternal result (Some sw) emptyRcTiming
