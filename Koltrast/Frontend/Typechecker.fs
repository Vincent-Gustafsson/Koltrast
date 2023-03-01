module Koltrast.Frontend.Typechecker

open System.Collections.Generic
open Koltrast.Diagnostics
open Koltrast.OptionBuilder
open Koltrast.Frontend.AST

type Env() =
    let variables: Map<string, (Type * Mutability)> Stack = Stack()
    member this.addVar name value =
        variables.Pop()
        |> Map.add name value
        |> variables.Push
    member this.lookupVar name =
        let rec inner scopes =
            match scopes with
            | [] -> None
            | scope::scopes' ->
                match Map.tryFind name scope with
                | Some res -> Some res
                | None -> inner scopes'
        inner (variables.ToArray() |> List.ofArray)
    member this.enterScope() = variables.Push Map.empty
    member this.leaveScope() = variables.Pop() |> ignore

let isIntType ty = Seq.contains ty [| I8; I64 |] 

let getTypeWeight = function
    | I8 -> 8
    | I64 -> 64
    | _ -> failwith "uh oh"

let computeType ty1 ty2 =
    if (getTypeWeight ty1) > (getTypeWeight ty2)
    then ty1
    else ty2

let mkTypedExpr expr loc ty = { _expr=expr; Loc=loc; Metadata=ty }

let rec checkType (diagnostics: DiagnosticBag) (env: Env) (expr: UntypedExpr) (expectedTy: Type): TypedExpr option =
    let check = checkType diagnostics env
    let infer = inferType diagnostics env
    let reportTypeError msg hint = diagnostics.add { Message=msg; Hint=hint; Loc=expr.Loc; Kind=DiagnosticKind.Type; Level=DiagnosticLevel.Error }
    
    match expr._expr with
    | _ ->
        match infer expr with
        | Some tyExpr ->
            if tyExpr.Metadata = expectedTy
            then Some tyExpr
            else reportTypeError $"type mismatch: expected {typeToStr expectedTy}, got {typeToStr tyExpr.Metadata}" ""; None
        | None -> None

and inferType (diagnostics: DiagnosticBag) (env: Env) (expr: UntypedExpr): TypedExpr option =
    let check = checkType diagnostics env
    let infer = inferType diagnostics env
    let reportTypeError msg hint = diagnostics.add { Message=msg; Hint=hint; Loc=expr.Loc; Kind=DiagnosticKind.Type; Level=DiagnosticLevel.Error } // maybe return 'None'?

    match expr._expr with
    | NumericLiteral num ->
        let numLitWithType = mkTypedExpr (NumericLiteral num) expr.Loc
        match num with
        | n when n >= -128 && n <= 127 -> Some(numLitWithType I8)
        // | n when n >= -32768 and n <= 32767 -> Ok(NumLitWithType I16)
        // | n when n >= -2147483648 and n <= 2147483647 -> Ok(NumLitWithType I32)
        | n when n >= -9223372036854775808L && n <= 9223372036854775807L -> Some(numLitWithType I64)
        | _ -> reportTypeError "number is outside of the allowable range for i64" ""; None
    | BoolLiteral b -> Some(mkTypedExpr (BoolLiteral b) expr.Loc Bool)
    | Ident name ->
        match env.lookupVar name with
        | Some(ty, _) -> Some(mkTypedExpr (Ident name) expr.Loc ty)
        | None -> reportTypeError $"'{name}' is not defined." ""; None
    | Var v ->
        match v.TyAnnot, v.InitExprOpt with
        | Some typeAnnotation, Some initExpr ->
            match check initExpr typeAnnotation with
            | Some typedInitExpr ->
                env.addVar v.Name (typeAnnotation, v.Mut)
                Some(mkTypedExpr (Var {| Name=v.Name; Mut=v.Mut; InitExprOpt=Some typedInitExpr; TyAnnot=v.TyAnnot |}) expr.Loc Unit)
            | None -> None
        | Some typeAnnotation, None ->
            env.addVar v.Name (typeAnnotation, v.Mut)
            Some(mkTypedExpr (Var({| Name=v.Name; Mut=v.Mut; InitExprOpt=None; TyAnnot=v.TyAnnot |})) expr.Loc Unit)
        | None, Some initExpr ->
            match infer initExpr with
            | Some typedInitExpr ->
                env.addVar v.Name (typedInitExpr.Metadata, v.Mut)
                Some(mkTypedExpr (Var {| Name=v.Name; Mut=v.Mut; InitExprOpt=Some typedInitExpr; TyAnnot=None |}) expr.Loc Unit)
            | None -> None
        | _ -> failwith "covered by the parser (Var)"
    | Assign ass ->
        match env.lookupVar ass.Name with
        | Some (ty, mut) ->
            match mut with
            | Mutable ->
                match check ass.AssignExpr ty with
                | Some tyAssExpr -> Some (mkTypedExpr (Assign {| Name=ass.Name; AssignExpr=tyAssExpr |}) expr.Loc ty)
                | None -> None
            | Immutable ->
                reportTypeError $"'trying to assign to immutable variable '{ass.Name}'" "can't assign to an immutable variable"; None
        | None ->
            reportTypeError $"'{ass.Name}' is not defined." ""; None
    | BinOp bin ->
        opt {
            let! typedLeft = infer bin.Left
            let! typedRight = infer bin.Right
            
            let lType = typedLeft.Metadata
            let rType = typedRight.Metadata
            
            return! (
                match bin.Op with
                | Add | Sub | Mul | Div | Mod ->
                    if isIntType lType && isIntType rType then
                        let resTy = computeType lType rType
                        Some(mkTypedExpr (BinOp({| Op=bin.Op; Left=typedLeft; Right=typedRight |})) expr.Loc resTy)
                    else
                        reportTypeError
                            $"invalid operand types. they have type {typeToStr lType} and {typeToStr rType}, expected ({typeToStr I8}, {typeToStr I64})"
                            $"{typeToStr lType} {binOpStr bin.Op} {typeToStr rType}"
                        ; None
                
                | GtEq | LtEq | Gt | Lt ->
                    if isIntType lType && isIntType rType then
                        Some(mkTypedExpr (BinOp({| Op=bin.Op; Left=typedLeft; Right=typedRight |})) expr.Loc Bool)
                    else
                        reportTypeError
                            $"invalid operand types. they have type {typeToStr lType} and {typeToStr rType}, expected ({typeToStr I8}, {typeToStr I64})"
                            $"{typeToStr lType} {binOpStr bin.Op} {typeToStr rType}"
                        ; None
                | Eq ->
                    match lType with
                    | Bool ->
                        if lType = rType then
                            Some(mkTypedExpr (BinOp({| Op=bin.Op; Left=typedLeft; Right=typedRight |})) expr.Loc Bool)
                        else
                            reportTypeError
                                $"type mismatch. they have type {typeToStr lType} and {typeToStr rType}"
                                $"{typeToStr lType} {binOpStr bin.Op} {typeToStr rType}"
                            ; None
                    | lType when isIntType lType && isIntType rType ->
                        Some(mkTypedExpr (BinOp({| Op=bin.Op; Left=typedLeft; Right=typedRight |})) expr.Loc Bool)
                    | _ ->
                        reportTypeError
                            $"invalid operand types. they have type {typeToStr lType} and {typeToStr rType}, expected ({typeToStr I8}, {typeToStr I64}, {typeToStr Bool})"
                            $"{typeToStr lType} {binOpStr bin.Op} {typeToStr rType}"
                        ; None
            )
        }
    | If ifExpr ->
        opt {
            let! tCondExpr = check ifExpr.Cond Bool
            let! tThenExpr = infer ifExpr.ThenExpr
            let! tElseExpr = check ifExpr.ElseExpr tThenExpr.Metadata
            
            return mkTypedExpr (If({| Cond=tCondExpr; ThenExpr=tThenExpr; ElseExpr=tElseExpr |})) expr.Loc tThenExpr.Metadata
        }
    | Print pExpr ->
        match infer pExpr with
        | Some tExpr -> Some(mkTypedExpr (Print(tExpr)) expr.Loc Unit)
        | None -> None
    | Block block ->
        env.enterScope()
        
        let tBlockOpt =
            block
            |> List.map infer
            |> (fun result ->
                if List.contains None result then
                    None
                else
                    let tExprs = List.choose id result
                    // I should probably change the data structure of blocks (https://stackoverflow.com/a/1175111)
                    match tExprs with
                    | [] -> Some(Block [], Type.Unit)
                    | tExprs ->
                        let retTy = (List.last tExprs).Metadata
                        Some(Block tExprs,retTy)
            )
        
        env.leaveScope()
        
        match tBlockOpt with
        | Some (tBlock, retTy) -> Some(mkTypedExpr tBlock expr.Loc retTy)
        | None -> None
    | Func fn ->
        env.addVar fn.Name (fn.TyAnnot, Immutable)
        env.enterScope()
        
        let paramTypes, retTy =
            match fn.TyAnnot with
            | Fun(parameters, retTy) -> parameters, retTy
        
        (fn.Parameters, paramTypes)
        ||> List.zip
        |> List.iter (fun (name, ty) -> env.addVar name (ty, Immutable))
        
        let tBlockOpt = infer fn.Body
        
        env.leaveScope()
        
        match tBlockOpt with
        | Some tBlock ->
            if tBlock.Metadata = retTy then
                Some(mkTypedExpr (Func {| Name=fn.Name; Parameters=fn.Parameters; Body=tBlock; TyAnnot=fn.TyAnnot |}) expr.Loc Unit)
            else
                // get location of retExpr later on!
                diagnostics.add {
                    Message=($"unexpected return type, expected '{typeToStr retTy}', got '{typeToStr tBlock.Metadata}'")
                    Hint=""
                    Loc=tBlock.Loc
                    Kind=DiagnosticKind.Type
                    Level=DiagnosticLevel.Error
                }
                None
                (**
                reportTypeError
                    $"unexpected return type, expected '{typeToStr retTy}', got '{typeToStr tBlock.Metadata}'"
                    ""; None **)
        | None -> None
    | FuncAppl appl ->
        match env.lookupVar appl.Name with
        | Some (Fun (paramTypes, retTy), _) ->
            if appl.Arguments.Length = paramTypes.Length then
                (appl.Arguments, paramTypes)
                ||> List.map2 check
                |> (fun results ->
                    if List.contains None results then None
                    else Some(mkTypedExpr (FuncAppl {| Name=appl.Name; Arguments=(List.choose id results) |}) expr.Loc retTy)
                )
            else
                reportTypeError
                    $"number of arguments does not match the number of parameters (expected {paramTypes.Length}, got {appl.Arguments.Length})."
                    (if appl.Arguments.Length > paramTypes.Length then "too many arguments" else "too few arguments"); None
        | Some (ty, _) ->
            reportTypeError
                $"'{appl.Name}' is not a function, it's a '{typeToStr ty}'"
                ""; None
        | None ->
            reportTypeError
                $"'{appl.Name}' is not defined"
                ""; None
    | Entrypoint fnExpr ->
        match infer fnExpr with
        | Some typedFnExpr ->
            Some(mkTypedExpr (Entrypoint typedFnExpr) expr.Loc typedFnExpr.Metadata)
        | None -> None
    | AnonFunc anFn -> 
        env.enterScope()
        
        let paramTypes, retTy =
            match anFn.TyAnnot with
            | Fun(parameters, retTy) -> parameters, retTy
        
        (anFn.Parameters, paramTypes)
        ||> List.zip
        |> List.iter (fun (name, ty) -> env.addVar name (ty, Immutable))
        
        let tBlockOpt = infer anFn.Body
        
        env.leaveScope()
        
        match tBlockOpt with
        | Some tBlock ->
            if tBlock.Metadata = retTy then
                Some(mkTypedExpr (AnonFunc {| Parameters=anFn.Parameters; Body=tBlock; TyAnnot=anFn.TyAnnot |}) expr.Loc anFn.TyAnnot)
            else
                // get location of retExpr later on!
                diagnostics.add {
                    Message=($"unexpected return type, expected '{typeToStr retTy}', got '{typeToStr tBlock.Metadata}'")
                    Hint=""
                    Loc=tBlock.Loc
                    Kind=DiagnosticKind.Type
                    Level=DiagnosticLevel.Error
                }
                None
                (**
                reportTypeError
                    $"unexpected return type, expected '{typeToStr retTy}', got '{typeToStr tBlock.Metadata}'"
                    ""; None **)
        | None -> None
        
        
        
    | _ -> failwith $"missing infer case for {expr._expr}"

let typeCheck diagnostics exprs: Result<TypedExpr list,DiagnosticBag> =
    let env = Env()
    env.enterScope()
    
    exprs
    |> List.map (fun e -> inferType diagnostics env e)
    |> (fun result ->
        if List.exists Option.isNone result then
            Result.Error diagnostics
        else
            result
            |> List.map (function | Some expr -> expr)
            |> Ok)