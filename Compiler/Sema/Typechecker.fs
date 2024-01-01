module Compiler.Sema.Typechecker

open System
open System.Collections.Generic
open Compiler.AST.ParsedAST
open Compiler.AST.TypedAST
open Compiler.AST.Types
open Compiler.Diagnostics
open Compiler.OptionBuilder
open Microsoft.VisualBasic.CompilerServices

type Mutability =
    | Mutable
    | Immutable

type Scope = { variables: Map<Expr, Type * Mutability>; functions: Map<Expr, FnType> }

type Env() =
    // Holds all the scopes in a stack. Each element in the stack is an instance
    // of a map that maps a variables name (more precisely an identifier expression, i.e. the variables name) to its
    // type and mutability.
    let scopes: Scope Stack = Stack()
    
    // Add a new variable to the current scope
    member this.addVar(ident, ty, mut): unit =
        scopes.Pop()
        |> fun scope ->
            let variables' = scope.variables |> Map.add ident (ty, mut)
            { scope with variables=variables' }
        |> scopes.Push

    // Tries to lookup a variable with its identifier and returns its properties encased in a Some option.
    // If the function fails to find the variable in the scope stack it returns a None option.
    member this.lookupVar(ident): (Type * Mutability) option =
        let rec lookup scopes =
            match scopes with
            | [] -> None
            | scope::scopes' ->
                match Map.tryFind ident scope with
                | Some res -> Some res
                | None -> lookup scopes'
        
        lookup (
            scopes.ToArray()
            |> List.ofArray
            |> List.map (fun scope ->
                scope.variables))
    
    member this.addFunction(ident, ty): unit =
        scopes.Pop()
        |> fun scope ->
            let functions' = scope.functions |> Map.add ident ty
            { scope with functions=functions' }
        |> scopes.Push
    
    member this.lookupFunction(ident): FnType option =
        let rec lookup scopes =
            match scopes with
            | [] -> None
            | scope::scopes' ->
                match Map.tryFind ident scope with
                | Some res -> Some res
                | None -> lookup scopes'
        
        lookup (
            scopes.ToArray()
            |> List.ofArray
            |> List.map (fun scope ->
                scope.functions))
    
    member this.enterScope(): unit = scopes.Push { variables=Map.empty; functions=Map.empty }
    member this.leaveScope(): unit = scopes.Pop() |> ignore

let isIntType ty = Seq.contains ty [| I8; I32 |]

let promoteNumberType ty1 ty2 =
    let getTypeWeight = function
        | I8 -> 8
        | I32 -> 32
        | _ -> failwith "uh oh"
    
    if (getTypeWeight ty1) > (getTypeWeight ty2)
    then ty1
    else ty2

let mkTExpr expr loc ty: TExpr = { _expr=expr; Loc=loc; Ty=ty }

let rec checkType (diagnostics: DiagnosticBag) (env: Env) (expr: Expr) (expectedTy: Type): TExpr option =
    let check = checkType diagnostics env
    let infer = inferType diagnostics env
    let reportTypeError msg hint = diagnostics.add { Message=msg; Hint=hint; Loc=expr.Loc; Kind=DiagnosticKind.Type; Level=DiagnosticLevel.Error }
    
    match expr._expr with
    | _ ->
        match infer expr with
        | Some tyExpr ->
            if tyExpr.Ty = expectedTy
            then Some tyExpr
            else reportTypeError $"type mismatch: expected {expectedTy}, got {tyExpr.Ty}" ""; None
        | None -> None

and inferType (diagnostics: DiagnosticBag) (env: Env) (expr: Expr): TExpr option =
    let check = checkType diagnostics env
    let infer = inferType diagnostics env
    let reportTypeError msg hint = diagnostics.add { Message=msg; Hint=hint; Loc=expr.Loc; Kind=DiagnosticKind.Type; Level=DiagnosticLevel.Error } // maybe return 'None'?
    let mkTExpr tyE ty = mkTExpr tyE expr.Loc ty
    
    match expr._expr with
    | ExprKind.Error _ -> failwith "unreachable"
    | ExprKind.Block exprs ->
        env.enterScope()
        
        let tBlockOpt =
            exprs
            |> List.map infer
            |> (fun result ->
                if List.contains None result then
                    None
                else
                    let tExprs = List.choose id result
                    match tExprs with
                    | [] -> Some (mkTExpr (Block []) Type.Unit)
                    | tExprs ->
                        let retTy = (List.last tExprs).Ty
                        Some (mkTExpr (Block tExprs) retTy))
        
        env.leaveScope()
        tBlockOpt
    
    | ExprKind.Ident name ->
        match env.lookupVar expr with
        | Some(ty, _) -> Some(mkTExpr (Ident name) ty)
        | None -> reportTypeError $"the variable '{name}' is not defined." ""; None

    | ExprKind.IntegerLiteral num ->
        let numLitWithType = mkTExpr (IntegerLiteral num)
        match num with
        | n when n >= -128 && n <= 127 -> Some(numLitWithType I8)
        // | n when n >= -32768 and n <= 32767 -> Ok(NumLitWithType I16)
        | n when (n >= -2147483648) && n <= 2147483647 -> Some(numLitWithType I32)
        // | n when n >= -9223372036854775808L && n <= 9223372036854775807L -> Some(numLitWithType I64)
        | _ -> reportTypeError "number is outside of the allowable range for i64" ""; None
        
    | ExprKind.BooleanLiteral b -> Some(mkTExpr (BooleanLiteral b) Bool)
    
    | ExprKind.BinOp bin -> opt {
        let! typedLeft = infer bin.Left
        let! typedRight = infer bin.Right
        
        let lType = typedLeft.Ty
        let rType = typedRight.Ty
        
        return! (
            match bin.Op with
            | Add | Sub | Mul | Div ->
                if isIntType lType && isIntType rType then
                    let resTy = promoteNumberType lType rType
                    Some(mkTExpr (BinOp({| Op=bin.Op; Left=typedLeft; Right=typedRight |})) resTy)
                else
                    reportTypeError
                        $"invalid operand types. they have type {lType} and {rType}, expected ({I8}, {I32})"
                        $"{lType} {binOpStr bin.Op} {rType}"
                    ; None)}
        
    | ExprKind.IfExpr ifExpr -> opt {
        let! tCondExpr = check ifExpr.Cond Bool
        let! tThenExpr = infer ifExpr.Then
        let! tElseExpr = check ifExpr.Else tThenExpr.Ty
        
        return mkTExpr (IfExpr({| Cond=tCondExpr; Then=tThenExpr; Else=tElseExpr |})) tThenExpr.Ty}
    
    | ExprKind.FuncAppl fnAppl ->
        match env.lookupFunction fnAppl.Name with
        | Some fnTy ->
            if fnAppl.Arguments.Length = fnTy.Parameters.Length then
                let tArgs =
                    (fnAppl.Arguments, fnTy.Parameters)
                    ||> List.map2 check
                    |> List.choose id
                
                if tArgs.Length = fnAppl.Arguments.Length
                then Some (mkTExpr (FuncAppl {| Arguments=tArgs; Name=(mkTExpr (Ident (fnAppl.Name.ToString())) (Fn fnTy) ) |}) fnTy.Return) 
                else None
            else
                reportTypeError $"expected {fnTy.Parameters.Length} arguments, got {fnAppl.Arguments.Length}." "" ; None
        | None -> reportTypeError $"the function '{fnAppl.Name}' is not defined." "" ; None
        
    | ExprKind.While whi -> opt {
        let! tCondExpr = check whi.Cond Bool
        let! tBodyExpr = infer whi.Body
        
        return mkTExpr (While({| Cond=tCondExpr; Body=tBodyExpr |})) Unit}
        
    | ExprKind.LetVar v | ExprKind.ConstVar v -> opt {
        let! tInitExpr =
            match expr.TypeAnnotation with
            | Some tyAnn -> check v.InitExpr tyAnn
            | None -> infer v.InitExpr

        match expr._expr with
        | ExprKind.LetVar _ -> env.addVar(v.Name, tInitExpr.Ty, Mutable)
        | ExprKind.ConstVar _ -> env.addVar(v.Name, tInitExpr.Ty, Mutable)
        |> ignore
        
        return mkTExpr (LetVar({| Name=v.Name; InitExpr=tInitExpr |})) Unit}
    
    | ExprKind.Assign ass ->
        match env.lookupVar(ass.Name) with
        | Some(ty, mutability) ->
            if mutability = Mutable then
                match check ass.AssExpr ty with
                | Some tAssExpr -> Some (mkTExpr (Assign({| Name=ass.Name; AssExpr=tAssExpr |})) Unit)
                | None -> None
            else
                reportTypeError "can't reassign to constant variable" "immutable"; None
        | None -> reportTypeError "undefined variable" "undefined"; None

let typecheckItem (diagnostics: DiagnosticBag) (env: Env) item =
    match item._item with
    | Function fn ->
        let fnTy = {|
            Parameters=(fn.Params |> List.map (fun p -> p.Ty))
            Return=fn.ReturnType
        |}
        
        env.addFunction(fn.Name, fnTy)
        env.enterScope()
        
        fn.Params
        |> List.iter (fun p -> env.addVar(p.Name, p.Ty, Mutable))
        
        match checkType diagnostics env fn.Body fn.ReturnType with
        | Some tExpr -> Some { _item=item._item; Ty=Type.Fn(fnTy); Loc=item.Loc }
        | None -> None

let typecheckCompUnit diagnostics compUnit =
    let env = Env()
    env.enterScope()
    
    let tItemsRes =
        compUnit.Items
        |> List.map (typecheckItem diagnostics env)
    
    env.leaveScope()
    
    // If diagnostics is empty then all the elements in tItemRes are of the type Some.
    if diagnostics.isEmpty() then
        let tItems = tItemsRes |> List.choose id
        Result.Ok { Items=tItems }
    else
        Result.Error diagnostics
