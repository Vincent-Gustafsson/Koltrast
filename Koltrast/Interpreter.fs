module Koltrast.Interpreter

open Koltrast.Frontend.AST


type Value =
    | VInt of int
    | VBool of bool
    | VUnit of unit

type InterpEnv = Map<string, Value>

let rec evaluateExpr env expr =
    match expr with
    | NumericLiteral(_, _, n) -> env, VInt n
    | BoolLiteral(_, _, b) -> env, VBool b
    | BinOp(_, _, op, l, r) ->
        let env', lVal = evaluateExpr env l
        let env'', rVal = evaluateExpr env' r
        match op, lVal, rVal with
        | (Add, VInt n1, VInt n2) -> env, VInt(n1 + n2)
        | (Sub, VInt n1, VInt n2) -> env, VInt(n1 - n2)
        | (Mul, VInt n1, VInt n2) -> env, VInt(n1 * n2)
        | (Div, VInt n1, VInt n2) -> env, VInt(n1 / n2)
    | Ident(_, _, name) ->
        env, Map.find name env
    | Assign(_, _, id, assExpr) ->
        match id with
        | Ident(_,_,name) ->
            let env', assVal = evaluateExpr env assExpr
            let env'' = Map.add name assVal env'
            env'', assVal

let rec evaluateStatement (stmt: TypedStmt) (env: Map<string, Value>): (InterpEnv * Value) =
    match stmt with
    | AnnVarDecl(_, _, name, _, _, exprOption) ->
        match exprOption with
        | Some initExpr ->
            let env', initExprVal = evaluateExpr env initExpr
            let env'' = Map.add name initExprVal env'
            env'', VUnit()
        | None -> env, VUnit()
        
    | InferredVarDecl(_, _, name, _, _, initExpr) ->
        let (env', initExprVal) = evaluateExpr env initExpr
        let env'' = Map.add name initExprVal env'
        env'', VUnit()
    | Block(_, _, stmts) ->
        let env' =
            (env, stmts)
            ||> List.fold (fun en s ->
                let (env', _) = evaluateStatement s en
                env')
        env', VUnit()

    | ExprStmt(_, _, expr) -> (evaluateExpr env expr)

let evaluateCompUnit (compUnit: CompilationUnit<Type>) =
    evaluateStatement (compUnit |> snd) (Map.empty<string, Value>)
