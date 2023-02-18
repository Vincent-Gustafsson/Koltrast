module Koltrast.Interpreter

open Koltrast.Frontend.AST
open Koltrast.Frontend.Parser_utils

type Value =
    | VInt of int
    | VBool of bool
    | VUnit of unit
    | VFunc of string list * TypedExpr

type Env = {
    vars: Map<string, Value>
    funcs: Map<string, (string list * TypedExpr)>
}

module Env =
    let empty = { vars=Map.empty; funcs=Map.empty }
    let extendWith (env1: Env) (env2: Env) =
        let vars' =
            (env1.vars, env2.vars)
            ||> (fun a b -> Map.toList a, Map.toList b)
            ||> (@)
            |> Map.ofList
        
        let funcs' =
            (env1.funcs, env2.funcs)
            ||> (fun a b -> Map.toList a, Map.toList b)
            ||> (@)
            |> Map.ofList
            
        { vars=vars'; funcs=funcs' }
    let addVar env key value = { env with vars=(Map.add key value env.vars) }
    let lookupVar env key = Map.tryFind key env.vars
    let addFunc env key value = { env with funcs=(Map.add key value env.funcs) }
    let lookupFunc env key = Map.tryFind key env.funcs

let evaluateProgram (compUnit: CompilationUnit<Type>) =
    let rec evaluate (env: Env) expr =
        match expr with
        | NumericLiteral(_, _, n) -> env, VInt n
        | BoolLiteral(_, _, b) -> env, VBool b
        | Ident(_, _, name) ->
            match Env.lookupVar env name, Env.lookupFunc env name with
            | Some varVal, None -> env, varVal
            | None, Some (a, b) -> env, (VFunc(a, b))
            | _ -> failwith "What the fuck???"
        | BinOp(_, _, op, l, r) ->
            let (env', lVal) = evaluate env l
            let (env'', rVal) = evaluate env' r
            
            let vRes =
                match lVal, rVal with
                | VInt n1, VInt n2 ->
                    match op with
                    | Add -> VInt(n1 + n2)
                    | Sub -> VInt(n1 - n2)
                    | Mul -> VInt(n1 * n2)
                    | Div -> VInt(n1 / n2)
                    | Mod -> VInt(n1 % n2)
                    | Eq -> VBool(n1 = n2)
                    | GtEq -> VBool(n1 >= n2)
                    | LtEq -> VBool(n1 <= n2)
                    | Gt -> VBool(n1 > n2)
                    | Lt -> VBool(n1 < n2)
                | VBool b1, VBool b2 ->
                    match op with
                    | Eq -> VBool(b1 = b2)
            
            env'', vRes
        | Assign(_, _, ident, assExpr) ->
            let name = getNameFromIdent ident
            let env', assVal = evaluate env assExpr
            Env.addVar env' name assVal, assVal 
        | AnnVarDecl(ty, _, name, _, _, exprOption) ->
            match exprOption with
            | Some initExpr ->
                let env', initVal = evaluate env initExpr
                Env.addVar env' name initVal, VUnit()
            | None ->
                match ty with
                | I64 -> Env.addVar env name (VInt(0)), VUnit()
                | Bool -> Env.addVar env name  (VBool(false)), VUnit()
        | InferredVarDecl(_, _, name, _, _, initExpr) ->
            let env', initVal = evaluate env initExpr
            Env.addVar env' name initVal, VUnit()
        | If(_, _, condExpr, thenExpr, elseExpr) ->
            let env', condVal = evaluate env condExpr
            match condVal with
            | VBool condBool ->
                if condBool = true then
                    evaluate env' thenExpr
                else
                    evaluate env' elseExpr
        | Block(_, _, exprs) ->
            ((env, []), exprs)
            ||> List.fold (fun (env, vals) expr ->
                let env', exprVal = evaluate env expr
                env', vals @ [exprVal])
            |> (fun (env', vals) ->
                match vals with
                | [] -> env', VUnit()
                | vals -> env', List.last vals)
        | Func(_, _, name, params, _, block) ->
            let env' = Env.addFunc env name (params |> List.unzip |> fst, block)
            let env'' =
                Env.addVar env' 
            
            env'', VFunc((params |> List.unzip |> fst), block)
        
        | Call(_, _, name, args) ->
            let params, block =
                match Env.lookupFunc env name, Env.lookupVar env name with
                | Some res, None -> res
                | None, Some res ->
                    match res with
                    | VFunc(params, body) -> (params, body)

            let (env', argValues) =
                ((env, []), args)
                ||> List.fold (fun (env, values) expr ->
                    let env', value = evaluate env expr
                    env', values @ [value])
            
            let paramVars =
                (params, argValues)
                ||> List.zip
                |> Map.ofList
            
            let env'' =
                (params, argValues)
                ||> List.zip
                |> List.fold (fun env (parameter, argument) ->
                    match argument with
                    | VFunc (funValParams, body) -> Env.addFunc env parameter (funValParams, body)
                    | _ -> Env.addVar env parameter argument
                    ) env'
            
            let _, callResultValue = evaluate env'' block
            
            env, callResultValue
        | While(_, _, cond, block) ->
            let rec inner env =
                match evaluate env cond |> snd with
                | VBool b ->
                    if b then
                        let env', _ = evaluate env block
                        inner env'
                    else
                        ()
            inner env
            env, VUnit()
        | Print(_, _, expr) ->
            let env', value = evaluate env expr
            match value with
            | VInt n -> printfn $"{n}"
            | VBool b -> printfn $"{b}"
            env', value
            
            
    match compUnit with
    | CompilationUnit expr -> evaluate Env.empty expr
        