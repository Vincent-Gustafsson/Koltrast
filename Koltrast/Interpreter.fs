module Koltrast.Interpreter

open Koltrast.Frontend.AST

let evaluateStatement (stmt: TypedStmt) env =
    match stmt with
    | AnnVarDecl(_, _, name, _, _, exprOption) ->
        
        
    | InferredVarDecl of 'a * Location * string * Mutability * Option<Type> * Expr<'a>
    | Block of 'a * Location * Stmt<'a> list
    | ExprStmt of 'a * Location * Expr<'a>

let evauluateCompUnit (compUnit: CompilationUnit<Type>) =
    evaluateStatement (compUnit |> snd) (Map.empty<striing>)
