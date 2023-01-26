module Koltrast.Frontend.Environment

open System.Collections.Generic
open System.Collections.ObjectModel
open Koltrast.Frontend.AST

type VarSymbol = | VarSymbol of option<Type> * Mutability

type Scope = {
    VarDefinitions: Map<string, VarSymbol>
}

module Scope =
    let empty = { VarDefinitions=Map.empty }
    
    let lookupVar scope name =
        match Map.tryFind name scope.VarDefinitions with
        | Some varSymbol -> Ok varSymbol
        | None -> Error "undeclared variable"

    let addVar (scope: Scope) name varSym =
        if Map.containsKey name scope.VarDefinitions then
            Error "variable already declared"
        else
            let scope' = { scope with VarDefinitions=(Map.add name varSym scope.VarDefinitions) }
            Ok scope'

type Env = Scope Stack

let createEnv (ast: UntypedNode) =
    let initialEnv: Env = Stack()
    initialEnv.Push({VarDefinitions=Map.empty})
    
    let rec inner (env: Env) (ast: UntypedNode): Result<Env, string list> =
        let scope = env.Peek()
        match ast with
        | VarDecl(_,loc, name, mutability, tyStrOpt,_) ->
            let ty = match tyStrOpt with
                        | Some tyStr -> strToType tyStr
                        | None -> None
            
            let sym = VarSymbol(ty, mutability)
            match Scope.addVar scope name sym with
            | Ok scope' ->
                env.Pop() |> ignore
                env.Push(scope')
                Ok env
            | Error errStr -> Error [$"{name}:{loc}" + errStr]
        | Block(_,_, nodes) ->
            env.Push(scope)
            let res =
                nodes
                |>  List.fold (fun (änv, errs) n ->
                        match inner änv n with
                        | Ok änv' -> (änv', errs)
                        | Error e -> (änv, errs @ e)
                ) (env, [])
            
            match res with
            | (env', []) -> Ok env'
            | (env', errs) -> Error errs
            
        | _ -> Ok env
    inner initialEnv ast
    
    
(**

module Koltrast.Frontend.Environment

type private Scope(parent : Scope option ) =
  let mutable list = List.empty<VariableDeclaration>

  let identifierFromDeclaration (node: Node) =
    
    | ScalarVariableDeclaration(_, i)
    | ArrayVariableDeclaration(_, i) -> i

  let declaresIdentifier (identifierRef : IdentifierRef) declaration =
    (identifierFromDeclaration declaration) = identifierRef.Identifier

  member x.AddDeclaration declaration =
    let ifd = identifierFromDeclaration
    if List.exists (fun x -> ifd x = ifd declaration) list then
      raise (variableAlreadyDefined (identifierFromDeclaration declaration))
    list <- declaration :: list

  member x.FindDeclaration identifierRef =
    let found = List.tryFind (fun x -> declaresIdentifier identifierRef x) list
    match found with
    | Some(d) -> d
    | None ->
      match parent with
      | Some(ss) -> ss.FindDeclaration identifierRef
      | None -> raise (nameDoesNotExist (identifierRef.Identifier))

type private SymbolScopeStack() =
  let stack = new Stack<SymbolScope>()
  do stack.Push(new SymbolScope(None))

  member x.CurrentScope = stack.Peek()

  member x.Push() = stack.Push(new SymbolScope(Some(stack.Peek())))
  member x.Pop() = stack.Pop() |> ignore
  member x.AddDeclaration declaration = stack.Peek().AddDeclaration declaration



type SymbolTable(program) as self =
  …
  let whileStatementStack = Stack<WhileStatement>()
  let symbolScopeStack = new SymbolScopeStack() 



**)