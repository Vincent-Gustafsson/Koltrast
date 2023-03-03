module Koltrast.Frontend.IRBuilder

open System.Collections.Generic
open Koltrast.Frontend.AST
open Koltrast.Frontend.IR

type Scope = {
    Parent: Scope option
    Symbols: Dictionary<string, Operand>
    mutable TempVarCount: int
}

type IRBuilder() =
    let mutable allFunctions: Function list = []
    let mutable entrypointName: Option<string> = None
    let mutable currentFn : Function option = None
    let mutable fnStack : Function list = []
    let mutable scope : Scope = { Parent = None; Symbols = Dictionary(); TempVarCount=0 }
    
    member this.SetEntrypoint(name: string): unit = entrypointName <- Some name
    
    member this.GenTempVar(): OperandKind =
        let tempVar = TempVar ("%" + scope.TempVarCount.ToString())
        scope.TempVarCount <- scope.TempVarCount + 1
        tempVar
    
    member this.addVar (name: string) (opnd: Operand): unit =
        scope.Symbols[name] <- opnd
    
    member this.LookupVar (name: string): Operand =
        let rec lookup (scope: Scope option) (name: string) =
            match scope with
            | None -> None
            | Some(s) ->
                if s.Symbols.ContainsKey(name) then
                    Some(s.Symbols.[name])
                else
                    lookup s.Parent name
        
        match lookup (Some(scope)) name with
        | None -> failwith $"undeclared variable {name}"
        | Some opnd -> opnd
    
    member this.BeginFunction (name: string) (returnType: Type) (parameters: Parameter list): unit =
        let newFn = { Name=name; ReturnType=returnType; Parameters=parameters; Blocks=[] }
        currentFn <- Some newFn
        fnStack <- currentFn.Value :: fnStack
        let newBlock = { Name="entry"; Instructions=[] }
        newFn.Blocks <- newBlock :: newFn.Blocks
        scope <- { Parent = Some(scope); Symbols = Dictionary(); TempVarCount=0 }
        List.iter (fun (paramName, paramTy) -> this.addVar paramName { _opnd=Symbol paramName; Ty=paramTy } ) parameters

    member this.EndFunction(): unit =
        allFunctions <- allFunctions @ [currentFn.Value]
        // yeah... I'm sorry for this lol.
        match fnStack with
        | _ :: fn :: tl ->
            currentFn <- Some fn
            fnStack <- tl
        | fn :: tl ->
            currentFn <- Some fn
            fnStack <- tl
        | _ -> ()
            
        scope <- scope.Parent.Value

    member this.BeginBlock (name: string): unit =
        match currentFn with
        | None -> failwith "No current function defined"
        | Some fn ->
            let newBlock = { Name = name; Instructions = [] }
            fn.Blocks <- newBlock :: fn.Blocks
            scope <- { Parent = Some(scope); Symbols = Dictionary(); TempVarCount=0 }

    member this.EndBlock(): unit =
        match currentFn with
        | None -> failwith "No current function defined"
        | Some fn ->
            match fn.Blocks with
            | [] -> failwith "No blocks in current function"
            | hd :: tl -> fn.Blocks <- tl @ [hd]
            scope <- scope.Parent.Value

    member this.Emit(instr: Instruction) =
        match currentFn with
        | None -> failwith "No current function defined"
        | Some fn ->
            match fn.Blocks with
            | [] -> failwith "No blocks in current function"
            | hd :: tl ->
                let newhd = { hd with Instructions = hd.Instructions @ [instr] }
                fn.Blocks <- newhd :: tl

    member this.GetIR() = entrypointName.Value, allFunctions
