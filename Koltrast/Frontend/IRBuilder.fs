module Koltrast.Frontend.IRBuilder

open System.Collections.Generic
open Koltrast.Frontend.AST
open Koltrast.Frontend.IR




type Scope = {
    mutable Symbols: Map<string, int * Type>
}

type IRBuilder() =
    let mutable EntrypointName: Option<string> = None
    let mutable sealedFunctions: Function list = []
    
    let blockStack: Stack<BasicBlock> = Stack()
    let scopeStack: Stack<Scope> = Stack()
    let fnStack: Stack<Function> = Stack()
    let mutable varCountStack: Stack<int> =
        let s = Stack()
        s.Push(0)
        s

    member this.currentBlock with get() = blockStack.Peek()
    member this.currentScope with get() = scopeStack.Peek()
    member this.currentFn with get() = fnStack.Peek()
    
    
    member this.GenTempVar(ty: Type): Operand =
        let varCount = varCountStack.Pop()
        let tempVar = { _opnd=TempVar("%" + varCount.ToString()); Ty=ty }
        varCountStack.Push(varCount + 1)
        tempVar
    
    member this.AddVar(sourceName: string, ty: Type): Operand =
        let opnd = { _opnd=TempVar($"{sourceName}_{0}"); Ty=ty }
        this.currentScope.Symbols <- this.currentScope.Symbols.Add(sourceName, (0, ty))
        opnd
    
    member this.UpdateVar(sourceName: string): Operand =
        let ver, ty = Map.find sourceName this.currentScope.Symbols
        this.currentScope.Symbols <- this.currentScope.Symbols.Add(sourceName, (ver+1, ty))
        { _opnd=TempVar($"{sourceName}{ver+1}"); Ty=ty }

    member this.GetVar(sourceName: string): Operand =
        // I'm sorry for this code :(
        let rec lookup (scopes: Scope list) =
            match scopes with
            | [] -> failwith $"couldn't find var {sourceName}, shouldn't happen"
            | hd :: tl ->
                match Map.tryFind sourceName hd.Symbols with
                | Some(ver, ty) -> { _opnd=TempVar($"{sourceName}{ver}"); Ty=ty }
                | None -> lookup tl

        lookup (scopeStack.ToArray() |> List.ofArray)

    member this.EnterScope(): unit =
        scopeStack.Push({ Symbols=Map.empty })
        
    member this.LeaveScope(): unit =
        scopeStack.Pop() |> ignore
    
    member this.EnterBlock(): unit =
        varCountStack.Push(0)
        blockStack.Push({
            Name=($"bb{blockStack.ToArray().Length}")
            Instructions=[]
            Predecessors=[]
            Successors=[]
        })
    
    member this.LeaveBlock(): unit =
        varCountStack.Pop()
        this.currentFn.Blocks <- this.currentFn.Blocks @ [blockStack.Pop()]
    
    member this.Emit(instr: Instruction): unit =
        this.currentBlock.Instructions <- this.currentBlock.Instructions @ [instr]

    member this.EnterFunction(fn: Function): unit =
        fnStack.Push(fn)
        this.EnterScope()
        
        varCountStack.Push(0)
        
        fn.Parameters
        |> List.map this.AddVar
        |> ignore
        
        blockStack.Push({
            Name=($"entry")
            Instructions=[]
            Predecessors=[]
            Successors=[]
        })
    
    member this.LeaveFunction(): unit =
        this.LeaveBlock()
        this.LeaveScope()
        sealedFunctions <- sealedFunctions @ [fnStack.Pop()]

    member this.SetEntrypoint(name: string): unit =
        EntrypointName <- Some name
    
    member this.GetIR() = EntrypointName.Value, sealedFunctions