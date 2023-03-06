module Koltrast.Frontend.IRBuilder

open System.Collections.Generic
open Koltrast.Frontend.AST
open Koltrast.Frontend.IR




type Scope = {
    Symbols: Dictionary<string, Operand>
}

type IRBuilder() =
    let mutable EntrypointName: Option<string> = None
    let mutable sealedFunctions: Function list = []
    
    let blockStack: Stack<BasicBlock> = Stack()
    let scopeStack: Stack<Scope> = Stack()
    let fnStack: Stack<Function> = Stack()
    member this.currentBlock with get() = blockStack.Peek()
    member this.currentScope with get() = scopeStack.Peek()
    member this.currentFn with get() = fnStack.Peek()
    
    
    member this.GenTempVar(ty: Type): Operand =
        let tempVar = { _opnd=TempVar("%" + this.currentBlock.VarCount.ToString()); Ty=ty }
        this.currentBlock.VarCount <- this.currentBlock.VarCount + 1
        tempVar
    
    member this.AddVar(sourceName: string, ty: Type): Operand =
        let opnd = this.GenTempVar(ty)
        this.currentScope.Symbols.Add(sourceName, opnd)
        opnd
    
    member this.UpdateVar(sourceName: string, opnd: Operand): unit =
        this.currentScope.Symbols.Add(sourceName, opnd)

    member this.GetVar(sourceName: string): Operand =
        // I'm sorry for this code :(
        let rec lookup (scopes: Scope list) =
            match scopes with
            | [] -> failwith "couldn't find var, shouldn't happen"
            | hd :: tl ->
                let opnd = ref { _opnd=IntConst 1; Ty=I8 }
                if hd.Symbols.TryGetValue(sourceName, opnd) then
                    opnd.Value
                else
                    lookup tl

        lookup (scopeStack.ToArray() |> List.ofArray)

    member this.EnterScope(): unit =
        scopeStack.Push({ Symbols=Dictionary() })
        
    member this.LeaveScope(): unit =
        scopeStack.Pop() |> ignore
    
    member this.EnterBlock(): unit =
        blockStack.Push({
            Name=($"bb{blockStack.ToArray().Length}")
            Instructions=[]
            Predecessors=[]
            Successors=[]
            VarCount=0
        })
    
    member this.LeaveBlock(): unit =
        this.currentFn.Blocks <- this.currentFn.Blocks @ [blockStack.Pop()]
    
    member this.Emit(instr: Instruction): unit =
        this.currentBlock.Instructions <- this.currentBlock.Instructions @ [instr]

    member this.EnterFunction(fn: Function): unit =
        fnStack.Push(fn)
        this.EnterScope()
        
        fn.Parameters
        |> List.map this.AddVar
        |> ignore
        
        blockStack.Push({
            Name=($"entry")
            Instructions=[]
            Predecessors=[]
            Successors=[]
            VarCount=0
        })
    
    member this.LeaveFunction(): unit =
        this.LeaveBlock()
        this.LeaveScope()
        sealedFunctions <- sealedFunctions @ [fnStack.Pop()]

    member this.SetEntrypoint(name: string): unit =
        EntrypointName <- Some name
    
    member this.GetIR() = EntrypointName.Value, sealedFunctions