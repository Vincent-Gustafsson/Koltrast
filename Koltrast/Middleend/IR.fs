module Koltrast.Middleend.IR

open System.Collections.Generic
open Koltrast.Frontend.AST



type Instruction =
    | BinOp of {| Op: BinOpKind; Fst: Operand; Snd: Operand; Dst: Operand |}
    | Set of {| Value: Operand; Dst: Operand |}
    | Load of {| Symbol: Operand; Dst: Operand; |}
    | If of {| Cond: Operand; Label: Operand; |}
    | Goto of Operand
    | Call of Operand
    | Return of Option<Operand>
    | Push of Operand
    | Pop of Operand
    override x.ToString() =
        match x with
        | BinOp bin -> $"{bin.Dst} = {bin.Fst} {binOpStr bin.Op} {bin.Snd}"
        | Set s -> $"{s.Dst} = {s.Value}"
        | Load l -> $"{l.Dst} = {l.Symbol}"
        | If iff -> $"if {iff.Cond} then {iff.Label}"
        | Goto s -> $"{s}"
        | Call label -> $"call {label}"
        | Return opOpt ->
            match opOpt with
            | Some op -> $"ret {op}"
            | None -> $"ret"
        | Push op -> $"push {op}"
        | Pop op -> $"pop {op}"

and Operand =
    | ConstI8 of int64
    | ConstI64 of int64
    | ConstBool of bool
    | TempVar of string
    | Symbol of string
    | Label of string
    override x.ToString() =
        match x with
        | ConstI8 num -> $"{num}"
        | ConstI64 num -> $"{num}"
        | ConstBool b -> $"{b}"
        | TempVar name -> name
        | Symbol name -> name
        | Label name ->  name

type IRBuilder() =
    let mutable symbolTable: (string list) list = [[]]
    let mutable currentScope = -1
    let mutable labels: Map<string, Instruction list> = Map.empty |> Map.add "entry" []
    let mutable currentLabel: string = "entry"
    let mutable labelStack: Stack<string> = Stack()
    member this.emit (instr: Instruction) =
        labels <- Map.add currentLabel (labels[currentLabel] @ [instr] ) labels

    member this.genTempVar() = Operand.TempVar("%" + $"{labels[currentLabel].Length + 1}")
    
    member this.enterScope() =
        currentScope <- currentScope + 1
        if (currentScope+1) > symbolTable.Length then
            symbolTable <- symbolTable @ [[]]
    member this.exitScope() = currentScope <- currentScope - 1
    
    member this.enterLabel name =
        labelStack.Push(currentLabel)
        if Option.isSome (Map.tryFind name labels)
        then ()
        else labels <- Map.add name [] labels
        currentLabel <- name
    member this.exitLabel() = currentLabel <- labelStack.Pop()
    
    member this.addVar string =
        let scope = symbolTable[currentScope]
        let scope' = scope @ [string]
        symbolTable <- (List.mapi (fun i scope -> if currentScope = i then scope' else scope) symbolTable)
    
    member this.lookupVar name =
        let rec inner scopes =
            match scopes with
            | [] -> false
            | scope::scopes' ->
                match List.contains name scope with
                | true -> true
                | false -> inner scopes'
        inner (symbolTable)
    
    member this.getIR() = labels

let generateIR expr =
    let builder = new IRBuilder()
    
    let expectOperand (operandOption): Operand =
        match operandOption with
        | Some op -> op
        | None -> failwith "???"
    
    let rec gen expr: Option<Operand> =
        match expr._expr with
        | ExprKind.NumericLiteral num -> Some (ConstI64 num)
        | ExprKind.BoolLiteral b -> Some (ConstBool b)
        | ExprKind.BinOp bin ->
            let left = expectOperand (gen bin.Left)
            let right = expectOperand (gen bin.Right)
            let resultSym = builder.genTempVar()
            builder.emit (Instruction.BinOp {| Op=bin.Op; Fst=left; Snd=right; Dst=resultSym |})
            Some resultSym
        | Block exprs ->
            builder.enterScope()
            
            let sym = 
                match exprs with
                | [] -> None
                | [expr] -> gen expr
                | allExprs ->
                    allExprs
                    |> List.map gen
                    |> List.last
                    
            builder.exitScope()
            sym
        | Var v ->
            builder.addVar v.Name
            match v.InitExprOpt with
            | Some initExpr ->
                let initExprSym = expectOperand (gen initExpr)
                builder.emit (Set({| Dst=Symbol v.Name; Value=initExprSym |}))
            | None -> ()
            None
        | Ident id ->
            if builder.lookupVar id then
                let temp = builder.genTempVar()
                builder.emit (Load {| Dst=temp; Symbol=(Symbol id) |})
                Some temp
            else
                failwith $"umm, no clue what to do here. This shouldn't happen. (couldn't find ident '{id}')"
        | Assign ass ->
            if builder.lookupVar ass.Name then
                let assExprSym = expectOperand (gen ass.AssignExpr) 
                builder.emit (Set({| Dst=Symbol ass.Name; Value=assExprSym |}))
                Some assExprSym
            else
                failwith "umm, no clue what to do here. This shouldn't happen."
        | Func fn ->
            builder.addVar fn.Name
            builder.enterLabel fn.Name

            builder.enterScope()
            
            List.iter builder.addVar fn.Parameters
            
            // Pop the parameters off the stack in reverse order (since it's pushed in order)
            fn.Parameters
            |> List.rev
            |> List.iter (fun param ->
                if builder.lookupVar param
                then builder.emit (Pop (Symbol param))
                else failwith "umm"
            )
            
            let sym = gen fn.Body
            
            builder.emit (Return sym)
            builder.exitScope()
            builder.exitLabel()
            None
        | FuncAppl appl ->
            if builder.lookupVar appl.Name then
                appl.Arguments
                |> List.map gen
                |> List.iter (fun argOpt ->
                    match argOpt with
                    | Some arg -> builder.emit (Push arg)
                    | None -> ()
                )
                builder.emit (Call (Label appl.Name))
                None
            else
                failwith "not again :("
        
        | _ -> failwith $"incomplete match cases for the IR ({expr._expr})"
            
                
    gen expr |> ignore
    builder.getIR()
