module Koltrast.Frontend.IR

open Koltrast.Frontend.AST

type OperandKind =
    | IntConst of int64
    | BoolConst of bool
    | Symbol of string
    | TempVar of string
    override x.ToString() =
        match x with
        | IntConst num -> num.ToString()
        | BoolConst b -> b.ToString()
        | Symbol name -> name
        | TempVar name -> name

type Operand =
    { _opnd: OperandKind; Ty: Type }
    override this.ToString() = this._opnd.ToString()

type InstructionKind =
    | BinInstr of {| Op: BinOpKind; Dst: Operand; Fst: Operand; Snd: Operand |}
    | Store of {| Dst: Operand; Src: Operand |}
    | Load of {| Dst: Operand; Src: Operand |}
    | Return of Option<Operand>
    Alloc <-------------
    override x.ToString() =
        match x with
        | BinInstr bin -> $"{bin.Dst} = {bin.Fst} {binOpStr bin.Op} {bin.Dst}"
        | Store s -> $"{s.Dst} = {s.Src}"
        | Load s -> $"{s.Dst} = {s.Src}"
        | Return retValOpt ->
            match retValOpt with
            | Some retVal -> $"ret {retVal}"
            | None -> "ret"

[<StructuredFormatDisplay("{_instr}")>]
type Instruction = {
    _instr: InstructionKind
    Ty: Type
}

// Maybe call it basic block instead?
type Block = {
    Name: string
    Instructions: Instruction list
}

type Parameter = (string * Type)

type Function = {
    Name: string
    ReturnType: Type
    Parameters: Parameter list
    mutable Blocks: Block list
}
