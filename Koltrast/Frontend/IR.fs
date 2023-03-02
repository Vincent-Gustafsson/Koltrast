module Koltrast.Frontend.IR

open Koltrast.Frontend.AST

type Symbol = string
type OperandKind =
    | IntConst of int64
    | BoolConst of bool
    | Symbol of Symbol

type Operand = { Value: OperandKind; Ty: Type }

type InstructionKind =
    | BinOp of {| Op: BinOpKind; Dst: Operand; Fst: Operand; Snd: Operand |}
    | Store of {| Dst: Operand; Fst: Operand |}
    | Return of Option<Operand>

type Instruction = {
    Value: InstructionKind
    Ty: Type
}

type Block = {
    Name: Symbol
    Instructions: Instruction list
}

type Function = {
    Name: Symbol
    Parameters: (Symbol * Type) list
    Blocks: Block list
}
