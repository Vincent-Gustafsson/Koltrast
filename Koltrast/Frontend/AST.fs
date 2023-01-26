module Koltrast.Frontend.AST

type Location = {
    StreamName: string
    Start: {| Index: int; Line: int; Col: int |}
    End: {| Index: int; Line: int; Col: int  |}
}

type BinOpKind =
    | Add
    | Sub
    | Mul
    | Div

let binOpStr op =
    match op with
    | Add -> "+"
    | Sub -> "-"
    | Mul -> "*"
    | Div -> "/"

type Mutability =
    | Mutable
    | Immutable

type Type =
    | Int
    | Bool
    | UserDefined of string
    | Void

let strToType str =
    match str with
    | "int" -> Some Int
    | "void" -> Some Void
    | _ -> Some (UserDefined str)

(**
Or do this:
type VarType = 
    | Implicit
    | Explicit of string

type Value = 
    | IntValue of int
    | TypeConstructor of string * Value

type LetDeclr = LetDeclr of string * VarType * Value
**)

type Node<'a> =
    | NumericLiteral of 'a * Location * int
    | BoolLiteral of 'a * Location * bool
    | BinOp of 'a * Location * BinOpKind * Node<'a> * Node<'a>
    | VarDecl of 'a * Location * string * Mutability * Option<string> * Option<Node<'a>>
    | Block of 'a * Location * Node<'a> list

type UntypedNode = Node<unit>
type TypedNode = Node<Type>
