module Koltrast.Frontend.Parser

open FParsec
open AST
open Parser_utils

let ws = spaces

let getLoc p = parse {
    do! ws
    let! startPos = getPosition
    let! pRes = p
    let! endPos = getPosition
    
    let loc = locFromFParsecPos startPos endPos
    return (loc, pRes)
}

let thingy loc1 loc2 ={
    StreamName = loc1.StreamName
    Start = {| Index=int loc1.Start.Index; Line=int loc1.Start.Line; Col=int loc1.Start.Col |}
    End = {| Index=int loc2.End.Index; Line=int loc2.End.Line; Col=int loc2.End.Col |}
}

// Keywords
let pmut = stringReturn "mut" Mutable <|>% Immutable
let plet = skipString "let"


// Expressions
let opp = new OperatorPrecedenceParser<_,_,_>()

let addInfixOperator str prec assoc mapping =
    let op = InfixOperator(str, ws >>. getPosition, prec, assoc, (), (fun opPos l r -> mapping (thingy (getExprPos l) (getExprPos r)) l r))
    (opp.AddOperator(op))


addInfixOperator "+" 7 Associativity.Left (fun loc l r -> UntypedNode.BinOp((), loc, Add, l, r))
addInfixOperator "-" 7 Associativity.Left (fun loc l r -> UntypedNode.BinOp((), loc, Sub, l, r))
addInfixOperator "*" 8 Associativity.Left (fun loc l r -> UntypedNode.BinOp((), loc, Mul, l, r))
addInfixOperator "/" 8 Associativity.Left (fun loc l r -> UntypedNode.BinOp((), loc, Div, l, r))

let expr = opp.ExpressionParser

let betweenParens p =
    between (skipChar '(') (skipChar ')') p

let numericLiteral = getLoc pint64 .>> ws |>> (fun (loc, n) -> UntypedNode.NumericLiteral ((), loc, int n))
let boolLiteral = getLoc (stringReturn "true" true <|> stringReturn "false" false) .>> ws |>> (fun (loc, b) -> UntypedNode.BoolLiteral ((), loc, b))

// TODO: Check for keywords later on
let ident =
    many1Satisfy isValidIdentChar
    >>= (fun str -> if isKeyword str then fail $"'{str}' is a reserved keyword" else preturn str)

let literal =
    choice [
        numericLiteral
        boolLiteral
    ]

opp.TermParser <- choice [
    betweenParens expr
    literal
]



// Declarations

let varDecl = parse{
    do! ws
    let! startPos = getPosition
    do! plet <?> "'let' keyword in variable declaration."
    
    do! ws
    let! mutStatus = attempt pmut <?> "incomplete variable declaration."
    
    do! ws
    let! name = ident

    do! ws
    let! tyAnnotation = opt (attempt (skipChar ':') >>. ws  >>. (ident <?> "incomplete variable declaration. Expected a type annotation."))

    do! ws
    let! expr = match mutStatus with
                | Mutable -> opt (skipChar '=' >>. (expr <?> "incomplete variable declaration. Expected an expression."))
                | Immutable -> (skipChar '=' >>. expr) <?> "incomplete variable declaration. An immutable variable needs an initializer. ignore this part ->" |>> Some
    let! endPos = getPosition
    
    let loc = locFromFParsecPos startPos endPos
    
    return UntypedNode.VarDecl((), loc, name, mutStatus, tyAnnotation, expr)
}

let line = varDecl .>> skipChar ';'
// Block
let block = getLoc (skipChar '{' >>. many line .>> skipChar '}') |>> (fun (loc, nodes) -> Block((), loc, nodes))


let program = block .>> eof

let parseFile path =
    match runParserOnFile program () path System.Text.Encoding.Default with
    | Success(ast, _, _) -> Result.Ok ast
    | Failure(err, _, _) -> Result.Error err