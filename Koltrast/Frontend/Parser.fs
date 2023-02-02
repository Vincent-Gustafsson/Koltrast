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

let thingy loc1 loc2 = {
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
    let op = InfixOperator(str, ws >>. getPosition, prec, assoc, (), (fun opPos l r -> mapping (thingy (getExprLoc l) (getExprLoc r)) l r))
    (opp.AddOperator(op))


addInfixOperator "+" 7 Associativity.Left (fun loc l r -> UntypedExpr.BinOp((), loc, Add, l, r))
addInfixOperator "-" 7 Associativity.Left (fun loc l r -> UntypedExpr.BinOp((), loc, Sub, l, r))
addInfixOperator "*" 8 Associativity.Left (fun loc l r -> UntypedExpr.BinOp((), loc, Mul, l, r))
addInfixOperator "/" 8 Associativity.Left (fun loc l r -> UntypedExpr.BinOp((), loc, Div, l, r))

addInfixOperator "=" 3 Associativity.Right (fun loc l r -> UntypedExpr.Assign((), loc, l, r))

let expr = opp.ExpressionParser

let betweenParens p =
    between (skipChar '(') (skipChar ')') p

let numericLiteral = getLoc pint64 .>> ws |>> (fun (loc, n) -> UntypedExpr.NumericLiteral ((), loc, int n))
let boolLiteral = getLoc (stringReturn "true" true <|> stringReturn "false" false) .>> ws |>> (fun (loc, b) -> UntypedExpr.BoolLiteral ((), loc, b))

// TODO: Check for keywords later on
let ident =
    getLoc (many1Satisfy isValidIdentChar .>> ws)
    >>= (fun (loc, name) ->
        if isKeyword name then
            fail $"'{name}' is a reserved keyword"
        else
            preturn (UntypedExpr.Ident((), loc, name))
    )

let literal =
    choice [
        numericLiteral
        boolLiteral
    ]

opp.TermParser <- choice [
    betweenParens expr
    literal
    ident
]



// Declarations

let typeAnnotation = choice [
    (stringReturn "int" (Type.Int))
    (stringReturn "bool" (Type.Bool))
]

let varDecl = parse{
    do! ws
    let! startPos = getPosition
    do! plet <?> "'let' keyword in variable declaration."
    
    do! ws
    let! mut = attempt pmut <?> "incomplete variable declaration."
    
    do! ws
    let! parsedIdent =  ident
    let name = getNameFromIdent parsedIdent
    
    do! ws
    let! tyAnnOpt = opt (attempt (skipChar ':') >>. ws  >>. (typeAnnotation <?> "incomplete variable declaration. Expected a type annotation."))

    do! ws
    let! initExprOpt = match mut with
                | Mutable -> opt (skipChar '=' >>. ws >>. (expr <?> "incomplete variable declaration. Expected an expression."))
                | Immutable -> (skipChar '=' >>. ws >>. expr) <?> "incomplete variable declaration. An immutable variable needs an initializer." |>> Some
    let! endPos = getPosition
    
    let loc = locFromFParsecPos startPos endPos
    
    do!
        if tyAnnOpt.IsNone && initExprOpt.IsNone then
            fail "incomplete variable declaration. A variable needs either a type annotation, or an initializer."
        else
            preturn ()
            
    let stmt =
        match (tyAnnOpt, initExprOpt) with
        | (Some ty, Some initExpr) -> UntypedStmt.AnnVarDecl((), loc, name, mut, ty, Some initExpr)
        | (Some ty, None) -> UntypedStmt.AnnVarDecl((), loc, name, mut, ty, None)
        | (None, Some initExpr) -> UntypedStmt.InferredVarDecl((), loc, name, mut, None, initExpr)
        | (None, None) -> failwith "covered by the fail parser above."

    return stmt    
}

let exprStatement = getLoc expr |>> (fun (loc, expression) -> UntypedStmt.ExprStmt((), loc, expression))

let statement =
    choice [varDecl; exprStatement]
    .>> skipChar ';' .>> ws

let block =
    getLoc(
        between
            (skipChar '{' .>> ws)
            (skipChar '}' .>> ws)
            (many statement)
    )
    |>> (fun (loc, stmts) -> UntypedStmt.Block((), loc, stmts))

let program = block .>> eof |>> (fun stmt -> CompilationUnit((), stmt))

let parseFile path =
    match runParserOnFile program () path System.Text.Encoding.Default with
    | Success(ast, _, _) -> Result.Ok ast
    | Failure(err, _, _) -> Result.Error err