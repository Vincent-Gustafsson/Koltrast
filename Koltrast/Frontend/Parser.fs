module Koltrast.Frontend.Parser

open FParsec
open AST
open Koltrast.Location
open Koltrast.Frontend.AST
open Parser_utils



let ws = spaces

// Verifies that the given expr is an ident. Right now I just "panic" / fail, I'd like to maybe recover from this and
// perhaps add a diagnostic to some kind of UserState on the parser.
let ensureIdentExpr exprKind =
    match exprKind._expr with
    | Ident id -> id
    | _ -> failwith "expected identifier, what do?"

let ensureBlockExpr exprKind =
    match exprKind._expr with
    | Block block -> block
    | _ -> failwith "expected block, what do?"

type SymbolKind =
    | Dot
    | Comma
    | Colon
    | Semicolon
    | OpenCurly
    | CloseCurly
    | OpenParen
    | CloseParen
    | OpenBracket
    | CloseBracket
    | RArrow

// Parse a symbol and then strip trailing ws
let symbol sym =
    (match sym with
    | Dot -> skipString "."
    | Comma -> skipString ","
    | Colon -> skipString ":"
    | Semicolon -> skipString ";"
    | OpenCurly -> skipString "{"
    | CloseCurly -> skipString "}"
    | OpenParen -> skipString "("
    | CloseParen -> skipString ")"
    | OpenBracket -> skipString "["
    | CloseBracket -> skipString "]"
    | RArrow -> skipString "->"
    .>> ws)

type KeywordKind =
    | Let
    | Fn
    | Mut
    | While
    | Print
    | Entry

// Parse a symbol and then strip trailing ws
let keyword kw =
    (match kw with
    | Let -> skipString "let"
    | Fn -> skipString "fn"
    | Mut -> skipString "mut"
    | While -> skipString "while"
    | Print -> skipString "print"
    | Entry -> skipString "entry"
    .>> ws)

let betweenParens p = between (symbol OpenParen) (symbol CloseParen) p

let getLoc p = parse {
    do! ws
    let! startPos = getPosition
    let! pRes = p
    let! endPos = getPosition
    
    let loc = locFromFParsecPos startPos endPos
    return (loc, pRes)
}

let combineLocations loc1 loc2 = {
    StreamName = loc1.StreamName
    Start = {| Index=int loc1.Start.Index; Line=int loc1.Start.Line; Col=int loc1.Start.Col |}
    End = {| Index=int loc2.End.Index; Line=int loc2.End.Line; Col=int loc2.End.Col |}
}

let pMut = (stringReturn "mut" Mutable <|>% Immutable) .>> ws

let mkExpr loc exprKind: UntypedExpr = { _expr=exprKind; Loc=loc; Metadata=() }

// Operators
let opp = new OperatorPrecedenceParser<_,_,_>()

let addInfixOperator str prec assoc mapping =
    let op = InfixOperator(str, ws >>. getPosition, prec, assoc, (),
        (fun opPos l r -> mapping (combineLocations l.Loc r.Loc) l r))
    (opp.AddOperator(op))

let addTernaryOperator str1 str2 prec assoc mapping =
    let op = TernaryOperator(str1, ws >>. getPosition, str2, ws >>. getPosition, prec, assoc,
        (fun condExpr thenExpr elseExpr ->
            let loc = (combineLocations  condExpr.Loc elseExpr.Loc)
            mapping loc condExpr thenExpr elseExpr))
    (opp.AddOperator(op))

let mkBinOpExpr op loc l r = mkExpr loc (ExprKind.BinOp({|  Op=op; Left=l; Right=r  |}))

addInfixOperator "+" 7 Associativity.Left (mkBinOpExpr Add)
addInfixOperator "-" 7 Associativity.Left (mkBinOpExpr Sub)
addInfixOperator "*" 8 Associativity.Left (mkBinOpExpr Mul)
addInfixOperator "/" 8 Associativity.Left (mkBinOpExpr Div)
addInfixOperator "%" 8 Associativity.Left (mkBinOpExpr Mod)

addInfixOperator "==" 5 Associativity.Left (mkBinOpExpr Eq)
addInfixOperator ">=" 5 Associativity.Left (mkBinOpExpr GtEq)
addInfixOperator "<=" 5 Associativity.Left (mkBinOpExpr LtEq)
addInfixOperator ">" 5 Associativity.Left (mkBinOpExpr Gt)
addInfixOperator "<" 5 Associativity.Left (mkBinOpExpr Lt)

addInfixOperator "=" 3 Associativity.Right (fun loc id assExpr ->
    mkExpr loc (Assign({| Name=(ensureIdentExpr id); AssignExpr=assExpr |})))

addTernaryOperator "?" ":" 10 Associativity.Left (fun loc condExpr thenExpr elseExpr ->
    mkExpr loc (If({| Cond=condExpr; ThenExpr=thenExpr; ElseExpr=elseExpr |})))

let expr = opp.ExpressionParser

let intLiteral = getLoc pint64 .>> ws |>> (fun (loc, n) -> mkExpr loc (NumericLiteral(n)))
let boolLiteral = getLoc (stringReturn "true" true <|> stringReturn "false" false) .>> ws |>> (fun (loc, b) ->
    mkExpr loc (BoolLiteral(b)))

let ident =
    getLoc (many1Satisfy isValidIdentChar .>> ws) <?> "identifier"
    >>= (fun (loc, name) ->
        if isKeyword name then
            fail $"'{name}' is a reserved keyword"
        else
            preturn (mkExpr loc (Ident(name)))
    )

// parse an identifier or a function call.
// Probably not the best way of doing this.
// But it works!
let identWithOptArgs =
    getLoc (tuple2 ident (opt (betweenParens (sepBy (ws >>. expr) (ws >>. skipChar ',')))))
    |>> (fun (loc, (id, optArgs)) ->
            match optArgs with
            | Some args -> mkExpr loc (FuncAppl({| Name=(ensureIdentExpr id); Arguments=args |}))
            | None -> mkExpr loc (Ident(ensureIdentExpr id)))

// Type annotation parsing
let tyAnnot, tyAnnotRef = createParserForwardedToRef<(Location * Type), _>()

let primitiveType = (choice [
    (stringReturn "i8" (Type.I8))
    (stringReturn "i64" (Type.I64))
    (stringReturn "bool" (Type.Bool))
    (stringReturn "unit" (Type.Unit))
] .>> ws)

let arrayType =
    symbol OpenBracket
    >>. tyAnnot
    .>> symbol Semicolon
    .>>. (pint64 .>> ws |>> int)
    .>> symbol CloseBracket
    |>> (fun ((_, ty), len) -> Type.Array(ty, len))

let optFuncType = opt (many1 (symbol RArrow >>. tyAnnot |>> snd))

// This looks really fucking weird, but hey, it works! :)
tyAnnotRef.Value <- (getLoc (choice [
    primitiveType
    arrayType
    betweenParens tyAnnot |>> snd
] .>>. optFuncType |>> (function
    | ty, None -> ty
    | ty1, Some tyRest ->
        let types = [ty1] @ tyRest
        Fun(types[0..(types.Length-2)], List.last types))
))

(**
tyAnnRef.Value <- (getLoc (choice [
    (START .>>. END |>> (function
    | ty, Some ty2 ->
        let types = [ty] @ ty2
        let parameterTypes = types[0..(types.Length-2)]
        let retTy = List.last types
        Fun(parameterTypes, retTy)
    | ty, None -> ty))
]))
**)

let pVar = parse {
    let! startPos = getPosition
    do! (keyword Let) <?> "'let' keyword in variable declaration."
    
    let! mut = attempt pMut <?> "incomplete variable declaration."
    
    let! id = ident
    let name = ensureIdentExpr id
    
    let! tyAnnOpt = opt (symbol Colon >>. tyAnnot) <?> "incomplete variable declaration. Expected a type annotation."

    let! initExprOpt =
        match mut with
        | Mutable -> opt (skipChar '=' >>. ws >>. (expr <?> "incomplete variable declaration. Expected an expression."))
        | Immutable -> (skipChar '=' >>. ws >>. expr) <?> "incomplete variable declaration. An immutable variable needs an initializer." |>> Some
    
    let! endPos = getPosition
    
    let loc = locFromFParsecPos startPos endPos
    
    do!
        if tyAnnOpt.IsNone && initExprOpt.IsNone then
            fail "incomplete variable declaration. A variable needs either a type annotation, or an initializer."
        else
            preturn ()

    return
        (match (tyAnnOpt, initExprOpt) with
        | (Some (annLoc, ty), Some initExpr) ->
            let varExpr = mkExpr loc (Var({| Name=(ensureIdentExpr id); Mut=mut; InitExprOpt=(Some initExpr); TyAnnot=Some ty |}))
            varExpr
        | (Some (annLoc, ty), None) ->
            let varExpr = mkExpr loc (Var({| Name=(ensureIdentExpr id); Mut=mut; InitExprOpt=None; TyAnnot=Some ty |}))
            varExpr
        | (None, Some initExpr) ->
            mkExpr loc (Var({| Name=(ensureIdentExpr id); Mut=mut; InitExprOpt=(Some initExpr); TyAnnot=None |}))
        | (None, None) -> failwith "covered by the fail parser above.")
}

let block =
    getLoc(
        between
            (symbol OpenCurly)
            (symbol CloseCurly)
            (many expr)
    ) <?> "block"
    |>> (fun (loc, exprs) -> mkExpr loc (Block(exprs)))

let func = parse {
    let! funcStartPos = getPosition
    
    do! keyword Fn

    let! isEntry = (opt (keyword Entry)) |>> Option.isSome
    
    let! nameOpt = (opt ident) >>= (function
        | Some name -> preturn (Some name)
        | None ->
            if isEntry
                then fail "only named functions can be the entrypoint" >>. preturn None
                else preturn None
    )
    
    let! parameters = betweenParens (sepBy ((ident |>> ensureIdentExpr) .>>. (symbol Colon >>. tyAnnot |>> snd)) (symbol Comma))
    
    do! symbol RArrow
    
    let! retType = tyAnnot |>> snd
    
    let! body = block

    let! funcEndPos = getPosition
    let funcLoc = locFromFParsecPos funcStartPos funcEndPos
    
    return (
        let paramNames = parameters |> List.unzip |> fst
        let paramTypes = parameters |> List.unzip |> snd
        let funTyAnnot = Type.Fun(paramTypes, retType)
        match nameOpt with
        | Some name ->
            let fnExpr = mkExpr funcLoc (Func({| Name=(ensureIdentExpr name); Parameters=paramNames; Body=body; TyAnnot=funTyAnnot |}))
            if isEntry
                then mkExpr funcLoc (Entrypoint(fnExpr))
                else fnExpr
        | None ->  mkExpr funcLoc (AnonFunc({| Parameters=paramNames; Body=body; TyAnnot=funTyAnnot |}))
    )
}

let pwhile =
    getLoc (
        keyword While
        >>. expr <?> "while condition"
        .>>. expr  <?> "while block"
    ) <?> "while loop"
    |>> (fun (loc, (cond, block)) -> mkExpr loc (ExprKind.While({| Cond=cond; Body=block |})))

let pprint = getLoc (keyword Print >>. expr) |>> (fun (loc, e) -> mkExpr loc (ExprKind.Print(e)))

opp.TermParser <- choice [
    block
    pwhile
    func
    pVar
    pprint // for prototyping "parsePrint"
    
    intLiteral
    boolLiteral
    identWithOptArgs
    (betweenParens expr)
]

let program = ws >>. many (func <|> pVar) .>> eof

let parseFile path =
    match runParserOnFile program () path System.Text.Encoding.Default with
    | Success(ast, _, _) -> Result.Ok ast
    | Failure(err, _, _) -> Result.Error err
