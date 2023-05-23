module Tests.ParseTests.ParserTests

open Compiler.AST
open Compiler.AST.ParsedAST
open FParsec
open NUnit.Framework
open Compiler.Parse.Parser
open ParsedASTConstruction
open ParsedASTEquality

[<TestFixture>]
type ``Parsing Tests`` () =
    let progComparer = ProgramEqualityComparer()
    let exprComparer = ExprEqualityComparer()
    
    let runParserAndFailOnError (parser: Parser<'a,unit>) input =
        match runParserOnString parser () "testing" input with
        | Success(ast,_,_) -> ast
        | Failure(errStr,_,_) -> Assert.Fail($"Unexpected parser failure:\n {errStr}"); exit 1
    
    [<Test>]
    member this.``parses an empty program``() =
        let input = ""
        let actual = runParserAndFailOnError pProgram input
        let expect = prog []
        Assert.That(actual, Is.EqualTo(expect).Using(progComparer))

    [<Test>]
    member this.``parses a program with one function``() =
        let input = "fn main() -> unit {}"
        let actual = runParserAndFailOnError pProgram input
        let expect = prog [(fn "main" [] Types.Unit (block []))]
        Assert.That(actual, Is.EqualTo(expect).Using(progComparer))
    
    [<Test>]
    member this.``parses an integer``() =
        let inputs = [ "1"; "10"; "-5"; "-69"; "0xCAFEBABE"; "0b1001"; "0o62" ]
        let expect = [ num 1; num 10; num -5; num -69; num 0xCAFEBABE; num 0b1001; num 0o62 ]
        
        (inputs, expect)
        ||> List.iter2 (fun input expect ->
            let actual = runParserAndFailOnError pInt input
            Assert.That(actual, Is.EqualTo(expect).Using(exprComparer)))

    [<Test>]
    member this.``parses a boolean``() =
        let inputs = [ "true"; "false" ]
        let expect = [ bool true; bool false ]
        
        (inputs, expect)
        ||> List.iter2 (fun input expect ->
            let actual = runParserAndFailOnError pBool input
            Assert.That(actual, Is.EqualTo(expect).Using(exprComparer)))

    [<Test>]
    member this.``parses a valid identifier``() =
        let inputs = [ "foo"; "_bar"; "Weird_mix_Case_tesT_" ]
        let expect = [ id "foo"; id "_bar"; id "Weird_mix_Case_tesT_" ]
        
        (inputs, expect)
        ||> List.iter2 (fun input expect ->
            let actual = runParserAndFailOnError pIdent input
            Assert.That(actual, Is.EqualTo(expect).Using(exprComparer)))

    [<Test>]
    member this.``parser fails if an identifier is a keyword``() =
        let inputs = [ "true"; "false"; "let"; "const"; "fn"; "while"; "if"; "then"; "else" ]
        let expectedErrors = List.map (fun kw -> $"'{kw}' is a reserved keyword") inputs
        
        (inputs, expectedErrors)
        ||> List.iter2 (fun input expect ->
            match runParserOnString pIdent () "testing" input with
            | Success _ -> Assert.Fail("parser unexpectedly succeeded")
            | Failure(_,err,_) ->
                let errMsg =
                    match err.Messages.Head with
                    | Message msg -> msg
                    | _ -> Assert.Fail("unexpected `ErrorMessageType`"); exit 1
                Assert.AreEqual($"'{input}' is a reserved keyword", errMsg))

    [<Test>]
    member this.``parses a let and const declaration without type annotation``() =
        let letDeclInput = "let foo = 42"
        let letDeclExpectation = let_ "foo" (num 42)
        
        let actual = runParserAndFailOnError pVar letDeclInput
        Assert.That(actual, Is.EqualTo(letDeclExpectation).Using(exprComparer))
        
        let constDeclInput = "const bar = baz"
        let constDeclExpectation = const_ "bar" (id "baz")
        
        let actual = runParserAndFailOnError pVar constDeclInput
        Assert.That(actual, Is.EqualTo(constDeclExpectation).Using(exprComparer))

    [<Test>]
    member this.``parses var. declarations with multiple different kinds of expressions``() =
        let inputs = [
            "let foo = 42"
            "let bar = true"
            "const baz = foobar"
            "const foobaz = if true then 42 else 0xBEEF"
            "let barbaz = { let barfoo = 1 barfoo + 1 }"
        ]
        let expect = [
            let_ "foo" (num 42)
            let_ "bar" (bool true)
            const_ "baz" (id "foobar")
            const_ "foobaz" (ifelse (bool true) (num 42) (num 0xBEEF))
            let_ "barbaz" (block [let_ "barfoo" (num 1); bin Add (id "barfoo") (num 1)])
        ]
        
        (inputs, expect)
        ||> List.iter2 (fun input expect ->
            let actual = runParserAndFailOnError pVar input
            Assert.That(actual, Is.EqualTo(expect).Using(exprComparer)))    
    