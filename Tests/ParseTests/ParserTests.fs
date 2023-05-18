module Tests.ParseTests.ParserTests

open Compiler.AST
open FParsec
open NUnit.Framework
open Compiler.Parse.Parser
open ParsedASTConstruction
open ParsedASTEquality

[<TestFixture>]
type ``Parsing Tests`` () =
    let runParserAndFailOnError (parser: Parser<'a,unit>) input =
        match runParserOnString parser () "testing" input with
        | Success(ast,_,_) -> ast
        | Failure(errStr,_,_) -> Assert.Fail($"Unexpected parser failure:\n {errStr}"); exit 1
    
    [<Test>]
    member this.``parses an empty program``() =
        let comparer = ProgramEqualityComparer()
        
        let input = ""
        let actual = runParserAndFailOnError pProgram input
        let expect = prog []
        Assert.That(actual, Is.EqualTo(expect).Using(comparer))

    [<Test>]
    member this.``parses a program with one function``() =
        let comparer = ProgramEqualityComparer()
        
        let input = "fn main() -> unit {}"
        let actual = runParserAndFailOnError pProgram input
        let expect = prog [(fn "main" [] Types.Unit (block []))]
        Assert.That(actual, Is.EqualTo(expect).Using(comparer))
