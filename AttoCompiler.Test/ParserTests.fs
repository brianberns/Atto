namespace Atto.Parser

open Microsoft.VisualStudio.TestTools.UnitTesting
open FParsec

[<TestClass>]
type ParserTests() =

    [<TestMethod>]
    member _.Fibonacci() =

        let input =
            """
            fn fib n is
                if = n 0
                    1
                    * n fib - n 1
            """

        let expected =
            let fib = Identifier "fib"
            let n = Identifier "n"
            [{
                Name = fib
                Args = [ n ]
                Expr =
                    If (
                        Equal (
                            Name n,
                            Literal (Num 0.0)),
                        Literal (Num 1.0),
                        Operation (
                            Multiplication,
                            Name n,
                            Call (
                                fib,
                                [|
                                    Operation (
                                        Subtraction,
                                        Name n,
                                        Literal (Num 1.0))
                                |])))
            }]

        match runParserOnString Program.parse Map.empty "" input with
            | Success (result, _, _) ->
                Assert.AreEqual<_>(expected, result)
            | Failure (message, _, _) -> Assert.Fail(message)
