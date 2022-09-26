namespace Atto.Parser

open Microsoft.VisualStudio.TestTools.UnitTesting

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

        match Program.parse input with
            | Choice1Of2 actual ->
                Assert.AreEqual<_>(expected, actual)
            | Choice2Of2 message -> Assert.Fail(message)
