namespace Atto.Parser

open Microsoft.VisualStudio.TestTools.UnitTesting
open FParsec

[<TestClass>]
type ParserTests() =

    [<TestMethod>]
    member _.Fibonacci() =

        let input =
            """
    fn f n is
        if = n 0
            1
            * n f - n 1
            """.Trim()

        match runParserOnString Program.parse Map.empty "" input with
            | Success (result, _, _) -> printfn "%A" result
            | Failure (message, _, _) -> printfn "%s" message
