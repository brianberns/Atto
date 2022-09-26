namespace Atto

open FParsec

module Main =

    let input =
        """
        fn fib n is
            if = n 0
                1
                * n fib - n 1

        fn main is
            print fib 10
        """

    let parse input =
        match runParserOnString Parser.Program.parse Map.empty "" input with
            | Success (fnMap, _, _) ->
                fnMap
            | Failure (message, _, _) ->
                failwith message

    [<EntryPoint>]
    let main args =
        parse input
            |> Generator.Program.generate "fib"
        0
