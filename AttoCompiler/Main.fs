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

    [<EntryPoint>]
    let main args =
        match Parser.Program.parse input with
            | Choice1Of2 fns ->
                Generator.Program.generate "fib" fns
            | Choice2Of2 message ->
                printfn "%s" message
        0
