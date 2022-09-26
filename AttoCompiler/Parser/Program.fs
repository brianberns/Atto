namespace Atto.Parser

open FParsec

module Program =

    let private parseProgram =
        spaces
            >>. many Function.parse
            .>> eof

    let private runParser parser input =
        match runParserOnString parser Map.empty "" input with
            | Success (fnMap, _, _) ->
                Choice1Of2 fnMap
            | Failure (message, _, _) ->
                Choice2Of2 message

    let parse input =
        runParser parseProgram input
