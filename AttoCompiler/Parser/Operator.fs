namespace Atto.Parser

open FParsec

type Operator =
    | Addition
    | Subtraction
    | Multiplication

module Operator =

    let parse<'s> : Parser<_, 's> =
        seq {
            '+', Addition
            '-', Subtraction
            '*', Multiplication
        }
            |> Seq.map (fun (c, op) ->
                skipChar c >>% op)
            |> Seq.toList
            |> choice
