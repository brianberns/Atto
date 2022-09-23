namespace Atto.Parser

open FParsec

type Operator =
    | Addition
    | Subtraction
    | Multiplication

module Operator =

    let parse<'s> : Parser<_, 's> =
        choice [
            skipChar '+' >>% Addition
            skipChar '-' >>% Subtraction
            skipChar '*' >>% Multiplication
        ]
