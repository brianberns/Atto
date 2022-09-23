﻿namespace Atto.Parser

open FParsec

module Program =

    let parse =
        spaces
            >>. many Function.parse
            .>> eof
            |>> Seq.map (fun fn ->
                fn.Name, fn)
            |>> Map
