namespace Atto

open System
open System.Reflection
open System.Reflection.Emit
open Lokad.ILPack.Metadata

open FParsec
open Atto.Parser

module Main =

    let input =
        """
        fn fib n is
            if = n 0
                1
                * n fib - n 1

        fn main is
            fib 10
        """

    let parse input =
        match runParserOnString Program.parse Map.empty "" input with
            | Success (fnMap, _, _) ->
                fnMap
            | Failure (message, _, _) ->
                failwith message

    let generate assemblyName fnMap =
        (*
        let domain = AppDomain.CurrentDomain
        let name = AssemblyName(assemblyName)
        let builder =
            AssemblyBuilder.DefineDynamicAssembly(
                name, AssemblyBuilderAccess.Run)
        *)
        printfn "%A "fnMap

    [<EntryPoint>]
    let main args =
        parse input
            |> generate "fib"
        0
