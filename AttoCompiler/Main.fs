namespace Atto

open System
open System.Reflection
open System.Reflection.Emit
open Lokad.ILPack

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

        let asmName = AssemblyName(assemblyName)
        let asmBldr =
            AssemblyBuilder.DefineDynamicAssembly(
                asmName, AssemblyBuilderAccess.Run)
        let modBldr =
            asmBldr.DefineDynamicModule($"{assemblyName}Module")
        let typeBldr =
            modBldr.DefineType(
                $"{assemblyName}Type",
                TypeAttributes.Public)
        let methodBldr =
            typeBldr.DefineMethod(
                $"{assemblyName}Method",
                MethodAttributes.Public ||| MethodAttributes.Static)

        let ilGen = methodBldr.GetILGenerator()
        ilGen.Emit(OpCodes.Ldc_I4, 1)

        let _typ = typeBldr.CreateType()

        let asmGen = AssemblyGenerator()
        asmGen.GenerateAssembly(asmBldr, $"{assemblyName}.exe")

    [<EntryPoint>]
    let main args =
        parse input
            |> generate "fib"
        0
