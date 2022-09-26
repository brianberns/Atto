namespace Atto

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open type SyntaxFactory

open Basic.Reference.Assemblies

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

    let generateFunction (Identifier fnName) fnMap =
        MethodDeclaration(
            returnType = PredefinedType(
                Token(
                    SyntaxKind.ObjectKeyword)),
            identifier = fnName)
            .AddModifiers(
                Token(SyntaxKind.StaticKeyword))
            .AddBodyStatements()
            :> Syntax.MemberDeclarationSyntax

    let generateFunctions fnMap =
        [|
            for fnName in Map.keys fnMap do
                generateFunction fnName fnMap
        |]

    let generate (assemblyName : string) fnMap =

        let mainMethod =

            let stmt =
                Block(
                    ExpressionStatement(
                        InvocationExpression(
                            IdentifierName("main"))))

            MethodDeclaration(
                returnType = PredefinedType(
                    Token(
                        SyntaxKind.VoidKeyword)),
                identifier = "Main")
                .AddModifiers(
                    Token(SyntaxKind.StaticKeyword))
                .AddBodyStatements(stmt)

        let classNode =
            let members = generateFunctions fnMap
            ClassDeclaration($"{assemblyName}Type")
                .AddMembers(items = members)
                .AddMembers(mainMethod)

        let namespaceNode =
            NamespaceDeclaration(
                IdentifierName(assemblyName))
                .AddMembers(classNode)

        let compilationUnit =
            CompilationUnit().AddMembers(namespaceNode)
        printfn "%A" <| compilationUnit.NormalizeWhitespace()

        let compilation =
            let references : MetadataReference[] =
                [|
                    Net60.mscorlib
                    Net60.SystemRuntime
                |]
            let options =
                CSharpCompilationOptions(OutputKind.ConsoleApplication)
                    .WithMainTypeName($"{namespaceNode.Name}.{classNode.Identifier}")
            CSharpCompilation
                .Create(assemblyName)
                .AddSyntaxTrees(compilationUnit.SyntaxTree)
                .AddReferences(references)
                .WithOptions(options)
        let result = compilation.Emit($"{assemblyName}.dll")
        for diagnostic in result.Diagnostics do
            printfn "%A" diagnostic
        if result.Success then printfn "Success"

        System.IO.File.Copy(
            "App.runtimeconfig.json",
            $"{assemblyName}.runtimeconfig.json",
            overwrite = true)

    [<EntryPoint>]
    let main args =
        parse input
            |> generate "fib"
        0
