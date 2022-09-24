namespace Atto

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis.CSharp.Syntax
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

    let generate (assemblyName : string) fnMap =
        let compilationUnit = SyntaxFactory.CompilationUnit()
        let namespaceNode =
            SyntaxFactory.NamespaceDeclaration(
                SyntaxFactory.IdentifierName(
                    assemblyName))
        let classNode = SyntaxFactory.ClassDeclaration($"{assemblyName}Type")
        let method =
            SyntaxFactory.MethodDeclaration(
                SyntaxFactory.PredefinedType(
                    SyntaxFactory.Token(
                        SyntaxKind.VoidKeyword)),
                "Main")
                .AddModifiers(SyntaxFactory.Token(SyntaxKind.StaticKeyword))
                .AddBodyStatements()
        let classNode' = classNode.AddMembers(method)
        let namespaceNode' = namespaceNode.AddMembers(classNode')
        let compilationUnit' = compilationUnit.AddMembers(namespaceNode')
        printfn "%A" <| compilationUnit'.NormalizeWhitespace()

        let compilation =
            CSharpCompilation
                .Create(assemblyName)
                .AddSyntaxTrees(compilationUnit'.SyntaxTree)
                .AddReferences(MetadataReference.CreateFromFile(typeof<obj>.Assembly.Location))
                .WithOptions(CSharpCompilationOptions(OutputKind.ConsoleApplication))
        let result = compilation.Emit($"{assemblyName}.exe")
        printfn "%A" result.Diagnostics
        assert(result.Success)

    [<EntryPoint>]
    let main args =
        parse input
            |> generate "fib"
        0
