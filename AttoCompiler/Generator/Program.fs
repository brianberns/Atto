namespace Atto.Generator

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open type SyntaxFactory

open Basic.Reference.Assemblies

module Program =

    let private mainMethod =

        let stmt =
            ExpressionStatement(
                InvocationExpression(
                    IdentifierName("main")))

        MethodDeclaration(
            returnType = PredefinedType(
                Token(
                    SyntaxKind.VoidKeyword)),
            identifier = "Main")
            .AddModifiers(
                Token(SyntaxKind.StaticKeyword))
            .AddBodyStatements(stmt)

    let private printMethod =
        MethodDeclaration(
            PredefinedType(
                Token(SyntaxKind.ObjectKeyword)),
            SyntaxFactory.Identifier("Print"))
            .WithModifiers(
                TokenList(
                    Token(SyntaxKind.StaticKeyword)))
            .WithParameterList(
                ParameterList(
                    SingletonSeparatedList(
                        Parameter(
                            SyntaxFactory.Identifier("value"))
                            .WithType(
                                PredefinedType(
                                    Token(SyntaxKind.ObjectKeyword))))))
            .WithBody(
                Block(
                    ExpressionStatement(
                        InvocationExpression(
                            MemberAccessExpression(
                                SyntaxKind.SimpleMemberAccessExpression,
                                MemberAccessExpression(
                                    SyntaxKind.SimpleMemberAccessExpression,
                                    IdentifierName("System"),
                                    IdentifierName("Console")),
                                IdentifierName("WriteLine")))
                            .WithArgumentList(
                                ArgumentList(
                                    SingletonSeparatedList(
                                        Argument(
                                            IdentifierName("value")))))),
                    ReturnStatement(
                        IdentifierName("value"))))

    let private generateFunctions fns =
        [|
            for fn in fns do
                Function.generate fn
                    :> Syntax.MemberDeclarationSyntax
        |]

    let generateCompilationUnit assemblyName fns =

        let classNode =
            let methods = generateFunctions fns
            ClassDeclaration($"{assemblyName}Type")
                .AddMembers(mainMethod, printMethod)
                .AddMembers(items = methods)

        let namespaceNode =
            NamespaceDeclaration(
                IdentifierName(assemblyName : string))
                .AddMembers(classNode)

        let compilationUnit =
            CompilationUnit().AddMembers(namespaceNode)
        let mainTypeName =
            $"{namespaceNode.Name}.{classNode.Identifier}"
        compilationUnit, mainTypeName

    let generate assemblyName fns =

        let compilationUnit, mainTypeName =
            generateCompilationUnit assemblyName fns

        let compilation =
            let references : MetadataReference[] =
                [|
                    Net60.SystemRuntime
                    Net60.SystemConsole
                |]
            let options =
                CSharpCompilationOptions(OutputKind.ConsoleApplication)
                    .WithMainTypeName(mainTypeName)
            CSharpCompilation
                .Create(assemblyName)
                .AddSyntaxTrees(compilationUnit.SyntaxTree)
                .AddReferences(references)
                .WithOptions(options)
        let result = compilation.Emit($"{assemblyName}.dll")
        if result.Success then
            System.IO.File.Copy(
                "App.runtimeconfig.json",
                $"{assemblyName}.runtimeconfig.json",
                overwrite = true)
            Array.empty
        else
            result.Diagnostics
                |> Seq.map string
                |> Seq.toArray
