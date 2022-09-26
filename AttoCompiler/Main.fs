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

    let objType =
        PredefinedType(
            Token(SyntaxKind.ObjectKeyword))

    let generateParameter (id : Identifier) =
        Parameter(SyntaxFactory.Identifier(id.String))
            .WithType(objType)

    let generateParameterList ids =
        let comma =
            Token(SyntaxKind.CommaToken)
                |> SyntaxNodeOrToken.op_Implicit
        seq {
            for i, id in Seq.indexed ids do
                if i > 0 then comma
                else generateParameter id
                    |> SyntaxNodeOrToken.op_Implicit
        }
            |> SeparatedList
            |> ParameterList

    let rec generateExpr expr : Syntax.ExpressionSyntax =
        match expr with
            | Literal (Num num) ->
                LiteralExpression(
                    SyntaxKind.NumericLiteralExpression,
                    SyntaxFactory.Literal(num))
            | Literal (Str str) ->
                LiteralExpression(
                    SyntaxKind.StringLiteralExpression,
                    SyntaxFactory.Literal(str))
            | Name id ->
                IdentifierName(id.String)
            | Call (fnName, args) ->
                InvocationExpression(
                    IdentifierName(fnName.String))
                    .WithArgumentList(generateArgumentList args)
            | Equal (left, right) ->
                BinaryExpression(
                    SyntaxKind.EqualsExpression,
                    generateExpr left,
                    generateExpr right)
            | If (pred, ifTrue, ifFalse) ->
                ConditionalExpression(
                    generateExpr pred,
                    generateExpr ifTrue,
                    generateExpr ifFalse)
            | _ ->
                LiteralExpression(
                    SyntaxKind.NullLiteralExpression)

    and generateArgument expr =
        Argument(generateExpr expr)

    and generateArgumentList exprs =
        let comma =
            Token(SyntaxKind.CommaToken)
                |> SyntaxNodeOrToken.op_Implicit
        seq {
            for i, expr in Seq.indexed exprs do
                if i > 0 then comma
                else generateArgument expr
                    |> SyntaxNodeOrToken.op_Implicit
        }
            |> SeparatedList
            |> ArgumentList

    let generateFunction fn =
        MethodDeclaration(
            returnType = objType,
            identifier = fn.Name.String)
            .AddModifiers(
                Token(SyntaxKind.StaticKeyword))
            .WithParameterList(
                generateParameterList fn.Args)
            .WithExpressionBody(
                generateExpr fn.Expr
                    |> ArrowExpressionClause)
            .WithSemicolonToken(
                Token(SyntaxKind.SemicolonToken))
            :> Syntax.MemberDeclarationSyntax

    let generateFunctions fns =
        [|
            for fn in fns do
                generateFunction fn
        |]

    let generate (assemblyName : string) fns =

        let mainMethod =

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

        let classNode =
            let members = generateFunctions fns
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
            |> Map.values
            |> generate "fib"
        0
