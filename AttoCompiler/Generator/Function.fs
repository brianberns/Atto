namespace Atto.Generator

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open type SyntaxFactory

open Atto

module Function =

    let private objType =
        PredefinedType(
            Token(SyntaxKind.ObjectKeyword))

    let private generateParameter (id : Parser.Identifier) =
        Parameter(Identifier(id.String))
            .WithType(objType)

    let private generateParameterList ids =
        let comma =
            Token(SyntaxKind.CommaToken)
                |> SyntaxNodeOrToken.op_Implicit
        seq {
            for i, id in Seq.indexed ids do
                if i > 0 then comma
                generateParameter id
                    |> SyntaxNodeOrToken.op_Implicit
        }
            |> SeparatedList
            |> ParameterList

    let rec private generateExpr expr : Syntax.ExpressionSyntax =
        match expr with
            | Parser.Literal (Parser.Num num) ->
                LiteralExpression(
                    SyntaxKind.NumericLiteralExpression,
                    Literal(num))
            | Parser.Literal (Parser.Str str) ->
                LiteralExpression(
                    SyntaxKind.StringLiteralExpression,
                    Literal(str))
            | Parser.Name id ->
                IdentifierName(id.String)
            | Parser.Call (fnName, args) ->
                InvocationExpression(
                    IdentifierName(fnName.String))
                    .WithArgumentList(generateArgumentList args)
            | Parser.Equal (left, right) ->
                InvocationExpression(
                    MemberAccessExpression(
                        SyntaxKind.SimpleMemberAccessExpression,
                        MemberAccessExpression(
                            SyntaxKind.SimpleMemberAccessExpression,
                            IdentifierName("System"),
                            IdentifierName("Object")),
                        IdentifierName("Equals")))
                    .WithArgumentList(
                        generateArgumentList [|left; right|])
            | Parser.Operation (op, left, right) ->
                let opKind =
                    match op with
                        | Parser.Addition -> SyntaxKind.AddExpression
                        | Parser.Subtraction -> SyntaxKind.SubtractExpression
                        | Parser.Multiplication -> SyntaxKind.MultiplyExpression
                let cast node =
                    CastExpression(
                        PredefinedType(
                            Token(SyntaxKind.DoubleKeyword)),
                        node)
                BinaryExpression(
                    opKind,
                    generateExpr left |> cast,
                    generateExpr right |> cast)
            | Parser.If (pred, ifTrue, ifFalse) ->
                ConditionalExpression(
                    generateExpr pred,
                    generateExpr ifTrue,
                    generateExpr ifFalse)
            | Parser.Print expr ->
                InvocationExpression(
                    IdentifierName("Print"))
                    .WithArgumentList(
                        generateArgumentList [| expr |])

    and private generateArgument expr =
        Argument(generateExpr expr)

    and private generateArgumentList exprs =
        let comma =
            Token(SyntaxKind.CommaToken)
                |> SyntaxNodeOrToken.op_Implicit
        seq {
            for i, expr in Seq.indexed exprs do
                if i > 0 then comma
                generateArgument expr
                    |> SyntaxNodeOrToken.op_Implicit
        }
            |> SeparatedList
            |> ArgumentList

    let generate (fn : Parser.Function) =
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
