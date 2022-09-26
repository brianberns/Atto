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

    let private generateSeparatedList nodes =
        let comma =
            SyntaxKind.CommaToken
                |> Token
                |> SyntaxNodeOrToken.op_Implicit
        seq {
            for i, (node : #SyntaxNode) in Seq.indexed nodes do
                if i > 0 then comma
                SyntaxNodeOrToken.op_Implicit node
        } |> SeparatedList

    let private generateParameterList ids =
        ids
            |> Seq.map generateParameter
            |> generateSeparatedList
            |> ParameterList

    let private generateLiteral = function
        | Parser.Num num ->
            LiteralExpression(
                SyntaxKind.NumericLiteralExpression,
                Literal(num))
        | Parser.Str str ->
            LiteralExpression(
                SyntaxKind.StringLiteralExpression,
                Literal(str))

    let rec private generateArgumentList exprs =
        exprs
            |> Seq.map (generateExpr >> Argument)
            |> generateSeparatedList
            |> ArgumentList

    and private generateCall (fnName : Parser.Identifier) args =
        InvocationExpression(
            IdentifierName(fnName.String))
            .WithArgumentList(generateArgumentList args)

    and private geneateEqual left right =
        InvocationExpression(
            MemberAccessExpression(
                SyntaxKind.SimpleMemberAccessExpression,
                MemberAccessExpression(
                    SyntaxKind.SimpleMemberAccessExpression,
                    IdentifierName("System"),
                    IdentifierName("Object")),
                IdentifierName("Equals")))
            .WithArgumentList(
                generateArgumentList [| left; right |])

    and private generateOperation op left right =
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

    and private generateIf pred ifTrue ifFalse =
        ConditionalExpression(
            generateExpr pred,
            generateExpr ifTrue,
            generateExpr ifFalse)

    and private generatePrint expr =
        InvocationExpression(
            IdentifierName("Print"))
            .WithArgumentList(
                generateArgumentList [| expr |])

    and private generateExpr expr : Syntax.ExpressionSyntax =
        match expr with
            | Parser.Literal literal -> generateLiteral literal
            | Parser.Name id -> IdentifierName(id.String)
            | Parser.Call (fnName, args) -> generateCall fnName args
            | Parser.Equal (left, right) -> geneateEqual left right
            | Parser.Operation (op, left, right) ->
                generateOperation op left right
            | Parser.If (pred, ifTrue, ifFalse) ->
                generateIf pred ifTrue ifFalse
            | Parser.Print expr -> generatePrint expr

    let generate (fn : Parser.Function) =
        MethodDeclaration(
            returnType = objType,
            identifier = fn.Name.String)
            .AddModifiers(
                Token(SyntaxKind.StaticKeyword))
            .WithParameterList(
                generateParameterList fn.Args)
            .WithExpressionBody(
                fn.Expr
                    |> generateExpr
                    |> ArrowExpressionClause)
            .WithSemicolonToken(
                Token(SyntaxKind.SemicolonToken))
