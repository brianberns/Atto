namespace Atto.Generator

open Microsoft.CodeAnalysis
open Microsoft.VisualStudio.TestTools.UnitTesting
open FParsec

[<TestClass>]
type GeneratorTests() =

    [<TestMethod>]
    member _.Fibonacci() =

        let input =
            """
            fn fib n is
                if = n 0
                    1
                    * n fib - n 1

            fn main is
                print fib 10
            """

        let expected =
            """
namespace fib
{
    class fibType
    {
        static void Main()
        {
            main();
        }

        static object Print(object value)
        {
            System.Console.WriteLine(value);
            return value;
        }

        static object fib(object n) => System.Object.Equals(n, 0) ? 1 : (double)n * (double)fib((double)n - (double)1);
        static object main() => Print(fib(10));
    }
}
            """.Trim()

        let parse input =
            match runParserOnString Atto.Parser.Program.parse Map.empty "" input with
                | Success (fnMap, _, _) ->
                    fnMap
                | Failure (message, _, _) ->
                    failwith message

        let actual =
            let compilationUnit, _ =
                parse input
                    |> Program.generateCompilationUnit "fib"
            compilationUnit.NormalizeWhitespace().ToString()

        Assert.AreEqual<_>(expected, actual)
