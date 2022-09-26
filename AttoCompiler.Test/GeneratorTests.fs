namespace Atto.Generator

open Microsoft.CodeAnalysis
open Microsoft.VisualStudio.TestTools.UnitTesting

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

        match Atto.Parser.Program.parse input with
            | Choice1Of2 fns ->
                let actual =
                    let compilationUnit, _ =
                        Program.generateCompilationUnit "fib" fns
                    compilationUnit.NormalizeWhitespace().ToString()
                Assert.AreEqual<_>(expected, actual)
            | Choice2Of2 message -> Assert.Fail(message)
