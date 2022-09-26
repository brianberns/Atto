namespace Atto

open System.IO
open System.Reflection

module Main =

    [<EntryPoint>]
    let main args =
        try
            let path = Seq.exactlyOne args
            let input =
                let corePath =
                    Path.Combine(
                        Path.GetDirectoryName(
                            Assembly.GetExecutingAssembly().Location),
                        "core.at")
                File.ReadAllText corePath
                    + System.Environment.NewLine
                    + File.ReadAllText path
            let fileName = Path.GetFileNameWithoutExtension(path)
            match Parser.Program.parse input with
                | Choice1Of2 fns ->
                    let messages =
                        Generator.Program.generate fileName fns
                    for message in messages do
                        printfn "%s" message
                | Choice2Of2 message ->
                    printfn "%s" message
        with exn -> printfn "%A" exn
        0
