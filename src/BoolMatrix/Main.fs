module Main
open Argu
open System
open bMatrix
type CLIArguments =
    | ReadMatrix
    | MultiplyMatrixes
    interface IArgParserTemplate with
           member s.Usage =
               match s with
               | ReadMatrix _ -> "read matrix"
               | MultiplyMatrixes _ -> "multiplies"
[<EntryPoint>]
        let main (argv: string array) =
            try
                let parser = ArgumentParser.Create<CLIArguments>(programName = "BoolMatrix")
                let results = parser.Parse(argv)
                if results.Contains ReadMatrix
                then
                    let file = Console.ReadLine() 
                    printListofPairs (readMatrix file)
                elif results.Contains MultiplyMatrixes
                then
                    printfn ("Укажите 3 пути")
                    let t = Console.ReadLine() 
                    let x = readMatrix t
                    let k = Console.ReadLine() 
                    let y = readMatrix k
                    let path = Console.ReadLine() 
                    writeOutputMatrix (returnMatrix (multiplyBool x y)) path                              
                else           
                    parser.PrintUsage() |> printfn "%s"
                0
            with
            | :? ArguParseException as ex ->
                printfn "%s" ex.Message
                1
            | ex ->
                printfn "Internal Error:"
                printfn "%s" ex.Message
                2
        
