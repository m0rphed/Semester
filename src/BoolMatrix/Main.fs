module Main
open Argu
open System
type CLIArguments =
    | ReadMatrix
    | MultiplyMatrixes
    | MultiplyReal
    | Generator
    | WriteRead
    interface IArgParserTemplate with
           member s.Usage =
               match s with
               | ReadMatrix _ -> "read matrix"
               | MultiplyMatrixes _ -> "multiplies"
               | MultiplyReal _ -> "mltpl matrixes"
               | Generator _ -> "generates"
               | WriteRead _ -> "writes and reads"
[<EntryPoint>]
        let main (argv: string array) =
            try
                let parser = ArgumentParser.Create<CLIArguments>(programName = "BoolMatrix")
                let results = parser.Parse(argv)
                if results.Contains ReadMatrix
                then
                    let file = Console.ReadLine() 
                    bMatrix.printListofPairs (bMatrix.readMatrix file)
                elif results.Contains MultiplyMatrixes
                then
                    printfn ("Укажите 3 пути")
                    let t = Console.ReadLine() 
                    let x = bMatrix.readMatrix t
                    let k = Console.ReadLine() 
                    let y = bMatrix.readMatrix k
                    let path = Console.ReadLine() 
                    bMatrix.writeOutputMatrix (bMatrix.returnMatrix (bMatrix.multiplyBool x y)) path
                elif results.Contains MultiplyReal
                then
                    let t = Console.ReadLine()
                    let k = bMatrix.readMatrixForTests t                   
                    let u = Console.ReadLine()
                    let o = bMatrix.readMatrixForTests u
                    printfn ("%A") (bMatrix.multiply k o)
                elif results.Contains Generator
                then
                    let k = Console.ReadLine() |> int
                    let n = Console.ReadLine() |> int
                    let t = Console.ReadLine() |> int
                    let fMatrix = bMatrix.generateRandomBoolMatrix (abs n) (abs k)
                    let sMatrix = bMatrix.generateRandomBoolMatrix (abs k) (abs t)
                    let first = bMatrix.returnMatrix (bMatrix.multiplyBool (bMatrix.createBoolMatrixFromStandart fMatrix) (bMatrix.createBoolMatrixFromStandart sMatrix))
                    printfn ("\n %A") first
                    let second = bMatrix.multiply fMatrix sMatrix
                    printfn ("\n %A") second
                elif results.Contains WriteRead
                then
                    let y = Console.ReadLine() |> int
                    let z = Console.ReadLine() |> int
                    let x = Console.ReadLine()
                    let u = bMatrix.generateRandomBoolMatrix y z
                    printfn ("\n %A") u
                    bMatrix.writeOutputMatrix u x
                    printfn ("\n %A") (bMatrix.returnMatrix (bMatrix.readMatrix x))                    
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
        
