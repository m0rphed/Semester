module Main
open Argu
open System
open BoolMatrix
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
                    BoolMatrix.printListofPairs (BoolMatrix.readMatrix file)
                elif results.Contains MultiplyMatrixes
                then
                    printfn ("Укажите 3 пути")
                    let t = Console.ReadLine() 
                    let x = BoolMatrix.readMatrix t
                    let k = Console.ReadLine() 
                    let y = BoolMatrix.readMatrix k
                    let path = Console.ReadLine() 
                    BoolMatrix.writeOutputMatrix (BoolMatrix.returnMatrix (BoolMatrix.multiplyBool x y)) path
                elif results.Contains MultiplyReal
                then
                    let t = Console.ReadLine()
                    let k = BoolMatrix.readMatrixForTests t                   
                    let u = Console.ReadLine()
                    let o = BoolMatrix.readMatrixForTests u
                    printfn ("%A") (BoolMatrix.multiply k o)
                elif results.Contains Generator
                then
                    let k = Console.ReadLine() |> int
                    let n = Console.ReadLine() |> int
                    let t = Console.ReadLine() |> int
                    let fMatrix = generateRandomBoolMatrix (abs n) (abs k)
                    let sMatrix = generateRandomBoolMatrix (abs k) (abs t)
                    let first = returnMatrix (multiplyBool (createBoolMatrixFromStandart fMatrix) (createBoolMatrixFromStandart sMatrix))
                    printfn ("\n %A") first
                    let second = multiply fMatrix sMatrix
                    printfn ("\n %A") second
                elif results.Contains WriteRead
                then
                    let y = Console.ReadLine() |> int
                    let z = Console.ReadLine() |> int
                    let x = Console.ReadLine()
                    let u = generateRandomBoolMatrix y z
                    printfn ("\n %A") u
                    writeOutputMatrix u x
                    printfn ("\n %A") (returnMatrix (readMatrix x))                    
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
        
