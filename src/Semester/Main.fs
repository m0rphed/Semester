namespace Semester
module Main =
    
    open Argu
    open System
    type CLIArguments =
        | Ftask of x: float
        | Stask of x: float
        | Thirdtask 
        | Fourthtask
        | Fifthtask
        | Sixthtask       
        interface IArgParserTemplate with
            member s.Usage =
                match s with
                | Ftask _ -> "Calculates the value of the polynomial"
                | Stask _ -> "Calculates the value of the polynomial in less time"
                | Thirdtask _ -> "Calculates indexes of array elements no larger than the specified number"
                | Fourthtask _ -> "Calculates indexes of array elements that lie outside the range specified by numbers"
                | Fifthtask _ -> "Changing the array elements in some places"
                | Sixthtask _ -> "Changes element i to element j"                
    [<EntryPoint>]
     let main (argv: string array) =
        try
         let parser = ArgumentParser.Create<CLIArguments>(programName = "Semester")
         let results = parser.Parse(argv)
         if results.Contains Ftask then
             let x = results.GetResult(Ftask)
             let result =  domashka.Ftask x
             printfn "Контрольное значение :"
             printfn "%A" result
         elif results.Contains Stask then
             let q = results.GetResult(Stask)
             let result = domashka.Stask q
             printf "Контрольное значение :"
             printf "%A" result
         elif results.Contains Thirdtask then
             printfn ("Введите размерность массива:")
             let x = Console.ReadLine() |> int 
             let array = domashka.create_array x             
             printfn ("Введите число, меньше которого будут выведены индексы:")
             let bigelement = Console.ReadLine() |> int
             if bigelement < 0 then printfn ("Введите размерность >0")
             else
                let secondarray = domashka.Thirdtask array  bigelement
                printfn ("%A:") secondarray
         elif results.Contains Fourthtask then
             printfn ("Введите размерность массива ")
             let x = Console.ReadLine() |> int 
             let array = domashka.create_array x 
             printfn ("Введите левую границу :")
             let felement = Console.ReadLine() |> int
             printfn ("Введите правую границу :")
             let selement = Console.ReadLine() |> int
             let thirdarray = domashka.Fourthtask array felement selement 
             printfn ("Индексы элементов вне диапазона :")
             printfn ("%A") thirdarray
         elif results.Contains Fifthtask then            
             let dimensionArray = 2          
             let array = domashka.create_array dimensionArray
             let arrayizm = domashka.Fifthtask array
             printfn ("Измененные элементы массива")
             printfn ("%A") arrayizm
         elif results.Contains Sixthtask then
             printfn ("Введите размерность массива ")
             let dimensionArray = Console.ReadLine() |> int
             if dimensionArray < 2 then failwith ("Array needs at least 2 elements")
             else
                 let array: int array  = domashka.create_array dimensionArray
                 let z = Console.ReadLine() |> int
                 let v = Console.ReadLine() |> int
                 if (z >=0 ) && (z <= dimensionArray - 1) && (v >= 0) && (v <= dimensionArray - 1) && (z <> v) then
                     let arrayizmeneniy = domashka.Sixthtask array z v 
                     printfn ("Измененный массив")
                     printfn ("%A") arrayizmeneniy
                 else printfn ("Введенные значения выходят за границы массива")            
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
