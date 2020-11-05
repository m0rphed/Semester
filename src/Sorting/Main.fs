namespace Sorting
module Main =   
    open Argu
    open System
    type CLIArguments =
        | SortBubble
        | SortBubbleList
        | QuickSortList
        | QuickArraySort1
        | QuickArraySort2   
        | Unpacking64to32
        | Unpacking64to16
        | Packing32to64
        | Packing16to64
        | QuickArraySort3
        interface IArgParserTemplate with
            member s.Usage =
                match s with
                | SortBubble _ -> "Sorting by bubble"
                | SortBubbleList _ -> "sortlist"
                | QuickSortList _ -> "Sorts quicker"
                | QuickArraySort2 _ -> "Sort Array quicker"
                | QuickArraySort1 _ -> "Sort Array quicker 2"
                | Unpacking64to32 _ -> "do what writes in func"
                | Unpacking64to16 _ -> "do what writes in func v.2.0"
                | Packing32to64 _ -> "Packing"
                | Packing16to64 _ -> "Packing 2"
                | QuickArraySort3 _ -> "New sort"
    [<EntryPoint>]
    let main (argv: string array) =
        let parser = ArgumentParser.Create<CLIArguments>(programName = "Sorting")
        try 
        let results = parser.Parse argv
        let Sort functionSort writeSmth readSmth =
            printfn ("Укажите путь к файлу")
            let path = Console.ReadLine()
            printfn ("Отсортированные данные %A") (functionSort (readSmth path))
            printfn ("Укажите путь, по которому перепишутся данные")
            let path1 = Console.ReadLine()
            writeSmth path1 (functionSort (readSmth path))
        let UnpackFunctions _function =
            printfn ("введите число, которое хотите распаковать")
            printfn ("Распакованные числа: %A") (_function (Console.ReadLine() |> int64))                 
        if results.Contains SortBubble
        then Sort Homework4.SortBubble Homework4.WriteArray Homework4.ReadfileArray
        elif results.Contains SortBubbleList
        then Sort Homework4.SortBubbleList Homework4.WriteList Homework4.ReadfileList 
        elif results.Contains QuickSortList
        then Sort Homework4.QuickSortList Homework4.WriteList Homework4.ReadfileList  
        elif results.Contains QuickArraySort2
        then Sort Homework4.QuickArraySort2 Homework4.WriteArray Homework4.ReadfileArray
        elif results.Contains QuickArraySort1
        then Sort Homework4.QuickArraySort1 Homework4.WriteArray Homework4.ReadfileArray
        elif results.Contains Unpacking64to16
        then UnpackFunctions Homework4.Unpacking64to16
        elif results.Contains Unpacking64to32
        then UnpackFunctions Homework4.Unpacking64to32
        elif results.Contains QuickArraySort3
        then Sort Homework4.QuickArraySort3 Homework4.WriteArray Homework4.ReadfileArray
        elif results.Contains Packing32to64
        then
            printfn ("Введите два числа")
            let x = Console.ReadLine() |> int32
            let y = Console.ReadLine() |> int32
            printfn ("%A") (Homework4.Packing32to64 (x, y))
        elif results.Contains Packing16to64
        then 
            printfn ("введите 4 числа")
            let x = Console.ReadLine() |> int16
            let y = Console.ReadLine() |> int16
            let z = Console.ReadLine() |> int16
            let v = Console.ReadLine() |> int16
            let k = Homework4.Packing16to64 (x, y, z, v)
            printfn ("%A") (Homework4.Packing16to64 (x, y, z, v))      
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
