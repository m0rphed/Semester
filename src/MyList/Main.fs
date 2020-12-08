module Main 
open Listik  
open Argu
open System
open Tree

type CLIArguments =
    | FuncToMyList
    | FuncMyStringMyTree
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | FuncToMyList _ -> "functions to my list"
            | FuncMyStringMyTree _ -> "functions to my string and tree"
[<EntryPoint>]
    let main (argv: string array) =
        try
            let parser = ArgumentParser.Create<CLIArguments>(programName = "MyList")
            let results = parser.Parse(argv)
            if results.Contains FuncToMyList
            then
                printfn "Итак, есть несколько функций, какую протестировать? \n 1) Length \n 2) Concat \n 3) Sort \n 4) Iter \n 5) Map \n 6) ConvertToList " 
                let x = Console.ReadLine() |> int
                let myList = generator 5
                if x = 1
                then
                    printfn "Вот такой листик сгенерировался %A" myList
                    printfn "Длина равна: %A" (length myList)
                elif x = 2
                then
                    let myList1 = generator 5 
                    printfn "Вот такой первый листик сгенерировался %A" myList
                    printfn "Вот такой второй листик сгенерировался %A" myList1
                    printfn "Сконкатенированный лист: \n %A" (concat myList myList1)
                elif x = 3
                then
                    printfn "Вот такой листик сгенерировался %A" myList
                    printfn "Отсортируем лист \n %A" (sort myList)
                elif x = 4
                then
                    printfn "Вот такой листик сгенерировался %A" myList
                    printfn "выпишем все элементы"
                    (iter (printfn "%A") myList)
                elif x = 5
                then
                    printfn "Вот такой листик сгенерировался %A" myList
                    printfn "Прибавим 2 к каждому элементу и выведем новый лист \n %A" (map ((+) 2) myList)
                elif x = 6
                then
                    printfn "Вот такой листик сгенерировался %A" myList
                    printfn "Сконвертим в лист \n %A" (toDefoltList myList)
            elif results.Contains FuncMyStringMyTree
            then          
                printfn "Итак, есть несколько функций, какую попробовать? \n 1) Concat \n 2) FromMyStringToString,FromStringToMyString \n 3) AverageInMyTree \n 4) MaxIntMyTree "
                let x = Console.ReadLine() |> int
                if x = 1
                then
                    printfn "Введите две строчки: "
                    let y = Cons ('п', Cons ('р', Cons ('и', Cons ('в', Cons ('е', One 'т')))))
                    let z = Cons ('т', Cons ('е', Cons ('в', Cons ('и', Cons ('р', One 'п')))))
                    printfn "%A \n %A " y z 
                    printfn "Сконкатенированная строка : %A " (concatMyString y z)
                elif x = 2
                then
                    let y = Cons ('п', Cons ('р', Cons ('и', Cons ('в', Cons ('е', One 'т')))))
                    printfn "Допустим есть такой MyList : \n %A" y
                    printfn "Переведем в строку %A" (toString y)
                    printfn "И обратно %A" (toMyString (toString y))
                elif x = 3
                then
                     let mTree =(Node ((-200), Cons (Node (-100000, One (Leaf (-500))),One (Node ((100000), Cons (Node (105, One (Leaf (200))),One (Node (1, One (Leaf (6))))))))))
                     let sMTree = (Node ((15), Cons ((Leaf (13)), One (Leaf (10)))))
                     printfn "Пусть у нас есть два таких дерева \n %A \n %A" mTree sMTree
                     printfn "Среднее значение у первого равно: %A" (average mTree)
                     printfn "Среднее значение у первого равно: %A" (average sMTree)
                elif x = 4
                then
                    let mTree = (Node ((100), Cons (Node (101, One (Leaf (102))),One (Node (103, One (Leaf (2)))))))
                    let sMTree = (Node ((-100), Cons ((Leaf (-1)), One (Leaf (10)))))
                    printfn "Пусть у нас есть два таких дерева \n %A \n %A" mTree sMTree
                    printfn "Максимальное значение у первого равно: %A" (max mTree)
                    printfn "Максимальное значение у первого равно: %A" (max sMTree)
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
