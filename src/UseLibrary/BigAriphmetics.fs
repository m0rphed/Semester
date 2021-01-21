module BigAriphmetics
open System
open Listik

let transferPositive x = // для инверснутого листа переносит после суммы в другой разряд div 10
    let checksLastIndex =
        let mutable j = 1
        let mutable el = indexElem x (length x)
        while el >= 10 do
            j <- j + 1
            el <- el / 10
        j   
    let fElem, fTail =         
        if head x >= 10
        then (head x % 10), (Cons ((head (tail x) + head x / 10), tail (tail x)))
        else (head x), (tail x)
    fold
        (fun acc elem ->
            if length acc = 1
            then Cons (elem, acc)               
            elif head acc >= 10
            then Cons (elem + head acc / 10, Cons ((head acc % 10), tail acc))
            else Cons (elem, acc))
        (One fElem)
        (rev (addZero (rev fTail) checksLastIndex)) |> deleteZeroes

let transferNegative x = // для инверснутого листа переносит после разности в другой разряд div 10
    let fElem, fTail =
        if head x < 0 && head x % 10 = 0 
        then ((10 + (head x % 10)) % 10, (Cons ((head (tail x) + (head x / 10)), tail (tail x))))
        elif head x < 0 
        then ((10 + (head x % 10)) % 10, (Cons ((head (tail x) + (head x / 10) - 1), tail (tail x))))
        else (head x), (tail x)
    let checksLastIndex =
        let mutable j = 1
        let mutable el = indexElem x (length x)
        while el < 0 do
            j <- j + 1
            el <- el / 10
        j
    fold
        (fun acc elem ->
            if length acc = 1
            then Cons (elem, acc)
            elif head acc < 0 && head acc % 10 = 0 
            then Cons (elem + (head acc / 10), Cons (0, tail acc))
            elif head acc < 0
            then Cons (elem + (head acc /10) - 1, Cons ((10 + (head acc % 10)) % 10, tail acc))
            else Cons (elem, acc))
        (One fElem)
        (rev (addZero (rev fTail) checksLastIndex)) 

let subtract x y = // вычитание
    let fList, sList = (deleteZeroes x), (deleteZeroes y)
    let bigger, smaller = 
        if greatest fList sList = true
        then (addZero fList 3), (addZero sList (length fList - length sList + 3))
        else (addZero sList 3), (addZero fList (length sList - length fList + 3))
    let summedList = (map2 (-) (One ((head bigger) - (head smaller))) (tail bigger) (tail smaller))
    if greatest fList sList = true
    then deleteZeroes (transferNegative summedList)
    else
        let out = deleteZeroes (transferNegative summedList)
        if length out = 1
        then One (-1 * head out)
        else Cons (head out * -1, tail out)

let sum fList sList = // сумма
    let bigger, smaller = 
        if length fList > length sList
        then (addZero fList 3), (addZero sList (length fList - length sList + 3))
        else (addZero sList 3), (addZero fList (length sList - length fList + 3))       
    let summedList = (map2 (+) (One ((head bigger) + (head smaller))) (tail bigger) (tail smaller))
    transferPositive summedList

let multiply fList sList = // перемножение 
    let bigger, smaller = 
        if length fList > length sList
        then (addZero fList 3), (rev (addZero sList (length fList - length sList + 3)))
        else (addZero sList 3), (rev (addZero fList (length sList - length fList + 3)))
    let firstMultiply = fold (fun acc elem -> Cons ((elem * head smaller), acc)) (One (head bigger * head smaller)) (tail bigger) |> rev
    let mutable k = 1
    fold (fun acc elem ->
                k <- k + 1            
                sum
                    ((addZero (fold
                                (fun acc1 elem1 -> Cons ((elem1 * elem), acc1))
                                (One (head bigger * elem))
                                (tail bigger)) k) |> rev)
                    acc)
            firstMultiply
            (tail smaller) |> deleteZeroes

let division x y = // деление
    let fList, sList = (deleteZeroes x), (deleteZeroes y)
    if sList = One 0
    then failwith "cannot divide"              
    elif greatest fList sList = false 
    then One 0
    else 
        let mutable k, t = 1, 1
        while greatest (choosePart fList 1 k) sList = false do
            k <- k + 1
        while head (subtract (choosePart fList 1 k) (multiply (One t) sList)) >= 0 do
            t <- t + 1
        if k + 1 > length fList
        then k <- k - 1                    
        if length fList = 1 || ((length fList = length sList || length fList = length sList + 1) && greatest (subtract fList (multiply (One (t - 1)) sList)) sList = false) 
        then One (t - 1)
        else
            fold
                (fun (acc, current) elem -> 
                    if greatest (concat current (One elem)) sList = false
                    then ((concat acc (One 0)),concat current (One elem))
                    else
                        let mutable j = 1
                        while head (subtract (concat current (One elem)) (multiply (One j) sList)) >= 0 do
                            j <- j + 1
                        (concat acc (One (j - 1))), (subtract (concat current (One elem)) (multiply (One (j - 1)) sList)))
                ((One (t - 1)), (subtract (choosePart fList 1 k) (multiply (One (t - 1)) sList)))
                (choosePart fList (k + 1) (length fList)) |> fst

                
                
                
