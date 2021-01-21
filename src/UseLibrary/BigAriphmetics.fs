module BigAriphmetics
open System
open Listik

// добавляет 0 впереди числа
let rec addZeroBeginning acc k = 
    match k with
    | k when k = 1 -> acc
    | k -> addZeroBeginning (Cons (0, acc)) (k - 1)

// добавляет 0 в конце числа 
let addZeroEnd x k = rev (addZeroBeginning (rev x) k)

// возвращает true если x >= y иначе false для big int
let compareBigInt x y =
    if length x > length y then true
    elif length y > length x then false
    else
        let compareInt x y = if x = max x y then true else false 
        let rec _go x y =
            match x, y with
            | One t, One t1 -> compareInt t t1
            | Cons (hd, tl), One t -> true
            | One t, Cons (hd, tl) -> false 
            | Cons (hd, tl), Cons (hd1, tl1) ->  if hd = hd1 then _go tl tl1 else compareInt hd hd1
        _go x y

// возвращает false если x <> y иначе true
let equals x y = 
    if length x <> length y then false
    else 
        let rec _go x y =
            match x with
            | One t -> if t = head y then true else false
            | Cons (hd, tl) -> if hd = head y then _go (tail x) (tail y) else false
        _go x y 

let deleteZeroes x = // удаляет нули незначащие
    let mutable flag = false
    iter (fun elem -> if elem <> 0 then flag <- true) x
    if flag = true
    then
        let rec _go acc =
            match head acc with
            | 0 -> _go (tail acc)
            | _ -> acc
        _go x
    else One 0 

(*
    функции transferPositive и transferNegarive для инверснутого листа переносят после суммы в другой разряд div 10
    checksLastIndex нужна для проверки последнего индекса на кратность 10,
    чтобы выделить дополнительное место, т.к. если это не сделать, в последнем индексе может оказаться число >=10
    fElem, fTail первая итерация для рекурсии
*)
let checksLastIndex x =
    let mutable j = 1
    let mutable el = indexElem x (length x)
    while el < 0 do
        j <- j + 1
        el <- el / 10
    j

let transferPositive x = 
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
        (addZeroEnd fTail (checksLastIndex x)) |> deleteZeroes

let transferNegative x = 
    let fElem, fTail =
        if head x < 0 && head x % 10 = 0 
        then ((10 + (head x % 10)) % 10, (Cons ((head (tail x) + (head x / 10)), tail (tail x))))
        elif head x < 0 
        then ((10 + (head x % 10)) % 10, (Cons ((head (tail x) + (head x / 10) - 1), tail (tail x))))
        else (head x), (tail x)
    fold
        (fun acc elem ->
            if length acc = 1
            then Cons (elem, acc)
            elif head acc < 0 && head acc % 10 = 0 
            then Cons (elem + (head acc / 10), Cons (0, tail acc))
            elif head acc < 0
            then Cons (elem + (head acc / 10) - 1, Cons ((10 + (head acc % 10)) % 10, tail acc))
            else Cons (elem, acc))
        (One fElem)
        (addZeroEnd fTail (checksLastIndex x))

// addZeroBeginning fList 3 нужно для того чтобы не работать с числами длины 2 и 1, так бы пришлось писать еще условия
// так же мы выравниваем числа до одинаковой длины, так удобнее работать
let subtract x y = 
    let fList, sList = (deleteZeroes x), (deleteZeroes y)
    let bigger, smaller = 
        if compareBigInt fList sList = true
        then (addZeroBeginning fList 3), (addZeroBeginning sList (length fList - length sList + 3))
        else (addZeroBeginning sList 3), (addZeroBeginning fList (length sList - length fList + 3))
    let summedList = (map2 (-) (One ((head bigger) - (head smaller))) (tail bigger) (tail smaller))
    if compareBigInt fList sList = true
    then deleteZeroes (transferNegative summedList)
    else
        // если отриц число, то ставим -1 иначе ничего не делаем
        let out = deleteZeroes (transferNegative summedList)
        if length out = 1
        then One (-1 * head out)
        else Cons (head out * -1, tail out)

let sum fList sList = 
    let bigger, smaller = 
        if length fList > length sList
        then (addZeroBeginning fList 3), (addZeroBeginning sList (length fList - length sList + 3))
        else (addZeroBeginning sList 3), (addZeroBeginning fList (length sList - length fList + 3))       
    let summedList = (map2 (+) (One ((head bigger) + (head smaller))) (tail bigger) (tail smaller))
    transferPositive summedList

let multiply fList sList =
    let bigger, smaller = 
        if length fList > length sList
        then (addZeroBeginning fList 3), (rev (addZeroBeginning sList (length fList - length sList + 3)))
        else (addZeroBeginning sList 3), (rev (addZeroBeginning fList (length sList - length fList + 3)))
    let firstMultiply = fold (fun acc elem -> Cons ((elem * head smaller), acc)) (One (head bigger * head smaller)) (tail bigger) |> rev
    let mutable k = 1
    fold
        (fun acc elem ->
            k <- k + 1            
            sum
                ((addZeroBeginning
                    (fold
                        (fun acc1 elem1 -> Cons ((elem1 * elem), acc1))
                        (One (head bigger * elem))
                        (tail bigger)) k) |> rev)
                acc)
        firstMultiply
        (tail smaller) |> deleteZeroes

let division x y =
    // микро оптимизация 
    let fList, sList = (deleteZeroes x), (deleteZeroes y)
    if sList = One 0
    then failwith "cannot divide"              
    elif compareBigInt fList sList = false then One 0
    else
        // первая итерация для рекурсии
        let mutable k, t = 1, 1
        while compareBigInt (choosePart fList 1 k) sList = false do k <- k + 1
        while head (subtract (choosePart fList 1 k) (multiply (One t) sList)) >= 0 do t <- t + 1
        if k + 1 > length fList then k <- k - 1
        // обработка вырожденного случая
        if length fList = 1 || ((length fList = length sList || length fList = length sList + 1) && compareBigInt (subtract fList (multiply (One (t - 1)) sList)) sList = false) 
        then One (t - 1)
        else
            fold
                (fun (acc, current) elem -> 
                    if compareBigInt (concat current (One elem)) sList = false
                    then ((concat acc (One 0)),concat current (One elem))
                    else
                        let mutable j = 1
                        while head (subtract (concat current (One elem)) (multiply (One j) sList)) >= 0 do j <- j + 1
                        (concat acc (One (j - 1))), (subtract (concat current (One elem)) (multiply (One (j - 1)) sList)))
                ((One (t - 1)), (subtract (choosePart fList 1 k) (multiply (One (t - 1)) sList)))
                (choosePart fList (k + 1) (length fList)) |> fst

                
                
                
