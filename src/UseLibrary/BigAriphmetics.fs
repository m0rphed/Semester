module BigAriphmetics
open System
open Listik

let rev x = // реверс
    toMyList (List.rev (toDefoltList x))

let indexElem x i = // индекс i листа x 
    let mutable firstElem =
        match x with
        | One t ->  t
        | Cons (hd, tl) -> hd
    let mutable k = 1
    iter (fun elem -> if k = i then firstElem <- elem; k <- k + 1 else k <- k + 1) x
    firstElem

let tail x = // хвост листа 
    toMyList ((toDefoltList x).Tail)

let head x = // голова листа
    (toDefoltList x).Head

let rec addZero acc k = // добавляет 0 
       match k with
       | k when k = 1 -> acc
       | k -> addZero (Cons (0, acc)) (k - 1)

let choosePart x k i = // выбирает кусок от k до i в листе 
    if k = i
    then One (indexElem x k)
    else
        let rec _go acc k =
            match k with
            | k when k = i -> Cons (indexElem x k, acc)
            | k -> _go (Cons (indexElem x k, acc)) (k + 1)
        rev (_go (One (indexElem x k)) (k + 1))

let equals x y =
    if length x <> length y then false
    else 
        let rec _go x y =
            match x with
            | One t ->
                if t = head y
                then true
                else false
            | Cons (hd, tl) ->
                if hd = head y
                then _go (tail x) (tail y)
                else false
        _go x y 

let deleteZeroes x = // удаляет нули незначащие
    let mutable flag = 0
    iter (fun elem -> if elem <> 0 then flag <- 1) x
    if flag = 0
    then One 0
    else
        let rec _go acc =
            match head acc with
            | 0 -> _go (tail acc)
            | _ -> acc
        _go x

let rec map2 f acc x y = // map2
    match x, y with
    | One t, One k -> Cons (f t k, acc)
    | Cons (hd, tl), One t -> Cons (f hd t, acc)
    | One t, Cons (hd, tl) -> Cons (f t hd, acc)  
    | Cons (hd, tl), Cons(hd1, tl1) -> map2 f (Cons (f hd hd1, acc)) tl tl1

let transferPositive x = // для инверснутого листа переносит после суммы в другой разряд div 10
    let mutable j = 1
    let mutable el = indexElem x (length x)
    let checksLastIndex =
        while el >= 10 do
            j <- j + 1
            el <- el / 10
        j   
    let helper1, helper2 =         
        if head x >= 10
        then (head x % 10), (Cons ((head (tail x) + head x / 10), tail (tail x)))
        else (head x), (tail x)
    fold (fun acc elem ->
                if length acc = 1
                then Cons (elem, acc)               
                elif head acc >= 10
                then Cons (elem + head acc / 10, Cons ((head acc % 10), tail acc))
                else Cons (elem, acc))
            (One helper1)
            (rev (addZero (rev helper2) checksLastIndex)) |> deleteZeroes

let greatest x y = // выясняет кто больше из двух листов 
    let rec _go x y =
        match x with
        | One t ->
            if length y > 1
            then false
            elif length y = 1
            then
                if t = head y
                then true
                elif t > head y
                then true
                else false
            else true
        | Cons (hd, tl) ->
            if length y = length (Cons (hd, tl))
            then
                if hd > head y
                then true
                elif hd = head y
                then _go tl (tail y)
                else false
            elif length y > length (Cons (hd, tl))
            then false
            else true
    _go x y

let transferNegative y = // для инверснутого листа переносит после разности в другой разряд div 10
    if length y = 1
    then y
    elif length y = 2
    then
        if head y < 0 && head y % 10 = 0 
        then concat (One (head (tail y) + (head y / 10))) (One (10 + ((head y) % 10) % 10))
        elif head y < 0 
        then concat (One (head (tail y) + (head y / 10) - 1)) (One (10 + ((head y) % 10) % 10))
        else rev y 
    else
        let x = y
        let helper1, helper2 =
            if head x < 0 && head x % 10 = 0 
            then ((10 + (head x % 10)) % 10, (Cons ((head (tail x) + (head x / 10)), tail (tail x))))
            elif head x < 0 
            then ((10 + (head x % 10)) % 10, (Cons ((head (tail x) + (head x / 10) - 1), tail (tail x))))
            else (head x), (tail x)
        let mutable j = 1
        let mutable el = indexElem x (length x)
        let checksLastIndex =
            while el < 0 do
                j <- j + 1
                el <- el / 10
            j
        fold (fun acc elem ->
                    if length acc = 1
                    then Cons (elem, acc)
                    elif head acc < 0 && head acc % 10 = 0 
                    then Cons (elem + (head acc / 10), Cons (0, tail acc))
                    elif head acc < 0
                    then Cons (elem + (head acc /10) - 1, Cons ((10 + (head acc % 10)) % 10, tail acc))
                    else Cons (elem, acc))
                (One helper1)
                (rev (addZero (rev helper2) checksLastIndex)) 

let subtract x y = // вычитание
    let fList, sList = (deleteZeroes x), (deleteZeroes y)
    let mutable flag = 0
    let newList, newList1 = 
        if length fList > length sList
        then fList, (addZero sList (length fList - length sList + 1))
        elif length fList = length sList 
        then
            if greatest fList sList = true
            then
                flag <- 0
                (Cons (0, fList)),(Cons (0, sList))
            else
                flag <- 1
                (Cons (0, sList)), (Cons (0, fList))
        else
            flag <- 1
            sList, (addZero fList (length sList - length fList + 1))
    let summedList = (map2 (-) (One ((head newList) - (head newList1))) (tail newList) (tail newList1))
    printfn "%A" summedList
    if flag = 0
    then deleteZeroes (transferNegative summedList)
    else
        let out = deleteZeroes (transferNegative summedList)
        if length out = 1
        then One (-1 * head out)
        else Cons (head out * -1, tail out)

let sum fList sList = // сумма
    let newList, newList1 = 
        if length fList > length sList
        then (addZero fList 3), (addZero sList (length fList - length sList + 3))
        else (addZero sList 3), (addZero fList (length sList - length fList + 3))       
    let summedList = (map2 (+) (One ((head newList) + (head newList1))) (tail newList) (tail newList1))
    transferPositive summedList

let multiply fList sList = // перемножение 
    let newList, newList1 = 
        if length fList > length sList
        then (addZero fList 3), (rev (addZero sList (length fList - length sList + 3)))
        else (addZero sList 3), (rev (addZero fList (length sList - length fList + 3)))
    let firstIteration = fold (fun acc elem -> Cons ((elem * head newList1), acc)) (One (head newList * head newList1)) (tail newList) |> rev
    let mutable k = 1
    fold (fun acc elem ->
                k <- k + 1            
                sum
                    ((addZero (fold
                                (fun acc1 elem1 -> Cons ((elem1 * elem), acc1))
                                (One (head newList * elem))
                                (tail newList)) k) |> rev)
                    acc)
            firstIteration
            (tail newList1) |> deleteZeroes
         
let division fList sList = // деление
    if deleteZeroes sList = One 0
    then failwith "cannot divide"              
    else
        if greatest fList sList = false 
        then One 0
        elif equals fList sList = true
        then One 1
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
                fold (fun (acc, current) elem -> 
                            if greatest (concat current (One elem)) sList = false
                            then ((concat acc (One 0)),concat current (One elem))
                            else
                                let mutable j = 1
                                while head (subtract (concat current (One elem)) (multiply (One j) sList)) >= 0 do
                                    j <- j + 1
                                (concat acc (One (j - 1))), (subtract (concat current (One elem)) (multiply (One (j - 1)) sList)))
                        ((One (t - 1)), (subtract (choosePart fList 1 k) (multiply (One (t - 1)) sList)))
                        (choosePart fList (k + 1) (length fList)) |> fst
                
                
                
