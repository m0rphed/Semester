module BigAriphmetics

open System

open Listik

type sign =
    | Positive 
    | Negative 

let private detect x = if x = 1 then Positive elif x = -1 then Negative else failwith "wrong sign"

type BigInt =
    val signOfNumber: sign
    val digits: MyList<int>
    new ((k: int), p) = {signOfNumber = (detect k); digits = p}
    member this.sign = if this.signOfNumber = Positive then 1 else -1

let initPosInt x = BigInt (1, x) 

let createBigInt length =
    let randomSign = System.Random().Next(0,2) 
    let out = generator length
    if randomSign = 0
    then BigInt (-1, out)
    else BigInt (1, out)

// добавляет 0 впереди числа
let rec private addZeroBeginning acc k = if k = 1 then acc else addZeroBeginning (Cons (0, acc)) (k - 1)

// добавляет 0 в конце числа 
let private addZeroEnd x k = rev (addZeroBeginning (rev x) k)

// возвращает true если x >= y иначе false для листов (проще говоря сравнивает числа по модулю)
let compareNumbers x y =
    if length x = length y
    then
        let rec _go x y =
            match x, y with
            | One t, One k -> t >= k
            | Cons (hd, tl), Cons (hd1, tl1) -> if hd = hd1 then _go tl tl1 else hd > hd1
            | _, _ -> failwith "cant be in this case"
        _go x y 
    else length x > length y 

let private deleteZeroes x = // удаляет нули незначащие
    let rec _go x =
        match x with
        | One k -> One k 
        | Cons (0, tl) -> _go tl
        | Cons (hd, tl) -> Cons (hd, tl)
    _go x

let private equalizeLength (x: BigInt) (y: BigInt) =
    if length x.digits > length y.digits
    then x, BigInt (y.sign, addZeroBeginning y.digits (length x.digits - length y.digits + 1))
    elif length x.digits < length y.digits
    then BigInt (x.sign, addZeroBeginning x.digits (length y.digits - length x.digits + 1)), y
    else x, y

let private choosePart x k i = // выбирает кусок от k до i в листе
    if k = i
    then One (indexElem x k)
    else
        let rec _go acc k =
            if k = i
            then Cons (indexElem x k, acc)
            else _go (Cons (indexElem x k, acc)) (k + 1)
        rev (_go (One (indexElem x k)) (k + 1))

let transferDigits x =
    let output = 
        fold
            (fun (remainder, acc) elem ->
                let current = elem + remainder
                if current >= 0
                then (current / 10, Cons (current % 10, acc))
                else (-1, Cons (10 + (current % 10), acc)))
            (0, One 0)
            (rev x)
    if fst output <> 0
    then Cons (fst output, rev (tailOrZero (rev (snd output)))) |> deleteZeroes 
    else rev (tailOrZero (rev (snd output))) |> deleteZeroes
 
let sum (x: BigInt) (y: BigInt) =
    // если знаки равны просто складываем, если нет то находим большее по модулю и однозначно знаем откуда вычитать
    let fList, sList = equalizeLength x y 
    if fList.sign = sList.sign
    then BigInt (fList.sign, transferDigits (map2 (+) fList.digits sList.digits))
    elif compareNumbers fList.digits sList.digits  
    then BigInt (fList.sign, transferDigits (map2 (-) fList.digits sList.digits))
    else BigInt (sList.sign, transferDigits (map2 (-) sList.digits fList.digits))

let sub (x: BigInt) (y: BigInt) = sum x (BigInt (y.sign * -1, y.digits)) 

let multiply (x: BigInt) (y: BigInt) =
    // я придумал обходилку эксепшона с вызовом хвоста у единичного листа, везде юзаю special tail
    // и добавляю к изначальным листам по 1 нулю, и тогда, когда попадается лист длины 1, все работает исправно
    let fList, sList = addZeroBeginning x.digits 2, addZeroEnd (rev y.digits) 2
    let fIter = fold (fun acc elem -> Cons ((elem * head sList), acc)) (One (head fList * head sList)) (tailOrZero fList) |> rev
    let mutable k = 1
    let output = 
        fold
            (fun acc elem ->               
                k <- k + 1
                sum
                    (initPosInt ((addZeroBeginning
                                   (fold
                                       (fun acc1 elem1 -> Cons ((elem1 * elem), acc1))
                                       (One (head fList * elem))
                                       (tailOrZero fList)) k) |> rev))
                    acc)
            (initPosInt fIter)
            (tailOrZero sList)
    BigInt (x.sign * y.sign, deleteZeroes output.digits)

// умножалка по модулю
let private absMul x y = (multiply (BigInt (1, x)) (BigInt (1, y))).digits

let division (x: BigInt) (y: BigInt) =
    let divident, divisor = (deleteZeroes x.digits), (deleteZeroes y.digits)
    if divisor = One 0
    then failwith "cannot divide"              
    elif divisor = divident
    then BigInt (x.sign * y.sign, One 1)
    elif compareNumbers divisor divident
    then initPosInt (One 0)
    else
        let returnRemainder divident0 counter =
            let mutable counter1 = counter
            while (sub divident0 (initPosInt (absMul (One counter1) divisor))).sign = 1 do counter1 <- counter1 + 1
            (One (counter1 - 1)), (sub divident0 (initPosInt (absMul (One (counter1 - 1)) divisor))).digits
        let mutable k = 1
        while compareNumbers divisor (choosePart divident 1 k) && divisor <> (choosePart divisor 1 k) do k <- k + 1 // первая итерация для фолда
        let fDividentValue, fRemainder = returnRemainder (initPosInt (choosePart divident 1 k)) 0
        if k + 1 > length divident then k <- k - 1
        if length divident = 1 || (length divident = length divisor || length divident = length divisor + 1) // обработка случая когда при делении получается число длины 1
                && compareNumbers divisor (sub (initPosInt divident) (initPosInt (absMul fDividentValue divisor))).digits      
        then BigInt (x.sign * y.sign, fDividentValue)
        else
            BigInt (x.sign * y.sign,
                fold
                    (fun (acc, current) elem -> 
                        if compareNumbers divisor (concat current (One elem)) 
                        then ((concat acc (One 0)), concat current (One elem))
                        else
                            let dividentValue, remainder = returnRemainder (initPosInt (concat current (One elem))) 0
                            (concat acc dividentValue), remainder)
                    (fDividentValue, fRemainder)
                    (choosePart divident (k + 1) (length divident)) |> fst)   
