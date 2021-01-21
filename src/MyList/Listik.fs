module Listik

type MyList<'t> =
    | One of 't
    | Cons of 't * MyList<'t>

type MyString = MyList<char>

let rec fold f acc x =
    match x with
    | One t -> f acc t  
    | Cons (hd, tl) -> fold f (f acc hd) tl
    
let length x =
    fold (fun acc elem -> acc +  1) 0 x

let toMyList x = 
    if List.length x < 1
    then failwith "use correct list"
    else
        let y = List.rev x
        List.fold (fun acc elem -> if x = [] then acc else Cons (elem, acc)) (One y.[0]) (y.Tail)

let  toDefoltList x =        
    List.rev (fold (fun acc elem -> elem :: acc) [] x)

let generator t =
    if t < 1
    then failwith "MyList cannot be created because input values uncorrect"
    else (toMyList (List.init t (fun _ -> System.Random().Next(0,10))))

let rec concat x y =
    match x with
    | One t -> Cons (t, y)
    | Cons (i, o) -> Cons (i, concat o y)

let sort x = 
    let rec _go x =
        match x with
        | One t -> x
        | Cons (head, Cons (head2, tl)) when head > head2 -> (Cons (head2, _go (Cons (head, tl))))
        | Cons (head, Cons (head2, tl)) -> (Cons (head, _go (Cons (head2, tl))))
        | Cons (head, One t) when head < t -> Cons (head, One t)
        | Cons (head, One t) -> Cons (t, One head)  
    let rec _go1 x k =
        match k with
        | k when (k = length x) -> x
        | _ -> _go1 (_go x) (k + 1)
    _go1 (_go x) 0
           
let rec iter f x =
    match x with
    | One t -> f t
    | Cons (i, o) ->
        f i
        iter f o

let rec map f x =
    match x with
    | One t -> One (f t)
    | Cons (i, o) -> Cons (f i, map f o)

let concatMyString (x: MyString) (y: MyString) =
    (concat x y): MyString 

let toMyString (str: string) =
    let k = [for i in str -> i]
    toMyList k:MyString

let toString (x: MyString) =
    fold (fun acc elem -> acc + string elem) "" x
  
let indexElem x i = // индекс i листа x 
    let mutable firstElem =
        match x with
        | One t ->  t
        | Cons (hd, tl) -> hd
    let mutable k = 1
    iter (fun elem -> if k = i then firstElem <- elem; k <- k + 1 else k <- k + 1) x
    firstElem

let tail x = // хитрый хвост листа, который для последнего элемента возвращает One 0  
    match x with
       | One t -> One 0
       | Cons (hd, tl) -> tl

let head x = // голова листа
    indexElem x 1
    
let rev x = // реверс
    if length x > 1
    then fold (fun acc elem -> Cons (elem, acc)) (One (head x)) (tail x)
    else x

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
