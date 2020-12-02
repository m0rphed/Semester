module Listik

type MyList<'t> =
    | One of 't
    | Cons of 't * MyList<'t>

type MyString = MyList<char>

type SomeMeasures =
    | Int of int
    | Char of char
    | Float of float
    | Double of double
        
type MyTree<'t> =
    | Leaf of SomeMeasures
    | Node of SomeMeasures * MyList<MyTree<'t>>

let length x =
    let rec _go acc x =
        match x with
        | One y -> acc + 1
        | Cons (hd, tl) -> _go (acc + 1) tl
    _go 0 x

let generatorMyList t =
    if t < 1
    then failwith "MyList cannot be created because input values uncorrect"
    else
    let rec _go acc x =
        match x with
        | x when (length x) = t -> x
        | x -> _go (acc - 1) (Cons (System.Random().Next(),x))
    _go t (One (System.Random().Next()))

let concatMyList x y =
    if length x < 1 || length y < 1
    then failwith "use correct lists"
    else
        let rec concat x y =
            match x with
            | One t -> Cons (t, y)
            | Cons (i, o) -> Cons (i, concat o y)
        concat x y

let sortForMyList x =
    if length x < 1
    then failwith "use correct list"
    else
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
        
let iterMyList f x =
    if length x < 1
    then failwith "use correct list"
    else
        let rec _go x =
            match x with
            | One t -> f t
            | Cons (i, o) ->
                f i
                _go o
        _go x  

let mapMyList f x =
    if length x < 1
    then failwith "use correct list"
    else
        let rec _go x =
            match x with
            | One t -> One (f t)
            | Cons (i, o) -> Cons (f i, _go o)
        _go x

let fromStandartToMyList x =
    if List.length x < 1
    then failwith "use correct list"
    else
        let y = List.rev x
        let rec _go acc x =
            match x with
            | [] -> acc
            | hd :: tl -> _go (Cons (hd, acc)) tl
        _go (One y.[0]) (y.Tail)

let fromMyListtoStandart x =
    if length x < 1
    then failwith "use correct list"
    else
        let rec _go acc x =
            match x with
            | One t -> t :: acc
            | Cons (hd, tl) -> _go (hd :: acc) tl
        List.rev (_go [] x)

let concatMyString x y =
    if length x < 1 || length y < 1
    then failwith "use correct MyString"
    else
        let rec concat (x: MyString) (y: MyString): MyString =
            match x with
            | One t -> Cons (t, y)
            | Cons (i, o) -> Cons (i, concat o y)
        concat x y

let fromStringToMyString (str: string) =
    let k = [for i in str -> i]
    fromStandartToMyList k:MyString

let fromMyStringToString x =
    if length x < 1
    then failwith "use correct MyList"
    else
        let rec _go (acc: string) (x: MyString) =
            match x with
            | One t -> acc + string t 
            | Cons (hd, tl) -> _go (acc + string hd) tl
        (_go "" x)
   
let avgMyTree x =
    let rec _go acc x =
        match x with
        | Leaf (SomeMeasures.Int t) -> acc + t 
        | Node (SomeMeasures.Int hd, tl) ->
            let rec _go1 count tl = 
                match tl with
                | One t -> _go acc t + count
                | Cons (t, k) -> _go1 (count + _go acc t)  k
            _go1 hd tl
    let rec _go2 acc x =
        match x with
        | Leaf (SomeMeasures.Int t) -> acc + 1
        | Node (SomeMeasures.Int hd, tl) ->
            let rec _go1 count tl = 
                match tl with
                | One t -> _go2 acc t + count
                | Cons (t, k) -> _go1 (count + _go2 acc t)  k
            _go1 1 tl
    (_go 0 x) / (_go2 0 x)

let maxInMyTree x =
    let rec _go acc x =
        match x with
        | Leaf (SomeMeasures.Int t) ->
            if acc > t
            then acc
            else t
        | Node (SomeMeasures.Int hd, tl) ->
            let rec _go1 el tl = 
                match tl with
                | One t ->
                    if acc > el
                    then _go acc t
                    else _go el t 
                | Cons (t, k) ->
                    if acc > el
                    then _go1 (_go acc t) k
                    else _go1 (_go el t) k  
            _go1 hd tl
    _go 0 x 
