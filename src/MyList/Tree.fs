module Tree
open Listik

type MyTree<'t> =
    | Leaf of 't
    | Node of 't * MyList<MyTree<'t>>

let avgMyTree x =
    let rec _go acc x =
        match x with
        | Leaf t -> acc + t 
        | Node (hd, tl) ->
            let rec _go1 count tl = 
                match tl with
                | One t -> _go acc t + count
                | Cons (t, k) -> _go1 (count + _go acc t)  k
            _go1 hd tl
    let rec _go2 acc x =
        match x with
        | Leaf t -> acc + 1
        | Node (hd, tl) ->
            let rec _go1 count tl = 
                match tl with
                | One t -> _go2 acc t + count
                | Cons (t, k) -> _go1 (count + _go2 acc t)  k
            _go1 1 tl
    (_go 0 x) / (_go2 0 x)

let maxInTree x =
    let rec _go acc x =
        match x with
        | Leaf t ->
            if acc > t
            then acc
            else t
        | Node (hd, tl) ->
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

