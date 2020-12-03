module Tree
open Listik

type MyTree<'t> =
    | Leaf of 't
    | Node of 't * MyList<MyTree<'t>>

let avgTree x =
    let rec _go acc x =
        match x with
        | Leaf t -> acc + t  
        | Node (hd, tl) ->
            fold (fun acc1 elem -> acc1 + _go acc elem) hd tl   
    let rec _go1 acc x =
        match x with
        | Leaf t -> acc + 1
        | Node (hd, tl) ->
            fold (fun acc1 elem -> acc1 + _go1 acc elem) 1 tl
    (_go 0 x) / (_go1 0 x)

let maxInTree x =
    let rec _go acc x =
        match x with
        | Leaf t ->
            if acc > t
            then acc
            else t
        | Node (hd, tl) ->
             fold (fun acc1 elem -> _go (max acc1 acc) elem) hd tl
    _go System.Int32.MinValue x 

