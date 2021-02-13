module QuadTree

open System.Collections.Generic

open bMatrix

open Group

open ExtendedMatrix

type QuadTree<'t when 't : equality> =
    | None 
    | Leaf of 't 
    | Node of QuadTree<'t> * QuadTree<'t> * QuadTree<'t> * QuadTree<'t>

let create (x: ExtendedMatrix<'t>) =
// fIter определяет 4 квадратных матрицы из одной, сравнивая все элементы с compPoint(которая является "центром" матрицы)
    let fIter (mtx: ExtendedMatrix<'t>) (compPoint: Pair) =
        let list, list1, list2, list3 =
            List.fold
                (fun (nw, ne, sw, se) (elem: Triple<'t>) ->
                    if int elem.coordinates.x > int compPoint.x - 1   
                    then
                        if int elem.coordinates.y > int compPoint.y - 1  
                        then (nw, ne, sw, List.append se [elem])
                        else (nw, ne, List.append sw [elem], se)
                    elif int elem.coordinates.y > int compPoint.y - 1 
                    then (nw, List.append ne [elem], sw, se)
                    else (List.append nw [elem], ne, sw, se))
                ([],[],[],[])
                mtx.notEmptyData
        let numOfColsRows = mtx.numOfCols / 2
        (ExtendedMatrix(numOfColsRows, numOfColsRows, list), ExtendedMatrix(numOfColsRows, numOfColsRows, list1)),
        (ExtendedMatrix(numOfColsRows, numOfColsRows, list2), ExtendedMatrix(numOfColsRows, numOfColsRows, list3))
    let rec _go (point: Pair) (matrix: ExtendedMatrix<'t>) =
        match matrix.numOfCols, matrix.notEmptyData with
        | 1, _ ->
            if matrix.notEmptyData = []
            then None
            else Leaf matrix.notEmptyData.[0].data
        | _, k when k.Length <> 0 ->
            let nwPnt = Pair((int point.x - matrix.numOfCols / 4) * 1<Row>, (int point.y - matrix.numOfCols / 4) * 1<Col>)
            let swPnt = Pair((int point.x + matrix.numOfCols / 4) * 1<Row>, (int point.y - matrix.numOfCols / 4) * 1<Col>)
            let nePnt = Pair((int point.x - matrix.numOfCols / 4) * 1<Row>, (int point.y + matrix.numOfCols / 4) * 1<Col>)
            let sePnt = Pair((int point.x + matrix.numOfCols / 4) * 1<Row>, (int point.y + matrix.numOfCols / 4) * 1<Col>)
            Node               
                (_go nwPnt (fst (fst (fIter matrix point))),
                _go nePnt (snd (fst (fIter matrix point))),
                _go swPnt (fst (snd (fIter matrix point))),
                _go sePnt (snd (snd (fIter matrix point))))
        | _, _ -> None           
    _go (Pair((x.numOfCols / 2) * 1<Row>, (x.numOfCols / 2) * 1<Col>)) x

let sum group x y =
    let neutral, operation =
        match group with
        | Monoid x -> x.neutral, x.binaryOp
        | SemiRing x -> x.monoid.neutral, x.monoid.binaryOp
    let rec _go x y = 
        match x, y with
        | Leaf a, Leaf b -> if neutral = operation a b then None else Leaf (operation a b)
        | None, k -> k
        | k, None -> k
        | Node (tl, tl1, tl2, tl3), Node (tail, tail1, tail2, tail3) ->
            let first = _go tl tail
            let second = _go tl1 tail1
            let third = _go tl2 tail2
            let fourth = _go tl3 tail3
            if first = None && second = None && third = None && fourth = None
            then None
            else Node (first, second, third, fourth)
        | _, _ -> failwith "cannot sum trees with different dimensions"
    _go x y

let multiply group x y =
    let neutral, operation =
        match group with
        | Monoid x -> failwith "monoid cannot be in multiply"
        | SemiRing x -> x.monoid.neutral, x.multiply
    let rec _go x y =
        match x, y with
        | Leaf t, Leaf k -> if neutral = operation t k then None else Leaf (operation t k)
        | None, _ -> None
        | _, None -> None
        | Node (q, q1, q2, q3), Node (qu, qu1, qu2, qu3) ->
            // тут на тестах some troubles, приходится честно проверять что если Node (None, None, None, None)
            // то это просто None
            let first =  sum group (_go q qu) (_go q1 qu2)
            let second = sum group (_go q qu1) (_go q1 qu3)
            let third = sum group (_go q2 qu) (_go q3 qu2)
            let fourth = sum group (_go q2 qu1) (_go q3 qu3)
            if first = None && second = None && third = None && fourth = None
            then None
            else Node (first, second, third, fourth)
        | _, _ -> failwith "cannot be in this case"
    _go x y

let multiplyScalar group (scalar: 't) x =
    let neutral, operation =
        match group with
        | Monoid x -> x.neutral, x.binaryOp
        | SemiRing x -> x.monoid.neutral, x.multiply
    if scalar = neutral
    then None
    else
        let rec _go x =
            match x with
            | Leaf t -> Leaf (operation scalar t)
            | None -> None
            | Node (q, q1, q2, q3) -> Node (_go q, _go q1, _go q2, _go q3)
        _go x

let tensorMultiply group x y =
    if y = None || x = None 
    then None
    else
        let rec _go x =
            match x with
            | Leaf t -> multiplyScalar group t y
            | None -> None
            | Node (q, q1, q2, q3) -> Node (_go q, _go q1, _go q2, _go q3)
        _go x
