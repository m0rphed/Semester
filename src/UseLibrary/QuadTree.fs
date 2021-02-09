module QuadTree

open bMatrix

open ExtendedMatrix

// int хранит длину строки и столбца деревьев
type QuadTree<'t> =
    | None
    | Leaf of 't
    | Node of  QuadTree<'t> * QuadTree<'t> * QuadTree<'t> * QuadTree<'t>

// создает дерево из расширенной матрицы
let create (x: ExtendedMatrix) =
    // fIter определяет 4 квадратных матрицы из одной, сравнивая все элементы с compPoint(которая является "центром" матрицы)
    let fIter (mtx: ExtendedMatrix) (compPoint: Pair) =
        let list, list1, list2, list3 =
            List.fold
                (fun (nw, ne, sw, se) (elem: Triple) ->
                    if int elem.x > int compPoint.x - 1   
                    then
                        if int elem.y > int compPoint.y - 1  
                        then (nw, ne, sw, List.append se [elem])
                        else (nw, ne, List.append sw [elem], se)
                    elif int elem.y > int compPoint.y - 1 
                    then (nw, List.append ne [elem], sw, se)
                    else (List.append nw [elem], ne, sw, se))
                ([],[],[],[])
                mtx.notEmptyData
        let numOfColsRows = mtx.numOfCols / 2
        (ExtendedMatrix(numOfColsRows, numOfColsRows, list), ExtendedMatrix(numOfColsRows, numOfColsRows, list1)),
        (ExtendedMatrix(numOfColsRows, numOfColsRows, list2), ExtendedMatrix(numOfColsRows, numOfColsRows, list3))
    let rec _go (point: Pair) (matrix: ExtendedMatrix) =
        match matrix.numOfCols, matrix.notEmptyData with
        | 1, _ ->
            if matrix.notEmptyData = []
            then None
            else Leaf (int matrix.notEmptyData.[0].data)
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

let sum x y =
    let rec _go x y = 
        match x, y with
        | Leaf k, Leaf t -> Leaf (k + t)
        | None, k -> k
        | k, None -> k
        | Node (tl, tl1, tl2, tl3), Node (tail, tail1, tail2, tail3) ->
            Node (_go tl tail, _go tl1 tail1, _go tl2 tail2, _go tl3 tail3)
        | _, _ -> failwith "cannot sum trees with different dimensions"
    _go x y

let multiply x y =
    let rec _go x y =
        match x, y with
        | Leaf t, Leaf k -> Leaf (t * k)
        | None, _ -> None
        | _, None -> None
        | Node (q, q1, q2, q3), Node (qu, qu1, qu2, qu3) ->
            // тут на тестах some troubles, приходится честно проверять что если Node (None, None, None, None)
            // то это просто None
            let first =  sum (_go q qu) (_go q1 qu2)
            let second = sum (_go q qu1) (_go q1 qu3)
            let third = sum (_go q2 qu) (_go q3 qu2)
            let fourth = sum (_go q2 qu1) (_go q3 qu3)
            if first = second && second = third && third = fourth && fourth = None
            then None
            else Node (first, second, third, fourth)
        | _, _ -> failwith "cannot be in this case"
    _go x y 

let multiplyScalar scalar x =
    if scalar = 0
    then None
    else 
        let rec _go x =
            match x with
            | Leaf t -> Leaf (t * scalar)
            | None -> None
            | Node (q, q1, q2, q3) -> Node (_go q, _go q1, _go q2, _go q3)
        _go x

let tensorMultiply x y =
    if y = None
    then None
    else
        let rec _go x =
            match x with
            | Leaf t -> multiplyScalar t y
            | None -> None
            | Node (q, q1, q2, q3) -> Node (_go q, _go q1, _go q2, _go q3)
        _go x
