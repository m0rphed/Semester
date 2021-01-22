module QuadTree

open bMatrix

open ExtendedMatrix

// int хранит длину строки и столбца деревьев
type QuadTree<'t> =
    | None
    | Leaf of 't
    | Node of int * QuadTree<'t> * QuadTree<'t> * QuadTree<'t> * QuadTree<'t>

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
        match matrix.numOfCols with
        | 1 ->
            if matrix.notEmptyData = []
            then None
            else Leaf ((int matrix.notEmptyData.[0].x, int matrix.notEmptyData.[0].y), int matrix.notEmptyData.[0].data)
        | _ ->
            let nwPnt = Pair((int point.x - matrix.numOfCols / 4) * 1<Row>, (int point.y - matrix.numOfCols / 4) * 1<Col>)
            let swPnt = Pair((int point.x + matrix.numOfCols / 4) * 1<Row>, (int point.y - matrix.numOfCols / 4) * 1<Col>)
            let nePnt = Pair((int point.x - matrix.numOfCols / 4) * 1<Row>, (int point.y + matrix.numOfCols / 4) * 1<Col>)
            let sePnt = Pair((int point.x + matrix.numOfCols / 4) * 1<Row>, (int point.y + matrix.numOfCols / 4) * 1<Col>)
            Node
                (matrix.numOfCols / 2,
                _go nwPnt (fst (fst (fIter matrix point))),
                _go nePnt (snd (fst (fIter matrix point))),
                _go swPnt (fst (snd (fIter matrix point))),
                _go sePnt (snd (snd (fIter matrix point))))
    _go (Pair((x.numOfCols / 2) * 1<Row>, (x.numOfCols / 2) * 1<Col>)) x

let sum x y =
    let rec _go x y = 
        match x, y with
        | None, None -> None
        | None, Leaf _ -> y
        | Leaf _, None -> x 
        | Leaf ((p1, p2), data), Leaf ((p3, p4), data0) when p1 = p3 && p2 = p4 -> Leaf ((p1, p2), data + data0)
        | None, Node _ -> y
        | Node _, None -> x
        | Node (smth, tl, tl1, tl2, tl3), Node (smth1, tail, tail1, tail2, tail3) ->
            Node (smth, _go tl tail, _go tl1 tail1, _go tl2 tail2, _go tl3 tail3)
        | _, _ -> failwith "cannot sum trees with different dimensions"
    _go x y

// определяет длину стоки и столбца дерева, тем самым мы знаем размерность 
let dim x =
    match x with
    | Leaf _ -> 1
    | None -> 1
    | Node (smth, _, _, _, _) -> smth * 2 

let mySign x = if x > 0 then 1 else -1

// находит элемент по заданным (i,k) 
let findData x (i, k) =
    let rec _go point x =
        match x with
        | Leaf ((p1, p2), data) -> ((p1, p2), data)
        | None -> ((i, k), 0)
        | Node (smth, q, q1, q2, q3) ->
            let first = mySign (i - fst point + 1)
            let second = mySign (k - snd point + 1)
            let definedPoint = ((fst point + (smth / 2) * first), (snd point + (smth / 2) * second))
            match first, second with
            | 1, 1 ->  _go definedPoint q3
            | -1, -1 -> _go definedPoint q
            | 1, -1 -> _go definedPoint q2
            | -1, 1 -> _go definedPoint q1
            | _, _ -> failwith "cannot be in this case"            
    _go (dim x / 2, dim x / 2) x

let multiply x y =
    let outputTree = create (generatorOneValue (dim x) (dim x) 1)
    let rec _go tree =
        match tree with       
        | Leaf ((p1, p2), data) ->
            let mutable res = 0         
            for i in 0 .. (dim x) - 1 do
                res <- res + snd (findData x (p1, i)) * snd (findData y (i, p2))
            if res = 0  
            then None
            else Leaf ((p1, p2), res)
        | Node (smth, q, q1, q2, q3) -> Node (smth, _go q, _go q1, _go q2, _go q3)
        | _ -> failwith "not worked in this"
    _go outputTree

// вспомогательная функция для тензорного умножения, домножает всю дату на скаляр 
// вдобавок однозначно определяет индекс элемента в будущем дереве
let multiplyConst sigma1 sigma2 alpha x =
    let rec _go x =
        match x with
        | Leaf ((p1, p2), data) -> Leaf ((p1 + sigma1, p2 + sigma2), data * alpha)
        | None -> None
        | Node (smth, q, q1, q2, q3) -> Node (smth, _go q, _go q1, _go q2, _go q3)
    _go x

let tensorMultiply x y =
    let rec _go x =
        match x with
        | Leaf ((p1, p2), data) -> multiplyConst (p1 * (dim y)) (p2 * (dim y)) data y
        | None -> None
        | Node (smth, q, q1, q2, q3) -> Node (smth * (dim y), _go q, _go q1, _go q2, _go q3)
    _go x

// превращает дерево в матрицу
let toMatrix x =
    let outputMtx = Array2D.zeroCreate (dim x) (dim x)
    let rec _go x =
        match x with
        | Leaf ((p1, p2), data) -> outputMtx.[p1, p2] <- data
        | None -> outputMtx.[0, 0] <- outputMtx.[0, 0] 
        | Node (_, q1, q2, q3, q4) -> _go q1; _go q2; _go q3; _go q4
    _go x
    outputMtx