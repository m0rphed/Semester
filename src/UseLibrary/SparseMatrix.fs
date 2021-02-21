module SparseMatrix

open AlgebraicStruct

open bMatrix

type Triple<'t> =
    val coordinates: Pair
    val data: 't
    new (x, y, data) = {coordinates = (if x < 0 || y < 0 then failwith "expected positive" else Pair (x * 1<Row>, y * 1<Col>)); data = data}

type SparseMatrix<'t> =
    val numOfRows: int
    val numOfCols: int
    val notEmptyData: list<Triple<'t>> 
    new (k, p, lsts) = {numOfRows = k; numOfCols = p; notEmptyData = lsts}

type QuadTree<'t when 't : equality> =
    | None 
    | Leaf of 't 
    | Node of QuadTree<'t> * QuadTree<'t> * QuadTree<'t> * QuadTree<'t>
    static member sum group x y =
        let neutral, operation =
            match group with
            | Monoid x -> x.neutral, x.sum
            | SemiRing x -> x.monoid.neutral, x.monoid.sum
        let rec _go x y = 
            match x, y with
            | Leaf a, Leaf b ->
                let current = operation a b 
                if neutral = current then None else Leaf current
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

    static member multiply group x y =
        let neutral, operation =
            match group with
            | Monoid x -> failwith "monoid cannot be in multiply"
            | SemiRing x -> x.monoid.neutral, x.mul
        let rec _go x y =
            match x, y with
            | Leaf t, Leaf k ->
                let current = operation t k 
                if neutral = current then None else Leaf current
            | None, _ -> None
            | _, None -> None
            | Node (q, q1, q2, q3), Node (qu, qu1, qu2, qu3) ->               
                let first =  QuadTree<'t>.sum group (_go q qu) (_go q1 qu2)
                let second = QuadTree<'t>.sum group (_go q qu1) (_go q1 qu3)
                let third = QuadTree<'t>.sum group (_go q2 qu) (_go q3 qu2)
                let fourth = QuadTree<'t>.sum group (_go q2 qu1) (_go q3 qu3)
                if first = None && second = None && third = None && fourth = None
                then None
                else Node (first, second, third, fourth)
            | _, _ -> failwith "cannot be in this case"
        _go x y

    static member multiplyScalar group (scalar: 't) x =
        let neutral, operation =
            match group with
            | Monoid x -> x.neutral, x.sum
            | SemiRing x -> x.monoid.neutral, x.mul
        if scalar = neutral
        then None
        else
            let rec _go x =
                match x with
                | Leaf t -> Leaf (operation scalar t)
                | None -> None
                | Node (q, q1, q2, q3) -> Node (_go q, _go q1, _go q2, _go q3)
            _go x

    static member tensorMultiply group x y =
        if y = None || x = None 
        then None
        else
            let rec _go x =
                match x with
                | Leaf t -> QuadTree<'t>.multiplyScalar group t y
                | None -> None
                | Node (q, q1, q2, q3) -> Node (_go q, _go q1, _go q2, _go q3)
            _go x

    static member toMatrix x size (neutral: 't) =
        let toSparse x neutral =
            let mutable counter = 0
            for i in 0 .. Array2D.length1 x - 1 do
                for j in 0 .. Array2D.length2 x - 1 do
                    if x.[i,j] <> neutral
                    then counter <- counter + 1
            let failIndex = Array2D.length1 x + 1
            let output = Array.create counter (Triple(failIndex, failIndex, neutral))
            counter <- 0
            for i in 0 .. Array2D.length1 x - 1 do
                   for j in 0 .. Array2D.length2 x - 1 do
                       if x.[i,j] <> neutral
                       then
                           output.[counter] <- Triple(i, j, x.[i,j])
                           counter <- counter + 1
            SparseMatrix(failIndex - 1, failIndex - 1, List.ofArray output)
    
        let outputMtx = Array2D.create size size neutral
        let rec _go x (point: Pair) size =
            match x, size with
            | Leaf t, _ -> outputMtx.[int point.x, int point.y] <- t
            | None, _ -> outputMtx.[0, 0] <- outputMtx.[0, 0]
            | Node (q1, q2, q3, q4), 2 ->
                let nwPnt = Pair((int point.x - size / 2) * 1<Row>, (int point.y - size / 2) * 1<Col>)
                let swPnt = Pair((int point.x) * 1<Row>, (int point.y - size / 2) * 1<Col>)
                let nePnt = Pair((int point.x - size / 2) * 1<Row>, (int point.y) * 1<Col>)
                let sePnt = Pair((int point.x) * 1<Row>, (int point.y) * 1<Col>)
                _go q1 nwPnt (size / 2); _go q2 nePnt (size / 2); _go q3 swPnt (size / 2); _go q4 sePnt (size / 2)
            | Node (q1, q2, q3, q4), _ ->
                let nwPnt = Pair((int point.x - size / 4) * 1<Row>, (int point.y - size / 4) * 1<Col>)
                let swPnt = Pair((int point.x + size / 4) * 1<Row>, (int point.y - size / 4) * 1<Col>)
                let nePnt = Pair((int point.x - size / 4) * 1<Row>, (int point.y + size / 4) * 1<Col>)
                let sePnt = Pair((int point.x + size / 4) * 1<Row>, (int point.y + size / 4) * 1<Col>)
                _go q1 nwPnt (size / 2); _go q2 nePnt (size / 2); _go q3 swPnt (size / 2); _go q4 sePnt (size / 2)
        _go x (Pair((size / 2) * 1<Row>, (size / 2) * 1<Col>)) size
        toSparse outputMtx neutral

let createTree (x: SparseMatrix<'t>) =
// fIter определяет 4 квадратных матрицы из одной, сравнивая все элементы с compPoint(которая является "центром" матрицы)
    let fIter (mtx: SparseMatrix<'t>) (compPoint: Pair) =
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
        (SparseMatrix(numOfColsRows, numOfColsRows, list), SparseMatrix(numOfColsRows, numOfColsRows, list1)),
        (SparseMatrix(numOfColsRows, numOfColsRows, list2), SparseMatrix(numOfColsRows, numOfColsRows, list3))
    let rec _go (point: Pair) (matrix: SparseMatrix<'t>) =
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

let private getNeutral group =
    match group with
    | Monoid x -> x.neutral
    | SemiRing x -> x.monoid.neutral

let sum group (x: SparseMatrix<'t>) (y: SparseMatrix<'t>) =
    if x.numOfCols <> y.numOfCols && y.numOfRows <> x.numOfRows
    then failwith "size not correct"
    else
        let first, second = createTree x, createTree y
        (QuadTree.toMatrix (QuadTree<'t>.sum group first second) x.numOfCols (getNeutral group))

let multiply group (x: SparseMatrix<'t>) (y: SparseMatrix<'t>) =
    if x.numOfCols <> y.numOfCols && y.numOfRows <> x.numOfRows
    then failwith "size not correct"
    else
        let first, second = createTree x, createTree y
        (QuadTree.toMatrix (QuadTree<'t>.multiply group first second) x.numOfCols (getNeutral group))

let multiplyScalar group scalar (y: SparseMatrix<'t>) =
    (QuadTree.toMatrix (QuadTree<'t>.multiplyScalar group scalar (createTree y)) y.numOfCols (getNeutral group))

let multiplyTensor group (x: SparseMatrix<'t>) (y: SparseMatrix<'t>) =
    if x.numOfCols <> y.numOfCols && y.numOfRows <> x.numOfRows
    then failwith "size not correct"
    else
        let first, second = createTree x, createTree y
        (QuadTree.toMatrix (QuadTree<'t>.tensorMultiply group first second) x.numOfCols (getNeutral group))
