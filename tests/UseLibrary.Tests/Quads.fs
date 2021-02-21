module Quads

open AlgebraicStruct

open SparseMatrix

open bMatrix

open test

open Expecto

// генерирует кортеж из int[,] и ее расширенной матрицы
let generator k i =
    let reconstruct, reconstruct1 = generateRandomBoolAndDefoltMtx k i
    let x = List.map (fun (elem: Pair) ->
        Triple (int elem.x, int elem.y, (System.Random().Next()))) reconstruct1.notEmptyData
    for j in x do reconstruct.[int j.coordinates.x, int j.coordinates.y] <- j.data
    reconstruct, SparseMatrix(reconstruct1.numOfRows, reconstruct1.numOfCols, x)

let createEM (x: int[,]) =
    let mutable counter = 0  
    for i in 0 .. Array2D.length1 x - 1 do
        for j in 0 .. Array2D.length2 x - 1 do
            if x.[i,j] = 0 then counter <- counter + 1
    let arrOfData = Array.create (Array2D.length2 x * Array2D.length1 x - counter) (Triple (1, 1, 0))
    counter <- 0 
    for i in 0 .. Array2D.length1 x - 1 do
        for j in 0 .. Array2D.length2 x - 1 do
            if x.[i,j] <> 0 then 
                arrOfData.[counter] <- Triple (i, j, x.[i,j])
                counter <- counter + 1
    SparseMatrix(Array2D.length1 x, Array2D.length2 x, List.ofArray arrOfData)

// создает ExtendedMatrix на основе матрицы, в которой если value <> 0 существуют все элементы
let generatorOneValue k i value =
    let mutable counter = 0
    let mtx = Array2D.create k i value
    let arr = Array.create (k * i) (Triple(-1, -1, 0))
    for i in 0 .. Array2D.length1 mtx - 1 do
        for j in 0 .. Array2D.length2 mtx - 1 do
            arr.[counter] <- Triple (i, j, value)
            counter <- counter + 1
    SparseMatrix(k, i, List.ofArray arr)


let sumMtx o (t: int[,]) =
    for i in 0 .. Array2D.length1 o - 1 do
        for j in 0 .. Array2D.length2 o - 1 do
            o.[i,j] <- o.[i,j] + t.[i,j]
    o

let m (o: int[,]) (t: int[,]) =
    let first = Array2D.length2 o
    let second = Array2D.length1 o 
    let third = Array2D.length1 t 
    let fourth = Array2D.length2 t
    if first = third
    then
        let mtx = Array2D.zeroCreate second fourth
        for i in 0 .. second - 1 do 
            for j in 0 .. fourth - 1 do 
                for k in 0 .. first - 1 do 
                    mtx.[i,j] <- mtx.[i,j] + (t.[k,j] * o.[i,k])
        mtx      
    else failwith "Cannot multiply this"

let multiplyByScalar alpha x =
    let y = Array2D.copy x 
    for i in 0 .. Array2D.length1 x - 1 do
        for j in 0 .. Array2D.length2 x - 1 do
            y.[i,j] <- alpha * y.[i,j]
    y

let tensor (o: int[,]) (t: int[,]) =
    let mutable count1, count2 = 0, 0
    let output = Array2D.create (Array2D.length1 o * Array2D.length2 t) (Array2D.length1 o * Array2D.length2 t) 1
    for i in 0 .. Array2D.length1 o - 1 do
        for j in 0 .. Array2D.length2 o - 1 do
            count1 <- i * (Array2D.length1 t) 
            count2 <- j * (Array2D.length2 t)
            Array2D.blit (multiplyByScalar o.[i,j] t) 0 0 output count1 count2 (Array2D.length1 t) (Array2D.length2 t)
    output
// для теств подойдет обычное полукольцо с сложением и умножением интовым
let y = new Monoid<int>((+), 0)
let x = new SemiRing<int>(y, (*))
let group = SemiRing x

[<Tests>]
let treesOperations =
    testList "check all operations"
        [
            testProperty "tomatrix test"
            <| fun (k: int) ->
                if k > 0 && abs k < 6
                then
                    let x = generator (pown 2 (abs k)) (pown 2 (abs k))
                    let y = (createTree (QuadTree<_>.toMatrix (createTree (snd x)) (snd x).numOfCols 0))
                    let z = createTree (snd x)
                    Expect.equal y z "needs to be equal"

            testProperty "tensor mult on matrix and on trees id №2"
            <| fun (k: int) ->
                if k <> 0 && abs k < 4
                then
                    let x = generator (pown 2 (abs k)) (pown 2 (abs k))
                    let y = generator (pown 2 (abs k)) (pown 2 (abs k))   
                    Expect.equal (createTree (createEM (tensor (fst x) (fst y)))) (QuadTree.tensorMultiply group (createTree (snd x)) (createTree (snd y))) "needs to be equal"

            testProperty "standart mult matrix and on trees id"
            <| fun (k: int) ->
                if k <> 0 && abs k < 7
                then
                    let x = generator (pown 2 (abs k)) (pown 2 (abs k))
                    let y = generator (pown 2 (abs k)) (pown 2 (abs k))
                    Expect.equal (createTree (createEM (m (fst x) (fst y)))) (QuadTree<_>.multiply group (createTree (snd x)) (createTree (snd y))) "needs to be equal"

            testProperty "standart mult matrix by scalar and on trees id"
            <| fun (k: int, scalar: int) ->
                if k <> 0 && abs k < 7
                then
                    let x = generator (pown 2 (abs k)) (pown 2 (abs k))
                    Expect.equal (createTree (createEM (multiplyByScalar scalar (fst x)))) (QuadTree.multiplyScalar group scalar (createTree (snd x))) "id"

            testProperty "standart sum matrix and on trees id"
            <| fun (k: int) ->
                if k <> 0 && abs k < 7
                then
                    let x = generator (pown 2 (abs k)) (pown 2 (abs k))
                    let y = generator (pown 2 (abs k)) (pown 2 (abs k))
                    Expect.equal (createTree (createEM (sumMtx (fst x) (fst y)))) (QuadTree<_>.sum group (createTree (snd x)) (createTree (snd y))) "needs to be equal"
        ]
