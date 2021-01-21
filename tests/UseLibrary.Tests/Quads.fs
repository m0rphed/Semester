module Quads

open QuadTree

open ExtendedMatrix

open Expecto

open BigAriphmetics

open Listik

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

let prin (x: Triple list) = List.iter (fun (elem: Triple) -> printfn "%A %A" (elem.x, elem.y) elem.data) x

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
[<Tests>]
let iDontKnowHowToNameIt =
    testList "check all operations"
        [
            testCase "tensor mult on matrix and on trees id"
            <| fun _ ->
                let x = ExtendedMatrix.generator 64 64
                let y = ExtendedMatrix.generator 64 64  
                Expect.equal (tensor (fst x) (fst y)) (tensorMultiply (create (snd x)) (create (snd y)) |> toMatrix) "needs to be equal"

            testCase "check dim"
            <| fun _ ->
                let x = ExtendedMatrix.generator 8 8
                Expect.equal (dim (create (snd x))) 8 "equals"

            testProperty "findData x point point1 equal someMatrix.[point, point1]"
            <| fun (k: int, index: int, index1: int) ->
                if k <> 0 && abs k < 7 && abs index <= pown 2 (abs k) - 1 && abs index1 <= pown 2 (abs k) - 1 // чтоб индексы не вылетели
                then
                    let x = ExtendedMatrix.generator (pown 2 (abs k)) (pown 2 (abs k))                    
                    Expect.equal (snd (findData (create (snd x)) ((abs index),(abs index1)))) (fst x).[abs index, abs index1] "id"

            testProperty "check create from Matrix to Tree"
            <| fun (k: int) ->
                if k <> 0 && abs k < 7
                then
                    let x = ExtendedMatrix.generator (pown 2 (abs k)) (pown 2 (abs k))
                    Expect.equal (create (snd x) |> toMatrix) (fst x) "id"
            
            testCase "tensor mult"
            <| fun _ ->
                let x = Array2D.create 2 2 1
                let y = Array2D.create 2 2 2
                let vector = Array2D.create 1 1 2
                Expect.equal y (tensor vector x) "equal" // 1 1 1 1 * 2 -> 2 2 2 2 только в матрице 2 * 2 

            testProperty "tensor mult on matrix and on trees id №2"
            <| fun (k: int) ->
                if k <> 0 && abs k < 7 // дальше выдает эксепшн на размер матрицы
                then
                    let x = ExtendedMatrix.generator (pown 2 (abs k)) (pown 2 (abs k))
                    let y = ExtendedMatrix.generator (pown 2 (abs k)) (pown 2 (abs k))   
                    Expect.equal (tensor (fst x) (fst y)) (tensorMultiply (create (snd x)) (create (snd y)) |> toMatrix) "needs to be equal"

            testProperty "standart mult matrix and on trees id"
            <| fun (k: int) ->
                if k <> 0 && abs k < 7
                then
                    let x = ExtendedMatrix.generator (pown 2 (abs k)) (pown 2 (abs k))
                    let y = ExtendedMatrix.generator (pown 2 (abs k)) (pown 2 (abs k))
                    Expect.equal (m (fst x) (fst y)) (QuadTree.multiply (create (snd x)) (create (snd y)) |> toMatrix) "needs to be equal"

            testProperty "standart mult matrix by scalar and on trees id"
            <| fun (k: int, scalar: int) ->
                if k <> 0 && abs k < 7
                then
                    let x = ExtendedMatrix.generator (pown 2 (abs k)) (pown 2 (abs k))
                    Expect.equal (multiplyByScalar scalar (fst x)) (multiplyConst 0 0 scalar (create (snd x)) |> toMatrix) "id"

            testProperty "standart sum matrix and on trees id"
                      <| fun (k: int) ->
                          if k <> 0 && abs k < 7
                          then
                              let x = ExtendedMatrix.generator (pown 2 (abs k)) (pown 2 (abs k))
                              let y = ExtendedMatrix.generator (pown 2 (abs k)) (pown 2 (abs k))
                              Expect.equal (sumMtx (fst x) (fst y)) (QuadTree.sum (create (snd x)) (create (snd y)) |> toMatrix) "needs to be equal"







        ]
