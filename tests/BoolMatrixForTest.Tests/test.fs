module test
open Expecto
open System
open bMatrix

let multiply (o: int[,]) (t: int[,]) =       
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
        for i in 0 .. second - 1 do
            for j in 0 .. fourth - 1 do
                if abs mtx.[i,j] > 0
                then mtx.[i,j] <- 1
        mtx      
    else failwith "Cannot multiply this"

let generateRandomBoolMatrix m n = 
    let rnd = System.Random()
    let arr = Array2D.init m n (fun x y -> rnd.Next(0,2))
    let firstOutput = arr
    let createBoolMatrixFromStandart (x: int[,]) =
        if x.Length = 0
        then Matrix (0, 0, [])
        elif x.Length < 0
        then failwith "Cannot create"
        else
            let mutable k = 0
            for i in 0 .. (Array2D.length1 x) - 1 do
                for j in 0 .. (Array2D.length2 x) - 1 do
                    if abs x.[i,j] > 0
                    then k <- k + 1
            let arr = Array.init k (fun _ -> Pair (-1<Row>, -1<Col>))
            k <- 0
            for i in 0 .. (Array2D.length1 x) - 1  do
                for j in 0 .. (Array2D.length2 x) - 1 do
                    if abs x.[i,j] > 0
                    then
                        arr.[k] <- Pair (i * 1<Row>, j * 1<Col>)
                        k <- k + 1
            Matrix ((Array2D.length1 x), (Array2D.length2 x), List.ofArray arr)
    (firstOutput, createBoolMatrixFromStandart arr)

[<Tests>]
let checkEquality =
    testList "test eqaulity"
        [
            testProperty "test"
            <| fun (k: int, n: int, t: int) ->
                if k <> 0 && n <> 0 && t <> 0
                then
                    let firstGenerate = generateRandomBoolMatrix (abs n) (abs k)
                    let secondGenerate = generateRandomBoolMatrix (abs k) (abs t)
                    let first = returnMatrix (multiplyBool (snd firstGenerate) (snd secondGenerate))
                    let second = multiply (fst firstGenerate) (fst secondGenerate)
                    Expect.equal first second "Needs to be equal"
            testProperty "test that write and read correctly"
            <| fun (x, y) ->
                if x > 0 && y > 0
                then
                    let f = generateRandomBoolMatrix x y 
                    writeOutputMatrix (fst f) "dasd.txt"
                    Expect.equal (fst f) (returnMatrix (readMatrix "dasd.txt")) "needs to be equal"
        ]
[<Tests>]
let checkSpecific =
    testList "tests specific values"
        [
           testCase "empty "
           <| fun _ ->
               let first = generateRandomBoolMatrix 0 0
               let second = generateRandomBoolMatrix 0 0
               Expect.equal (multiplyBool (snd first) (snd second)) (Matrix (0, 0, [])) "Needs to be equal"
           testCase "Input values < 0"
           <| fun _ ->
               Expect.throws (fun _ -> (fst (generateRandomBoolMatrix -1 -1)) |> ignore) "Cannot create matrix" 
        ]

