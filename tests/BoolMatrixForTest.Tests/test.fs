module test
open Expecto
open System
open BoolMatrix
[<Tests>]
let checkEquality =
    testList "test eqaulity"
        [
            testProperty "test"
            <| fun (k: int, n: int, t: int) ->
                if k <> 0 && n <> 0 && t <> 0
                then
                    let fitstGenerate = generateRandomBoolMatrix (abs n) (abs k)
                    let secondGenerate = generateRandomBoolMatrix (abs k) (abs t)
                    let first = returnMatrix (multiplyBool (createBoolMatrixFromStandart fitstGenerate) (createBoolMatrixFromStandart secondGenerate))
                    let second = multiply fitstGenerate secondGenerate
                    Expect.equal first second "Needs to be equal"
            testProperty "test that write and read correctly"
            <| fun (x, y) ->
                if x > 0 && y > 0
                then
                    let f = generateRandomBoolMatrix x y 
                    writeOutputMatrix f "dasd.txt"
                    Expect.equal f (returnMatrix (readMatrix "dasd.txt")) "needs to be equal"
        ]
[<Tests>]
let checkSpecific =
    testList "tests specific values"
        [
           testCase "Pustoi massiv"
           <| fun _ ->
               let first = createBoolMatrixFromStandart (generateRandomBoolMatrix 0 0)
               let second = createBoolMatrixFromStandart (generateRandomBoolMatrix 0 0)
               Expect.equal (multiplyBool first second) (Matrix (0, 0, [])) "Needs to be equal"
           testCase "Input values < 0"
           <| fun _ ->
               Expect.throws (fun _ -> generateRandomBoolMatrix -1 -1 |> ignore) "Cannot create matrix" 
        ]

