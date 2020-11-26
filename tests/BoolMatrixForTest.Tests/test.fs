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
                    let fitstGenerate = BoolMatrix.generateRandomBoolMatrix (abs n) (abs k)
                    let secondGenerate = BoolMatrix.generateRandomBoolMatrix (abs k) (abs t)
                    let first = BoolMatrix.returnMatrix (BoolMatrix.multiplyBool (BoolMatrix.createBoolMatrixFromStandart fitstGenerate) (BoolMatrix.createBoolMatrixFromStandart secondGenerate))
                    let second = BoolMatrix.multiply fitstGenerate secondGenerate
                    Expect.equal first second "Needs to be equal"
            testProperty "test that write and read correctly"
            <| fun (x, y) ->
                if x > 0 && y > 0
                then
                    let f = BoolMatrix.generateRandomBoolMatrix x y 
                    BoolMatrix.writeOutputMatrix f "dasd.txt"
                    Expect.equal f (BoolMatrix.returnMatrix (BoolMatrix.readMatrix "dasd.txt")) "needs to be equal"
        ]
[<Tests>]
let checkSpecific =
    testList "tests specific values"
        [
           testCase "Pustoi massiv"
           <| fun _ ->
               let first = BoolMatrix.createBoolMatrixFromStandart (BoolMatrix.generateRandomBoolMatrix 0 0)
               let second = BoolMatrix.createBoolMatrixFromStandart (BoolMatrix.generateRandomBoolMatrix 0 0)
               Expect.equal (BoolMatrix.multiplyBool first second) (Matrix (0, 0, [])) "Needs to be equal"
           testCase "Input values < 0"
           <| fun _ ->
               Expect.throws (fun _ -> BoolMatrix.generateRandomBoolMatrix -1 -1 |> ignore) "Cannot create matrix" 
        ]

