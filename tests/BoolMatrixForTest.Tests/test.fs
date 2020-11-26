module test
open Expecto
open System
[<Tests>]
let checkEquality =
    testList "test eqaulity"
        [
            testProperty "test"
            <| fun (k: int, n: int, t: int) ->
                if k <> 0 && n <> 0 && t <> 0
                then
                    let fitstGenerate = bMatrix.generateRandomBoolMatrix (abs n) (abs k)
                    let secondGenerate = bMatrix.generateRandomBoolMatrix (abs k) (abs t)
                    let first = bMatrix.returnMatrix (bMatrix.multiplyBool (bMatrix.createBoolMatrixFromStandart fitstGenerate) (bMatrix.createBoolMatrixFromStandart secondGenerate))
                    let second = bMatrix.multiply fitstGenerate secondGenerate
                    Expect.equal first second "Needs to be equal"
            testProperty "test that write and read correctly"
            <| fun (x, y) ->
                if x > 0 && y > 0
                then
                    let f = bMatrix.generateRandomBoolMatrix x y 
                    bMatrix.writeOutputMatrix f "dasd.txt"
                    Expect.equal f (bMatrix.returnMatrix (bMatrix.readMatrix "dasd.txt")) "needs to be equal"
        ]
[<Tests>]
let checkSpecific =
    testList "tests specific values"
        [
           testCase "Pustoi massiv"
           <| fun _ ->
               let first = bMatrix.createBoolMatrixFromStandart (bMatrix.generateRandomBoolMatrix 0 0)
               let second = bMatrix.createBoolMatrixFromStandart (bMatrix.generateRandomBoolMatrix 0 0)
               Expect.equal (bMatrix.multiplyBool first second) (bMatrix.Matrix (0, 0, [])) "Needs to be equal"
           testCase "Input values < 0"
           <| fun _ ->
               Expect.throws (fun _ -> bMatrix.generateRandomBoolMatrix -1 -1 |> ignore) "Cannot create matrix" 
        ]

