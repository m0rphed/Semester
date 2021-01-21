module LongArithmetic
open Expecto
open BigAriphmetics
open Listik
[<Tests>]
let testingsOperations =
    testList "sum div multi subtr"
        [
            testProperty "sum"
            <| fun (k: int, t: int) ->
                   if k <> 0 && abs k < 8 && t <> 0 && abs t < 8 // чтоб инты эксшепшн не выдавали
                   then
                       let first = generator (abs k)
                       let second = generator (abs t)
                       let firstInt = fold (fun acc elem -> string elem + acc) "" (rev first) |> int
                       let secondInt = fold (fun acc elem -> string elem + acc) "" (rev second) |> int
                       Expect.equal (firstInt + secondInt) (fold (fun acc elem -> string elem + acc) "" (rev (sum first second)) |> int) "equals"

            testProperty "multiply"
                       <| fun (k: int, t: int) ->
                              if k <> 0 && abs k < 5 && t <> 0 && abs t < 4 // чтоб инты эксшепшн не выдавали
                              then
                                  let first = generator (abs k)
                                  let second = generator (abs t)
                                  let firstInt = fold (fun acc elem -> string elem + acc) "" (rev first) |> int
                                  let secondInt = fold (fun acc elem -> string elem + acc) "" (rev second) |> int
                                  Expect.equal (firstInt * secondInt) (fold (fun acc elem -> string elem + acc) "" (rev (multiply first second)) |> int) "equals1"

            testProperty "subtract"
            <| fun (k: int, t: int) ->
                   if k <> 0 && abs k < 8 && t <> 0 && abs t < 8 // чтоб инты эксшепшн не выдавали
                   then
                       let first = generator (abs k)
                       let second = generator (abs t)
                       let firstInt = fold (fun acc elem -> string elem + acc) "" (rev first) |> int
                       let secondInt = fold (fun acc elem -> string elem + acc) "" (rev second) |> int
                       Expect.equal (firstInt - secondInt) (fold (fun acc elem -> string elem + acc) "" (rev (subtract first second)) |> int) "equals2"

            testProperty "division"
            <| fun (k: int, t: int) ->
                   if k <> 0 && abs k < 8 && t <> 0 && abs t < 8 // чтоб инты эксшепшн не выдавали
                   then
                       let first = generator (abs k)
                       let second = generator (abs t)
                       let firstInt = fold (fun acc elem -> string elem + acc) "" (rev first) |> int
                       let secondInt = fold (fun acc elem -> string elem + acc) "" (rev second) |> int
                       if secondInt = 0
                       then Expect.equal 1 1 "exception"
                       else Expect.equal (firstInt / secondInt) (fold (fun acc elem -> string elem + acc) "" (rev (division first second)) |> int) "equals3"

        ]
[<Tests>]
let testingOtherFunc =
    testList "other func"
        [
            testCase "transfer positive"
            <| fun _ ->
                Expect.equal (Cons (1,Cons (2,Cons (3, One 4)))) (transferPositive (rev (Cons (0, Cons (0, Cons (0, One 1234)))))) "needs to be equal"

            testCase "transfer negative"
            <| fun _ ->
                Expect.equal (Cons (2,Cons (0,Cons (8, One 0)))) (transferNegative (rev (Cons (2, Cons (3, Cons (-12, One -100)))))) "needs to be equal"

            testCase "greatest"
            <| fun _ ->
                Expect.isFalse (compareBigInt (Cons (2,Cons (0,Cons (8, One 0)))) (Cons (3,Cons (0,Cons (8, One 0))))) "first greater than second"

            testCase "equal"
            <| fun _ ->
                Expect.isTrue (equals (Cons (2,Cons (0,Cons (8, One 0)))) (Cons (2,Cons (0,Cons (8, One 0))))) "equal"

            testCase "index elem"
            <| fun _ ->
                Expect.equal 0 (indexElem (Cons (2,Cons (0,Cons (8, One 0)))) 2) "needs to be equal"

            testCase "choose part"
            <| fun _ ->
                Expect.equal  (Cons (0,One 8)) (choosePart (Cons (2,Cons (0,Cons (8, One 0)))) 2 3) "nees to be equal"

            testCase "delete zeroes"
            <| fun _ ->
                Expect.equal (Cons (2,Cons (0,Cons (8, One 0)))) (deleteZeroes (Cons (0, (Cons (2,Cons (0,Cons (8, One 0))))))) "needts to be equal"

            testCase "map2"
            <| fun _ ->
                Expect.equal (Cons (3, Cons (1, Cons (9, One 1)))) (rev (map2 (+) (One 3) (Cons (0,Cons (8, One 0))) (Cons (1,Cons (1, One 1))))) "needs to be equal"


            













        ]


