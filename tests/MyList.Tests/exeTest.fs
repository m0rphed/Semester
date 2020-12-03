module exeTest
open Expecto
open Listik
open Tree

[<Tests>]
let firstPackTest =
    testList "Check equatation"
        [
            testProperty "checks equivivalent list and MyList"
            <| fun (k: int) ->
                if abs k >= 1
                then
                    let myList = generator (abs k)
                    Expect.equal (toMyList (toDefoltList myList)) myList "needs to be equal"

            testProperty "checks equivivalent list and MyList #2"
            <| fun (k: int) ->
                if abs k >= 1
                then
                    let commonList = List.init (abs k) (fun _ -> System.Random().Next())
                    Expect.sequenceEqual (toDefoltList (toMyList commonList)) commonList "needs to be equal"

            testProperty "Check length id"
            <| fun (k: int) ->
                if abs k >= 1
                then
                    let myList = generator (abs k)
                    Expect.equal ((toDefoltList myList).Length) (length myList) "length need to be equal"

            testProperty "Check concat id"
            <| fun (k: int,t: int) ->
                if abs k >= 1 && abs t >= 1 
                then
                    let myList0 = generator (abs t)
                    let myList = generator (abs k)
                    Expect.equal (toMyList (List.concat [(toDefoltList myList0); (toDefoltList myList)])) (concat myList0 myList) "equality needs to be"

            testProperty "Check sort id"
            <| fun (k: int) ->
                if abs k >= 1
                then
                    let myList = generator (abs k)
                    Expect.equal ((List.sort (toDefoltList myList))) (toDefoltList (sort myList)) "needs to be equal"      
        ]
[<Tests>]
let secondTest =
    testList "testcases"
        [
            testCase "stringToMyString"
            <| fun _ ->
                Expect.equal "привет" (toString (toMyString "привет")) "need to be equal"

            testCase "string concat"
            <| fun _ ->
                Expect.equal ("привет" + " Андрей") (toString (concatMyString (toMyString "привет") (toMyString " Андрей"))) "Needs to be equal"

            testCase "avgTree"
            <| fun _ ->
                Expect.equal (avgMyTree (Node ((15), Cons ((Leaf (13)), One (Leaf (10)))))) 12 "38 / 3 = 12"

            testCase "maxInTree"
            <| fun _ ->
                Expect.equal (maxInTree (Node ((15), Cons ((Leaf (13)), One (Leaf (10)))))) 15 "max is 15 in tree"
           
            testCase "map"
            <| fun _ ->
                let myList = generator 5
                Expect.equal (List.map ((+) 2) (toDefoltList myList)) (toDefoltList (map ((+) 2) myList)) "needs to be equal"    
   
            testCase "iter"
            <| fun _ ->
                let myList = generator 5
                Expect.equal (List.iter (printfn "%A") (toDefoltList myList)) (iter (printfn "%A") myList) "needs to be equal"
        ]
