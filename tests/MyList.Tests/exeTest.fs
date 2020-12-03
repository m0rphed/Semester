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
                    let myList = generatorMyList (abs k)
                    Expect.equal (fromStandartToMyList (fromMyListtoStandart myList)) myList "needs to be equal"

            testProperty "checks equivivalent list and MyList #2"
            <| fun (k: int) ->
                if abs k >= 1
                then
                    let commonList = List.init (abs k) (fun _ -> System.Random().Next())
                    Expect.sequenceEqual (fromMyListtoStandart (fromStandartToMyList commonList)) commonList "needs to be equal"

            testProperty "Check length id"
            <| fun (k: int) ->
                if abs k >= 1
                then
                    let myList = generatorMyList (abs k)
                    Expect.equal ((fromMyListtoStandart myList).Length) (length myList) "length need to be equal"

            testProperty "Check concat id"
            <| fun (k: int,t: int) ->
                if abs k >= 1 && abs t >= 1 
                then
                    let myList0 = generatorMyList (abs t)
                    let myList = generatorMyList (abs k)
                    Expect.equal (fromStandartToMyList (List.concat [(fromMyListtoStandart myList0); (fromMyListtoStandart myList)])) (concatMyList myList0 myList) "equality needs to be"

            testProperty "Check sort id"
            <| fun (k: int) ->
                if abs k >= 1
                then
                    let myList = generatorMyList (abs k)
                    Expect.equal ((List.sort (fromMyListtoStandart myList))) (fromMyListtoStandart (sortForMyList myList)) "needs to be equal"      
        ]
[<Tests>]
let secondTest =
    testList "testcases"
        [
            testCase "stringToMyString"
            <| fun _ ->
                Expect.equal "привет" (fromMyStringToString (fromStringToMyString "привет")) "need to be equal"

            testCase "string concat"
            <| fun _ ->
                Expect.equal ("привет" + " Андрей") (fromMyStringToString (concatMyString (fromStringToMyString "привет") (fromStringToMyString " Андрей"))) "Needs to be equal"

            testCase "avgTree"
            <| fun _ ->
                Expect.equal (avgMyTree (Node ((15), Cons ((Leaf (13)), One (Leaf (10)))))) 12 "38 / 3 = 12"

            testCase "maxInTree"
            <| fun _ ->
                Expect.equal (maxInMyTree (Node ((15), Cons ((Leaf (13)), One (Leaf (10)))))) 15 "max is 15 in tree"
           
            testCase "map"
            <| fun _ ->
                let myList = generatorMyList 5
                Expect.equal (List.map ((+) 2) (fromMyListtoStandart myList)) (fromMyListtoStandart (mapMyList ((+) 2) myList)) "needs to be equal"    
   
            testCase "iter"
            <| fun _ ->
                let myList = generatorMyList 5
                Expect.equal (List.iter (printfn "%A") (fromMyListtoStandart myList)) (iterMyList (printfn "%A") myList) "needs to be equal"
        ]
