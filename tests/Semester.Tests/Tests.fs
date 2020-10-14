module Tests


open Expecto
open Semester
[<Tests>]
let tests1 =
    testList "first task tests"
        [
            testCase "first task for x = 0" <| fun _ ->
                let subject = domashka.Ftask 0.
                Expect.equal subject 1. "result must be equal 1."
            testCase "the polynomial takes the value 121 for x = 3" <| fun _ ->
                let subject = domashka.Ftask 3.
                Expect.equal subject 121. "result must be equal 121."
        ]
[<Tests>]
let tests2 =
    testList "second task tests"
        [
            testCase "0 to any power of 0 other than zero 2 task" <| fun _ ->
                let subject = domashka.Stask 0.
                Expect.equal subject 1. "result must be equal 1"
            testCase "the polynomial takes the value 121 for x = 3 2 task" <| fun _ ->
                let subject = domashka.Stask 3.
                Expect.equal subject 121. "result must be equal 121.0"  
        ]
[<Tests>]
let tests3 =
    testList "samples"
        [
            testCase "Given empty array gives empty array thirdtask" <| fun _ ->
                let subject = domashka.Thirdtask [||] 3
                Expect.equal subject [||] "Given empty array"          
            testCase "Positive always greater than negative its obvious,thirdtask" <| fun _ ->
                let subject = domashka.Thirdtask [|-2; -1; 1; 2 |] 0
                Expect.equal subject [| 0; 1 |] "result must be equal [| 0; 1 |]"
        ]
[<Tests>]
let tests4 =
    testList "fourth task tests"
        [
            testCase "Positive always greater than negative its obvious fourth task" <| fun _ ->
                let subject = domashka.Fourthtask [|-2; -1; 1; 2 |] 0 2 
                Expect.equal subject [| 0; 1 |] "result must be equal [| 0; 1 |]"           
            testCase "Given empty array gives empty array fourth task" <| fun _ ->
                let subject = domashka.Fourthtask [||] 3 5 
                Expect.equal subject [||] "Given empty array"
        ]
[<Tests>]
let tests5 =
    testList "fifth task tests"
        [
            testCase "input value" <| fun _ ->
                let subject = domashka.Fifthtask [|1; 2|] 
                Expect.sequenceEqual subject [|2; 1|] "result must be equal [|2; 1|]"
            testCase "input value 2 " <| fun _ ->
                let subject = domashka.Fifthtask [|0; -3|] 
                Expect.sequenceEqual subject [|-3; 0|] "result must be equal [|-3; 0|]"
        ]
[<Tests>]
let tests6 =
    testList "sixth task tests"
        [   testCase "throws" <| fun _ ->
                Expect.throws (fun _ -> domashka.Sixthtask [|1; 2; 3|] -1 5 |> ignore) "exception"
            testCase "input value 3" <| fun _ ->
                let subject = domashka.Sixthtask [|0; 1; 2; 3; 4; 5|] 1 2
                Expect.sequenceEqual subject [|0; 2; 1; 3; 4; 5|] "result must be equal [|0; 2; 1; 3; 4; 5|]"
        ]            
