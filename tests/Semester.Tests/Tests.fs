module Tests


open Expecto
open Semester

[<Tests>]
let tests =
    testList "samples"
        [ // дамаха) N1
            testCase "0 to any power of 0 other than zero" <| fun _ ->
                let subject = domashka.Ftask 0.
                Expect.equal subject 1. "result must be equal 1."
            testCase "the polynomial takes the value 121 for x = 3" <| fun _ ->
                let subject = domashka.Ftask 3.
                Expect.equal subject 121. "result must be equal 121."
            testCase "0 to any power of 0 other than zero 2 task" <| fun _ ->
                let subject = domashka.Stask 0.
                Expect.equal subject 1. "result must be equal 1"
            testCase "the polynomial takes the value 121 for x = 3 2 task" <| fun _ ->
                let subject = domashka.Stask 3.
                Expect.equal subject 121. "result must be equal 121.0"
            // дамаха) N2
            testCase "First Fibonacci number, first task" <| fun _ ->
                let subject = domashka2.Task7 1
                Expect.equal subject 1 "result must be equal 1"
            testCase "Second Fibonacci number, first task" <| fun _ ->
                let subject = domashka2.Task7 2
                Expect.equal subject 1 "result must be equal 1"
            testCase "Third Fibonacci number, first task" <| fun _ ->
                let subject = domashka2.Task7 3
                Expect.equal subject 2 "result must be equal 2"
            testCase "Twelveth Fibonacci number, first task" <| fun _ ->
                let subject = domashka2.Task7 12
                Expect.equal subject 144 "result must be equal 144"
            testCase "First Fibonacci number, second task" <| fun _ ->
                let subject = domashka2.Task8 1
                Expect.equal subject 1 "result must be equal 1"
            testCase "Second Fibonacci number, second task" <| fun _ ->
                let subject = domashka2.Task8 2
                Expect.equal subject 1 "result must be equal 1"
            testCase "Third Fibonacci number, second task" <| fun _ ->
                let subject = domashka2.Task8 3
                Expect.equal subject 2 "result must be equal 2"
            testCase "Twelveth Fibonacci number, second task" <| fun _ ->
                let subject = domashka2.Task8 12
                Expect.equal subject 144 "result must be equal 144"
            testCase "First Fibonacci number, third task" <| fun _ ->
                let subject = domashka2.Task9 1
                Expect.equal subject 1 "result must be equal 1"
            testCase "Second Fibonacci number, third task" <| fun _ ->
                let subject = domashka2.Task9 2
                Expect.equal subject 1 "result must be equal 1"
            testCase "Third Fibonacci number, third task" <| fun _ ->
                let subject = domashka2.Task9 3
                Expect.equal subject 2 "result must be equal 2"
            testCase "Twelveth Fibonacci number, third task" <| fun _ ->
                let subject = domashka2.Task9 12
                Expect.equal subject 144 "result must be equal 144"
            testCase "First Fibonacci number, fourth task" <| fun _ ->
                let subject = domashka2.Task10 1
                Expect.sequenceEqual subject  [| [|0; 1|]; [|1;2|]|] "result must be equal 1"
            testCase "Third Fibonacci number, fourth task" <| fun _ ->
                let subject = domashka2.Task10 3
                Expect.sequenceEqual subject [| [|1; 1|]; [|2;3|]|] "result must be equal 2"
            testCase "Twelveth Fibonacci number, fourth task" <| fun _ ->
                let subject = domashka2.Task10 12
                Expect.sequenceEqual subject [| [|89; 144|]; [|233;337|]|] "result must be equal 144"
            testCase "First Fibonacci number, fifth task" <| fun _ ->
                let subject = domashka2.Task11 1
                Expect.sequenceEqual subject [| [|0; 1|]; [|1;2|]|] "result must be equal 1"          
            testCase "Third Fibonacci number, fifth task" <| fun _ ->
                let subject = domashka2.Task11 3
                Expect.sequenceEqual subject [| [|1; 2|]; [|2;3|]|] "result must be equal 2"
            testCase "Twelveth Fibonacci number, fifth task" <| fun _ ->
                let subject = domashka2.Task11 12
                Expect.sequenceEqual subject [| [|89; 144|]; [|144;233|]|] "result must be equal 144"
            testCase "fibonnacis numbers 1" <| fun _ ->
                let subject = domashka2.Task12 1
                Expect.sequenceEqual subject [|1|] "result must be equal [| 1 |]"
            testCase "fibonnacis numbers 2" <| fun _ ->
                let subject = domashka2.Task12 2
                Expect.sequenceEqual subject [| 1; 1 |] "result must be equal [| 1; 1 |]"
            testCase "fibonnacis numbers 3" <| fun _ ->
                let subject = domashka2.Task12 3
                Expect.sequenceEqual subject [| 1; 1; 2 |] "result must be equal [| 1; 1; 2 |]"
            testCase "Twelveth Fibonacci number, fifth task" <| fun _ ->
                let subject = domashka2.Task12 4
                Expect.sequenceEqual subject [| 1; 1; 2; 3 |] "result must be equal [| 1; 1; 2; 3 |]"
        ]   

            




