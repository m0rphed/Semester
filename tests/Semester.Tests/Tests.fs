module Tests


open Expecto
open Semester

[<Tests>]
let tests =
    testList "samples"
        [ // дамаха) N1/2
            testCase "Given empty array gives empty array thirdtask" <| fun _ ->
                let subject = domashka.Thirdtask [||] 3
                Expect.equal subject [||] "Given empty array"
            testCase "Given empty array gives empty array fourth task" <| fun _ ->
                let subject = domashka.Fourthtask [||] 3 5 
                Expect.equal subject [||] "Given empty array"
            testCase "Positive always greater than negative its obvious,thirdtask" <| fun _ ->
                let subject = domashka.Thirdtask [|-2; -1; 1; 2 |] 0
                Expect.equal subject [| 0; 1 |] "result must be equal [| 0; 1 |]"
            testCase "Positive always greater than negative its obvious fourth task" <| fun _ ->
                let subject = domashka.Fourthtask [|-2; -1; 1; 2 |] 0 2 
                Expect.equal subject [| 0; 1 |] "result must be equal [| 0; 1 |]"           
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
            testCase "First Fibonacci number, first task" <| fun _ ->
                let subject = domashka2.Task7 1
                Expect.equal subject 1 "rфывфывesult must be equal 1"
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
                Expect.equal subject 144 "result must be equal "
            testCase "First Fibonacci number, fourth task" <| fun _ ->
                let subject = domashka2.Task10 1
                Expect.equal subject  1 "result must be equal 1"
            testCase "Third Fibonacci number, fourth task" <| fun _ ->
                let subject = domashka2.Task10 3
                Expect.equal subject 2 "result must be equal 2"
            testCase "Twelveth Fibonacci number, fourth task" <| fun _ ->
                let subject = domashka2.Task10 12
                Expect.equal subject 144 "result must be equal 144"
            testCase "First Fibonacci number, fifth task" <| fun _ ->
                let subject = domashka2.Task11 1
                Expect.equal subject 1 "result must be equal 1"          
            testCase "Third Fibonacci number, fifth task" <| fun _ ->
                let subject = domashka2.Task11 3
                Expect.equal subject 2 "result must be equal 2"
            testCase "Twelveth Fibonacci number, fifth task" <| fun _ ->
                let subject = domashka2.Task11 12
                Expect.equal subject 144 "result must be equal 144"
            testCase "fibonnacis numbers 1" <| fun _ ->
                let subject = domashka2.Task12 1
                Expect.sequenceEqual subject [|1|] "result must be equal [| 1 |]"
            testCase "fibonnacis numbers 2" <| fun _ ->
                let subject = domashka2.Task12 2
                Expect.sequenceEqual subject [| 1; 1 |] "result must be equal [| 1; 1 |]"
            testCase "fibonnacis numbers 3" <| fun _ ->
                let subject = domashka2.Task12 3
                Expect.sequenceEqual subject [| 1; 1; 2 |] "result must be equal [| 1; 1; 2 |]"
            testCase "Twelveth Fibonacci number, six" <| fun _ ->
                let subject = domashka2.Task12 4
                Expect.sequenceEqual subject [| 1; 1; 2; 3 |] "result must be equal [| 1; 1; 2; 3 |]"
            let moduleTest n =
                if n = 0 then 1
                else if n < 0
                then -n
                else n
            testProperty "task9 task10"
                <| fun ( n: int ) -> Expect.equal ( domashka2.Task9 ( moduleTest n )) ( domashka2.Task10 ( moduleTest n )) "Compare task10 task9"
            testProperty "task8 task9"
                <| fun ( n: int ) -> Expect.equal ( domashka2.Task8  (moduleTest n )) ( domashka2.Task9 ( moduleTest n )) "Compare task8 task9"
            testProperty "task10 task11"
            <| fun ( n: int ) -> Expect.equal ( domashka2.Task10 ( moduleTest n )) ( domashka2.Task11 ( moduleTest n )) "Compare task10 task11"
            ]    

            




