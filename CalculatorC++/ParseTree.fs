module ParseTree

open Exp

open System.Collections.Generic

open BigAriphmetics

let private toDotList = new Stack<_>() 

let private numbersOfOp = new Stack<_>() // нужно нумеровать операции чтобы они не склеивались в доте

numbersOfOp.Push("op0[shape = ellipse, label = program]")

let mutable private counter = 0

let private namingOp stringNameOp =
    counter <- counter + 1
    let numOfOp = "op" + string counter
    numbersOfOp.Push(numOfOp + sprintf "[shape = ellipse, label = %A];" stringNameOp)
    numOfOp

let private helpFunc vDict x y stringNameOp preOperationString operation1 mainRec = // вынес код и переиспользую
    let numOfOp = namingOp stringNameOp
    toDotList.Push(preOperationString + " -> " + numOfOp)
    let x1 = (mainRec vDict x numOfOp)
    let y1 = (mainRec vDict y numOfOp)
    operation1 x1 y1

let private helpFunc1 vDict x stringNameOp preOperationString operation1 mainRec =
    let numOfOp = namingOp stringNameOp
    toDotList.Push(preOperationString + " -> " + numOfOp) 
    operation1 (mainRec vDict x numOfOp)
    
let private processExpr (vDict: Dictionary<_,_>) expr toDotOp =
    let rec _go (vDict: Dictionary<_,_>) expr toDotOp = 
        match expr with
        | Exp.Num t ->
            let name = namingOp (string (toInt t))
            toDotList.Push(toDotOp + " -> " + string name)
            t
        | Exp.NVar t -> 
            let data =
                try
                    vDict.[t]
                with
                | _ -> failwithf "Variable %A is not declared" t
            let name =
                string <|
                match t with
                | Var t -> t
            let nvar = namingOp "Exp.NVar"
            let nVarName = namingOp name
            toDotList.Push(toDotOp + " -> " + nvar)
            toDotList.Push(nvar + " -> " + nVarName)
            data
        | Exp.Sum (x, y) -> helpFunc vDict x y " Exp.Sum " toDotOp sum _go     
        | Exp.Sub (x, y) -> helpFunc vDict x y " Exp.Sub " toDotOp sub _go   
        | Exp.Mul (x, y) -> helpFunc vDict x y " Exp.Mul " toDotOp multiply _go   
        | Exp.Div (x, y) -> helpFunc vDict x y " Exp.Div " toDotOp division _go
        | Exp.Abs x -> helpFunc1 vDict x " Exp.Abs " toDotOp absolute _go
        | Exp.Bin x -> helpFunc1 vDict x " Exp.Bin " toDotOp toBinary _go
        | Exp.Pow (x, y) -> helpFunc vDict x y " Exp.Pow " toDotOp power _go
        | Exp.DivRem (x, y) -> helpFunc vDict x y " Exp.DivRem " toDotOp remDiv _go        
    _go vDict expr toDotOp

let run out ast path =
    
    let processStmt (vDict: Dictionary<_,_>) stmt =
        match stmt with
        | Exp.Print t ->
            let (data: BigInt) =
                try
                    vDict.[t]
                with
                | _ -> failwithf "Variable %A is not declared" t
            let name =
                string <|
                match t with
                | Var t -> t
            let numOfOp = namingOp "print"
            let nVarName = namingOp name
            toDotList.Push(numOfOp + " -> " + nVarName)
            toDotList.Push("op0 -> " + numOfOp)
            if out then printfn "%A" (toInt data) else ()
        | Exp.VDecl (x, y) ->
            let name =
                string <|
                match x with
                | Var x -> x
            let numOfOp = namingOp name
            let vDecl = namingOp "VDecl"
            toDotList.Push("op0 -> " + vDecl)
            toDotList.Push(vDecl + " -> " + numOfOp)
            if vDict.ContainsKey x
            then vDict.[x] <- processExpr vDict y vDecl
            else vDict.Add (x, processExpr vDict y vDecl)
        vDict

    let vDict = new Dictionary<_,_>()
    List.fold processStmt vDict ast |> ignore
    let secArr = Array.append [|"digraph ParseTree"; "{"; "rankdir = TB"|] (Array.rev (numbersOfOp.ToArray()))
    let firstArr = (toDotList.ToArray())
    let newArr = Array.append secArr firstArr 
    newArr.[newArr.Length - 1] <- newArr.[newArr.Length - 1] + "}"
    System.IO.File.WriteAllLines (path, List.ofArray newArr)

