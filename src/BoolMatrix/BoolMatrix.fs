module bMatrix
open System
open System.IO

[<Measure>] type Row
[<Measure>] type Col
[<Struct>]
type Pair =
    val x: int<Row>
    val y: int<Col>
    new (p,q) = {x = p; y = q}
[<Struct>]
type Matrix =
    val numOfRows: int
    val numOfCols: int
    val notEmptyData: list<Pair>
    new (k, p, lsts) = {numOfRows = k; numOfCols = p; notEmptyData = lsts}
            
let readMatrix file =
    let processLine (str: string) (i, lst) =
        str.Split ' '
        |> Array.fold (
                        fun (j, lst) c ->
                            if c = "1"
                            then (j + 1, Pair (i * 1<Row>, j * 1<Col>) :: lst)
                            elif c = "0"
                            then (j + 1, lst)
                            else failwith "Matrix has incorrect format")
                        (0, lst)
    let rows = System.IO.File.ReadAllLines file
    let lengths = rows |> Array.map String.length |> Array.distinct
    if lengths.Length > 1 then failwith "Matrix has incorrect format"
    let mtx =
        rows
        |> Array.fold (fun (i, lst) str -> (i + 1, processLine str (i, snd lst))) (0, (0, []))
    let matrix = Matrix (fst mtx, fst (snd mtx), snd (snd mtx))
    matrix

let multiplyBool (o: Matrix) (t: Matrix) =
    if o.numOfCols = t.numOfRows
    then
        let ls = List.distinct [for i in 0 .. o.notEmptyData.Length - 1 do
                                    for j in 0 .. t.notEmptyData.Length - 1 do
                                        if int o.notEmptyData.[i].y = int t.notEmptyData.[j].x
                                        then Pair(int o.notEmptyData.[i].x * 1<Row>, int t.notEmptyData.[j].y * 1<Col>)
                                ]
        Matrix(o.numOfRows, t.numOfCols, ls)
    else failwith "Cannot multiply this"

let printListofPairs (x: Matrix) =
    for i in 0 .. x.notEmptyData.Length - 1 do
        printfn ("\n %A, %A") x.notEmptyData.[i].x x.notEmptyData.[i].y

let returnMatrix (o: Matrix) =
    if o.numOfRows = o.numOfCols && o.numOfCols < 0
    then failwith "nothing to return"
    elif o.numOfRows = 0 && o.numOfCols = 0
    then Array2D.zeroCreate 0 0 
    else
        let mtrx = Array2D.zeroCreate o.numOfRows o.numOfCols
        for i in 0 .. o.notEmptyData.Length - 1 do
            mtrx.[int o.notEmptyData.[i].x, int o.notEmptyData.[i].y] <- 1
        mtrx

let writeOutputMatrix (o: int[,]) path =
    if o.Length = 0
    then failwith "nothing to write"
    else
        let strArray = Array2D.map (fun i -> string i) o
        let str = Array.init strArray.[*,0].Length (fun i -> "")
        for i in 0 .. (Array2D.length1 strArray) - 1 do
            for j in 0 .. (Array2D.length2 strArray) - 1 do
                str.[i] <- str.[i] + strArray.[i,j] + " "
                if j = (Array2D.length2 strArray) - 1
                then str.[i] <- str.[i].Trim()
        File.WriteAllLines (path, str)
