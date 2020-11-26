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
            val m: int
            val n: int
            val lst: list<Pair>
            new (k, p, lsts) = {m = k; n = p; lst = lsts}
            
  (*  let readMatrix file =
        let a = System.IO.File.ReadAllLines file
        if a.Length = 0
        then failwith "nothing to read"
        else
            let mutable k = 0
            for i = 0 to a.Length - 1 do
                for j in 0 .. (a.[i].Split ' ').Length - 1 do
                    if int (a.[i].Split ' ').[j] = 1
                    then k <- k + 1
            let newArr = Array.zeroCreate k
            k <- 0
            for i = 0 to a.Length - 1 do
                for j in 0 .. (a.[i].Split ' ').Length - 1 do
                    if int (a.[i].Split ' ').[j] = 1
                    then
                        newArr.[k] <- Pair (i * 1<Row>, j * 1<Col>) 
                        k <- k + 1               
            let mtrx = Matrix (a.Length, (a.[0].Split ' ').Length, List.ofArray newArr)
            mtrx Почему это считывание хуже чем с фолдами? *)

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
        if o.n = t.m
        then
            let ls = List.distinct [for i in 0 .. o.lst.Length - 1 do
                                        for j in 0 .. t.lst.Length - 1 do
                                            if int o.lst.[i].y = int t.lst.[j].x
                                            then Pair(int o.lst.[i].x * 1<Row>, int t.lst.[j].y * 1<Col>)
                                   ]
            Matrix(o.m, t.n, ls)
        else failwith "Cannot multiply this"

    let printListofPairs (x: Matrix) =
        for i in 0 .. x.lst.Length - 1 do
            printfn ("\n %A, %A") x.lst.[i].x x.lst.[i].y

    let returnMatrix (o: Matrix) =
        if o.m = o.n && o.n < 0
        then failwith "nothing to return"
        elif o.m = 0 && o.n = 0
        then Array2D.zeroCreate 0 0 
        else
            let Mtrx = Array2D.zeroCreate o.m o.n
            for i in 0 .. o.lst.Length - 1 do
                Mtrx.[int o.lst.[i].x, int o.lst.[i].y] <- 1
            Mtrx

    let writeOutputMatrix (o: int[,]) path =
        if o.Length = 0
        then failwith "nothing to write"
        else
            let strArray = Array2D.map (fun i -> string i) o
            printfn ("%A") strArray
            let str = Array.init strArray.[*,0].Length (fun i -> "")
            printfn ("%A") str
            for i in 0 .. (Array2D.length1 strArray) - 1 do
                for j in 0 .. (Array2D.length2 strArray) - 1 do
                    str.[i] <- str.[i] + strArray.[i,j] + " "
                    if j = (Array2D.length2 strArray) - 1
                    then str.[i] <- str.[i].Trim()
            File.WriteAllLines (path, str)

    let createBoolMatrixFromStandart (x: int[,]) =
        if x.Length = 0
        then Matrix (0, 0, [])
        elif x.Length < 0
        then failwith "Cannot create"
        else
            let mutable k = 0
            for i in 0 .. (Array2D.length1 x) - 1 do
                for j in 0 .. (Array2D.length2 x) - 1 do
                    if abs x.[i,j] > 0
                    then k <- k + 1
            let arr = Array.init k (fun _ -> Pair (-1<Row>, -1<Col>))
            k <- 0
            for i in 0 .. (Array2D.length1 x) - 1  do
                for j in 0 .. (Array2D.length2 x) - 1 do
                    if abs x.[i,j] > 0
                    then
                        arr.[k] <- Pair (i * 1<Row>, j * 1<Col>)
                        k <- k + 1
            Matrix ((Array2D.length1 x), (Array2D.length2 x), List.ofArray arr)
    //Начиная с этого места идут функции нужные для тест проперти
    let multiply (o: int[,]) (t: int[,]) =       
        let I = Array2D.length2 o
        let J = Array2D.length1 o 
        let K = Array2D.length1 t 
        let R = Array2D.length2 t
        if I = K
        then
            let Mtx = Array2D.zeroCreate J R
            for i in 0 .. J - 1 do 
                for j in 0 .. R - 1 do 
                    for k in 0 .. I - 1 do 
                        Mtx.[i,j] <- Mtx.[i,j] + (t.[k,j] * o.[i,k])
            for i in 0 .. J - 1 do
                for j in 0 .. R - 1 do
                    if abs Mtx.[i,j] > 0
                    then Mtx.[i,j] <- 1
            Mtx      
        else failwith "Cannot multiply this"

    let readMatrixForTests file =
        let a = System.IO.File.ReadAllLines file
        if a.Length = 0
        then failwith "nothing to read"
        else
            let mutable k = 0
            let m = Array2D.zeroCreate a.Length (a.[0].Split ' ').Length 
            for i = 0 to a.Length - 1 do
                for j in 0 .. (a.[i].Split ' ').Length - 1 do
                    m.[i,j] <- int (a.[i].Split ' ').[j]
            m

    let generateRandomBoolMatrix m n = 
        let rnd = System.Random()
        let arr = Array2D.init m n (fun x y -> rnd.Next(0,2))
        arr
 
    

            
