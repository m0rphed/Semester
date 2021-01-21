module ExtendedMatrix

open test

open bMatrix

type Triple =
    inherit Pair
    val data: int
    new (x, y, data) = {inherit Pair(x * 1<Row>, y * 1<Col>); data = data}

type ExtendedMatrix =
    val numOfRows: int
    val numOfCols: int
    val notEmptyData: list<Triple> 
    new (k, p, lsts) = {numOfRows = k; numOfCols = p; notEmptyData = lsts}

// генерирует кортеж из int[,] и ее расширенной матрицы
let generator k i =
    let reconstruct, reconstruct1 = generateRandomBoolAndDefoltMtx k i
    let x = List.map (fun (elem: Pair) ->
        Triple (int elem.x, int elem.y, (System.Random().Next()))) reconstruct1.notEmptyData
    for j in x do reconstruct.[int j.x, int j.y] <- j.data
    reconstruct, ExtendedMatrix(reconstruct1.numOfRows, reconstruct1.numOfCols, x)

// создает ExtendedMatrix на основе матрицы, в которой если value <> 0 существуют все элементы
let generatorOneValue k i value =
    let mutable counter = 0
    let mtx = Array2D.create k i value
    let arr = Array.create (k * i) (Triple(-1, -1, 0))
    for i in 0 .. Array2D.length1 mtx - 1 do
        for j in 0 .. Array2D.length2 mtx - 1 do
            arr.[counter] <- Triple (i, j, value)
            counter <- counter + 1
    ExtendedMatrix (k, i, List.ofArray arr)         
            
            
