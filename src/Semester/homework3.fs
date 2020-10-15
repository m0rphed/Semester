module domashka2 
    let rec Task7 n =
        if n < 0
        then failwith "Fibonacci number is not negative"
        if n = 1 
        then 1
        elif n = 0
        then 0
        else Task7 (n - 1) + Task7 (n - 2)       
    let Task8 t =
        if t < 0 
        then failwith "Fibonacci number is not negative"       
        if t = 1
        then 1
        elif t = 0
        then 0
        else
            let a = [|0 .. t|]
            a.[0] <- 1
            a.[1] <- 1
            for i in 2 .. t - 1 do
                a.[i] <-  a.[i - 1] +  a.[i - 2]
            a.[t - 1]                  
    let Task9 q =
        if q < 0
        then failwith "Fibonacci number is not negative"
        let rec calc q k t = 
            if q = 1 
            then t
            elif q = 0
            then k
            else calc (q - 1) t (k + t)
        calc q 0 1     
    let Task12 t =
        if t < 0
        then failwith "Fibonacci number is not negative"
        let a = [|0 .. t|]
        for i in 0 .. t do 
            if i = 0
            then a.[i] <- 0
            elif i = 1
            then a.[i] <- 1
            else a.[i] <- a.[i - 1] + a.[i - 2]
        a   
    let multiply (o: array<array<int>>) (t: array<array<int>>) = 
        let I = o.Length 
        let J = o.[0].Length
        let K = t.[0].Length
        if I <> J
        then failwith "Matrix needs to be square"
        let Matrix = Array.init I ( fun _ -> Array.zeroCreate J )
        for i in 0 .. I - 1 do 
            for j in 0 .. J - 1  do 
                for k in 0 .. K - 1 do 
                    Matrix.[i].[j] <- Matrix.[i].[j] + (o.[k].[j] * t.[i].[k])
        Matrix
    let Matrixpow0 x =
        let matrix = Array.init x (fun _ -> Array.zeroCreate x)
        for i in 0 .. x - 1 do
            matrix.[i].[i] <- 1
        matrix
    let Task10 n  =
        if n < 0
        then failwith "Fibonacci number is not negative"
        let x = [|[|0; 1|]; [|1; 1|]|]
        let rec help n (x: int [] []) =
            if n = 1 
            then x
            elif n = 0
            then Matrixpow0 2
            else
                multiply ((help (n - 1) x)) x
        (help n x).[0].[1]
    let powMatrix (e: int [] []) x =
        if x = 0
        then Matrixpow0 e.Length
        elif x < 0
        then failwith "cannot power in negative digit"
        else
            let mutable k = e
            for i in 1 .. x - 1 do
                k <- multiply k e
            k                 
    let Task11 x =
        if x < 0
        then failwith "Fibonacci number is not negative"
        let rec helper x =
            let z = [|[|0; 1|]; [|1; 1|]|]
            if x = 1 
            then z
            elif x = 0
            then Matrixpow0 2
            else
                if x % 2 = 0
                then
                    powMatrix (helper (x / 2)) 2
                else
                    multiply z (powMatrix (helper ((x-1) / 2)) 2)
        (helper x).[0].[1]
