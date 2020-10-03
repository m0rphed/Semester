namespace Semester
module domashka =
    let Ftask x = x*x*x*x + x*x*x + x*x + x + 1.


    let Stask q = 
        let square = q * q
        ( square + q ) * ( square + 1. ) + 1.         
       

    let create_array dimensionArray =
        let genRandomNumbers x1 =
            let rnd = System.Random()
            Array.init x1 (fun _ -> rnd.Next (1,1000)) // Для удобства рандомные числа от 1 до 1000
        let array: int array = genRandomNumbers dimensionArray  
        printf "Сгенерированные элементы массива: "
        printfn "%A" array
        array


    let Thirdtask ( array: int array )   bigelement =
        let mutable k = 0
        let mutable t = 0
        for i = 0 to array.Length - 1 do 
            if array.[i] < bigelement then
                k <- k + 1
        let secondarray = Array.zeroCreate k 
        for i = 0 to array.Length - 1 do 
            if array.[i] < bigelement then
                secondarray.[t] <- i
                t <- t + 1
        secondarray 


    let Fourthtask ( array: int array )  felement selement =
        if felement > selement
        then failwith "left can be lesser than right try again"
        let mutable k = 0
        for i = 0 to array.Length - 1 do 
            if ( array.[i] > selement ) then 
                k <- k + 1
            if ( array.[i] < felement ) then
                k <- k + 1
        let thirdarray = Array.zeroCreate k
        k <- 0
        for i = 0 to thirdarray.Length - 1 do 
           if ( array.[i] > selement ) then
               thirdarray.[k] <- i
           if ( array.[i] < felement ) then  
               thirdarray.[k] <- i
           k <- k + 1     
        thirdarray   


    let Fifthtask ( array: int array ) =
        array.[0] <- array.[0] + array.[1]
        array.[1] <- array.[0] - array.[1]
        array.[0] <- array.[0] - array.[1]
        array


    let Sixthtask ( array: int array ) z v = 
        array.[z] <- array.[z] + array.[v]
        array.[v] <- array.[z] - array.[v]
        array.[z] <- array.[v] - array.[z]
        array


module domashka2 =
        let rec Task7 n =
            if n < 0
            then failwith "Fibonacci number is not negative"
            if ( n = 1 ) || ( n = 2 ) 
            then 1 
            else Task7( n - 1 ) + Task7( n - 2 )
        
        
        let Task8 t =
            if t <= 0 
            then failwith "Fibonacci number is not negative"
            let a = [| 0 .. t - 1 |]
            if t = 1 then 1
            else 
                a.[0] <- 1
                a.[1] <- 1
                for i in 2 .. t - 1 do
                    a.[i] <-  a.[i - 1] +  a.[i - 2]
                a.[t - 1]
                    
        
        let Task9 q =
            if q <= 0
            then failwith "Fibonacci number is not negative"
            let rec calc q k t = 
                if q = 1 || q = 0 
                then k
                else calc ( q - 1 ) t ( k + t )
            calc q 1 1
        
        
        let Task12 t =
            if t < 0
            then failwith "Fibonacci number is not negative"
            let a = [| 0 .. t - 1 |]
            for i in 0 .. t - 1 do 
                if ( i = 0 ) || ( i = 1 )
                then a.[i] <- 1
                else a.[i] <- a.[i - 1] + a.[i - 2]
            a
        
        
        let multiply ( o: array<array<int>> ) ( t: array<array<int>> ) = 
            let I = o.Length 
            let J = o.[0].Length
            let K = t.[0].Length 
            let Matrix = Array.init I ( fun _ -> Array.zeroCreate J )
            for i in 0 .. I - 1 do 
                for j in 0 .. J - 1  do 
                    for k in 0 .. K - 1 do 
                        Matrix.[i].[j] <- Matrix.[i].[j] + (o.[k].[j] * t.[i].[k])
            Matrix
        let Task10 n  =
            if n <= 0
            then failwith "Fibonacci number is not negative"
            let x = [| [| 0; 1 |]; [| 1; 1 |] |]
            let rec help n x: array<array<int>> =
                if n = 1 || n = 0
                then x
                else
                    multiply (( help ( n - 1 ) x )) x
            ( help n x ).[0].[1]
        let powMatrix x =
            multiply x x           

            
        let Task11 x =
            if x <= 0
            then failwith "Fibonacci number is not negative"
            let rec helper x =
                let z = [| [| 0; 1 |]; [| 1; 1 |] |]
                if x = 1 || x = 0
                then z
                else
                    if x % 2 = 0
                    then
                        powMatrix (helper ( x / 2 ))
                    else
                        multiply z (powMatrix (helper ( ( x-1 ) / 2 )))
            (helper x).[0].[1]
                        
                    
            
            
        


        
             

            
