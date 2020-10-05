namespace Semester
module domashka =
    let Ftask x = x*x*x*x + x*x*x + x*x + x + 1.

    let Stask q = 
        let square = q * q
        (square + q) * (square + 1.) + 1.         
       
    let create_array dimensionArray =
        let genRandomNumbers x1 =
            let rnd = System.Random()
            Array.init x1 (fun _ -> rnd.Next ())
        let array: int array = genRandomNumbers dimensionArray  
        printf "Сгенерированные элементы массива: "
        printfn "%A" array
        array

    let Thirdtask (array: int array)   bigelement =
        let mutable k = 0
        for i = 0 to array.Length - 1 do 
            if array.[i] < bigelement then
                k <- k + 1
        let secondarray = Array.zeroCreate k
        k <- 0
        for i = 0 to array.Length - 1 do 
            if array.[i] < bigelement then
                secondarray.[k] <- i
                k <- k + 1
        secondarray 

    let Fourthtask (array: int array)  felement selement =
        if felement > selement
        then failwith "left can be lesser than right try again"
        let mutable k = 0
        for i = 0 to array.Length - 1 do 
            if (array.[i] > selement) then 
                k <- k + 1
            if (array.[i] < felement) then
                k <- k + 1
        let thirdarray = Array.zeroCreate k
        k <- 0
        for i = 0 to thirdarray.Length - 1 do 
           if (array.[i] > selement) then
               thirdarray.[k] <- i
           if (array.[i] < felement) then  
               thirdarray.[k] <- i
           k <- k + 1     
        thirdarray   

    let Fifthtask (array: int array) =
        array.[0] <- array.[0] + array.[1]
        array.[1] <- array.[0] - array.[1]
        array.[0] <- array.[0] - array.[1]
        array

    let Sixthtask (array: int array) z v = 
        array.[z] <- array.[z] + array.[v]
        array.[v] <- array.[z] - array.[v]
        array.[z] <- array.[z] - array.[v]
        array



            
            
        


        
             

            
