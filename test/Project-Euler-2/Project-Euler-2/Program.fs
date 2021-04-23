module fibonacci

open System

let fibonacci = 
    let fibonacciList = []
    let rec loop fibonacciList first second = 
        if (first + second > 4000000) then fibonacciList
        else loop (fibonacciList @ [first + second]) second (first + second)
  //  List.filter (fun x -> x % 2 = 0) (loop fibonacciList 0 1)
    List.fold (fun acc i -> acc + i) 0 (List.filter (fun x -> x % 2 = 0) (loop fibonacciList 0 1))  

let listOfNumbers = fibonacci

printfn "%A" listOfNumbers