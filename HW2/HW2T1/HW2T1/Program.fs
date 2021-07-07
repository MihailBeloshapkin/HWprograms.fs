namespace HW2T1

module functions =
    
    // This function calculates number of even numbers using Map function.
    let calculateEvenCountWithMap list = 
        List.map (fun n -> if n % 2 = 0 then 1 else 0) list |> List.sum

    // This function calculates number of even numbers using Filter function.
    let calculateEvenCountWithFilter list =
        List.filter (fun n -> n % 2 = 0) list |> List.length

    // This function calculates number of even numbers using Fold function.
    let calculateEvenCountWithFold list =
        List.fold (fun acc n -> if n % 2 = 0 then acc + 1 else acc) 0 list

    printfn "%d" (calculateEvenCountWithMap [0; 1; 2; 3; 4])