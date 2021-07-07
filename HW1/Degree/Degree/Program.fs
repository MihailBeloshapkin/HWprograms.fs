open System

// Get degree of number.
let getDegree number degree =
    let rec loop n acc count =
        if (count = 0) then acc
        else loop n (acc * n) (count - 1)
    loop number 1 degree

// Get list.
let getListOfDegree n m =
    if (m < 0 || n < 0 || n > m) then raise(System.ArgumentException("Incorrect input"))
    let firstNumber = getDegree 2 n
    let rec loop acc count =
        if count = 0 then acc
        else loop ((acc.Head * 2) :: acc) (count - 1)
    loop [firstNumber] (m - n) |> List.rev

printfn "%A" (getListOfDegree 2 5)





        