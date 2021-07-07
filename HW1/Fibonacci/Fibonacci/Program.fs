open System

// Get fibonacci number.
let fibonacci n =
    if n < 0 then raise(System.ArgumentException("Number should be more than 0"))
    let rec loop first second count =
        match count with
        | 0 -> first
        | _ -> loop second (first + second) (count - 1)
    loop 0 1 n

printfn "%A" (fibonacci 30)

