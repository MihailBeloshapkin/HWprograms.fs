// Calculates  
let fibonacci n = 
    if (n < 0) then raise(System.ArgumentOutOfRangeException())
    let rec loop first second iterator =
        if (iterator = 0) then first
        else loop second (first + second) (iterator - 1)
    loop 0 1 n

printfn "%A" (fibonacci 7)
