// Calculates factorial 
let rec factorial n =
    match n with
    | 0 | 1 -> 1
    | _ when n > 0 -> n * factorial(n - 1)
    | _ when n < 0 -> -1

printfn "%d" (factorial -3)