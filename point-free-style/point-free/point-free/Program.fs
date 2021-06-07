module pointTree

open System

let f g l = List.map g (List.tail l)

let f'1 g = List.tail >> List.map g

let f'2 = List.map >> ((<<) List.tail)

printfn "%A" (f'2 (fun x -> x*2) [1; 2; 5])
