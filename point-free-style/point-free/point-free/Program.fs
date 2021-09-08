module PointTree

open System

/// Original.
let f g l = List.map g (List.tail l)

/// Without list argument.
let f'1 g = List.tail >> List.map g

/// Point free.
let f'2 = ((>>) List.tail) << List.map

printfn "%A" (f'2 (fun x -> x * 2) [0])


