module PointTree

open System

let multiplyByNumber n l = List.map (fun y -> y * n) l

let multiplyByNumber'1 n = List.map (fun y -> y * n)

let multilplyByNumber'2 n = List.map ((*) n)

let multiplyNyNumber'3 () = (*) >> List.map