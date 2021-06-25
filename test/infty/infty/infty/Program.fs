module inftyNumbers

open System

// Generates seq of n elements
let localSeq n =
    let rec loop countdowm seq =
        match countdowm with
        | 0 -> seq
        | _ -> loop (countdowm - 1) (Seq.append seq [n])
    loop n Seq.empty

// Generates inty seq
let inftySeq () =
    Seq.initInfinite localSeq 
    |> Seq.concat

let data = inftySeq () |> Seq.take 6

printfn "%A" data







