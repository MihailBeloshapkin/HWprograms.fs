module MultOfDigits

open System

/// <summary>
/// Calculates multiplication of digits.
/// </summary>
let multOfDigits (x : int) =
    let rec loop num acc =
        match num with
        | 0 -> if x = 0 then 0 else acc
        | _ -> loop (num / 10) (acc * (num % 10))
    loop (Math.Abs(x)) 1


let a = -24 % 10
printfn "%A"

