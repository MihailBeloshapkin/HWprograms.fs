module sumOfDigits

open System

let digit number =
    let rec loop number acc =
        match number with
        | 0 -> acc
        | _ -> loop (number / 10) (acc + number % 10)
    loop number 0

digit 123


