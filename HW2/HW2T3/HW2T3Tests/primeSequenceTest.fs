module HW2T3Tests

open NUnit.Framework
open FsUnit
open HW2T3

[<Test>]
let checkThatSequenceIsCorrecttest () =
    let rightAnswer = seq { 2; 3; 5; 7; 11; 13; 17 }
    primeSequence.primes () |> Seq.take 7 |> should equal rightAnswer
