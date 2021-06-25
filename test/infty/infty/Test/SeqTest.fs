module Test

open NUnit.Framework
open inftyNumbers
open FsUnit

[<Test>]
let ``Simple test`` () =
    let right = seq {1; 2; 2; 3; 3; 3}
    inftySeq () |> Seq.take 6 |> should equal right
