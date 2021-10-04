module AutosAlgTest

open NUnit.Framework
open FsUnit
open Parking

[<Test>]
let ``Simple test`` () =
    let park = Parking(1, 1)
    park.AutoIn(0) |> ignore
    park.AutoIn(0) |> should equal NotSuccess
