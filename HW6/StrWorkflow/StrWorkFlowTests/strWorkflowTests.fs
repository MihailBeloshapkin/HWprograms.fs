module StrWorkFlowTests

open System
open StrWorkFlow
open NUnit.Framework
open FsUnit

let calculate = StrWorkFlow()

[<Test>]
let ``Simple test`` () =
    calculate {
        let! a = "5"
        let! b = "7"
        let c = a + b
        return c
    } |> should equal <| Some(12)

[<Test>]
let ``Incorrect input test`` () =
    calculate {
        let! a = "x"
        let! b = "y"
        let c = a + b
        return c
    } |> should equal <| None
