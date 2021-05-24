module WorkFlowTests

open System
open Workflow
open NUnit.Framework
open FsUnit

[<Test>]
let ``Simple test`` () =
    Computing 3 {
        let! a = 2.0 / 12.0
        let! b = 3.5
        return a / b
    } |> should (equalWithin 0.0001) 0.048

[<Test>]
let ``Incorrect Input test`` () =
    (fun () -> Computing(-3) |> ignore) |> should throw typeof<System.ArgumentException>