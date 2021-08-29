module lambdaInterTests

open System
open NUnit.Framework
open FsUnit
open LambdaInterpreter

[<SetUp>]
let Setup () =
    ()

[<Test>]
let ``Simple test`` () =
    let x = System.Guid.NewGuid()
    let I x = Abstraction(x, Var x)
    let sample = Application(I x, I x)
    reduce sample |> should equal (I x)

