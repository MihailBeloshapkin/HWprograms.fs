module FibonacciTest

open NUnit.Framework
open FsUnit
open fibonacci

[<SetUp>]
let Setup () =
    ()

[<Test>]
let Test1 () =
    fibonacci |> should equal 4613732
