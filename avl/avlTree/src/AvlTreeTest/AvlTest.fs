module AvlTreeTest

open NUnit.Framework
open FsUnit
open AvlTree

[<SetUp>]
let Setup () =
    ()

[<Test>]
let ``simple addition test`` () =
    let sample = AvlTree.BinTree()
    sample.Add(30)
    sample.Contains(30) |> should equal true