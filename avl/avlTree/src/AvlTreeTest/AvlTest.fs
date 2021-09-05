module AvlTreeTest

open NUnit.Framework
open FsUnit
open AvlTree
open System
open System.Collections

[<Test>]
let ``Simple addition test`` () =
    let sample = AvlTree.BinTree()
    sample.Add(30)
    sample.Contains(30) |> should equal true

[<Test>]
let ``Simple deletion test`` () =
    let sample = AvlTree.BinTree()
    sample.Add(30)
    sample.Add(7)
    sample.Delete(7)
    sample.Contains(7) |> should equal false


[<Test>]
let ``Check that tree is BST`` () =
    let sample = BinTree()
    for i in 1..10 do sample.Add(i)
    sample.CheckThatBST() |> should equal true

[<Test>]
let ``Empty tree move test`` () =
    let sample = AvlTree.BinTree()
    (sample :> IEnumerator).MoveNext() |> should equal false

[<Test>]
let ``Simple move test`` () =
    let sample = AvlTree.BinTree()
    sample.Add(1)
    sample.Add(2)
    let enumer = sample :> IEnumerator
    enumer.MoveNext() |> ignore
    enumer.MoveNext() |> ignore
    enumer.Current |> should equal 2
 

[<Test>]
let ``Check that balanced`` () =
    let sample = BinTree()
    for i in 1 .. 10 do sample.Add(i)
    sample.CheckThatBalanced() |> should equal true

[<Test>]
let ``Reset test`` () =
    let sample = BinTree()
    for i in 1 .. 30 do sample.Add(i)
    let enumer = sample :> IEnumerator
    for i in 1 .. 15 do enumer.MoveNext() |> ignore
    enumer.Reset()
    enumer.Current |> should equal 16

[<Test>]
let ``Check balance algorithm with huge trees`` () =
    let sample = BinTree()
    [1 .. 500] |> List.map (fun x -> if x % 2 = 0 then sample.Add(x) else sample.Add(-x)) |> ignore
    sample.CheckThatBalanced() |> should equal true

[<Test>]
let ``Check that balanced after deletion`` () =
    let sample = BinTree()
    for i in 1 .. 500 do sample.Add(i)
    [1 .. 500] |> List.map (fun x -> if x % 2 = 0 then sample.Delete(x)) |> ignore
    sample.CheckThatBalanced() |> should equal true

                                          
                                               