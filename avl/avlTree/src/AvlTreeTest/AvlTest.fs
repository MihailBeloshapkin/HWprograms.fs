module AvlTreeTest

open NUnit.Framework
open FsUnit
open AvlTree
open System
open System.Collections
open System.Collections.Generic

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
let ``Check that tree is bst after deletions.`` () =
    let sample = BinTree()
    [1 .. 30] |> List.map (fun x -> sample.Add(x)) |> ignore
    [1 .. 10] |> List.map (fun x -> if x % 2 = 0 then sample.Delete(x) else ()) |> ignore
    sample.CheckThatBST() |> should equal true

[<Test>]
let ``Check that balanced`` () =
    let sample = BinTree()
    for i in 1 .. 10 do sample.Add(i)
    sample.CheckThatBalanced() |> should equal true


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


[<Test>]
let ``Check that huge tree is balanced after deletions`` () =
    let sample = BinTree()
    [1 .. 1000] |> List.map (fun x -> sample.Add(x)) |> ignore
    [1 .. 500] |> List.map (fun x -> if x % 2 = 0 then sample.Delete(x) else ()) |> ignore
    sample.CheckThatBalanced() |> should equal true


[<Test>]
let ``Test enumerator`` () =
    let sample = BinTree()
    for i in 1 .. 10 do sample.Add(i)
    let mutable enum = (sample :> IEnumerable).GetEnumerator()
    enum.MoveNext() |> ignore
    enum.Current |> should equal 4

[<Test>]
let ``Create two enumerators`` () =
    let sample = BinTree()
    for i in 1 .. 10 do sample.Add(i)
    let mutable enum1 = (sample :> IEnumerable).GetEnumerator()
    let mutable enum2 = (sample :> IEnumerable).GetEnumerator()
    for i in 1 .. 3 do enum1.MoveNext() |> ignore
    for i in 1 .. 4 do enum2.MoveNext() |> ignore
    (enum1.Current, enum2.Current) |> should equal (1, 3)            

[<Test>]
let ``Check that it is possible to use for`` () =
    let sample = BinTree()
    for i in 1 .. 10 do sample.Add(i)
    let enum = (sample :> IEnumerable).GetEnumerator()
    let data = List<obj>()
    for item in sample do data.Add(item)
    data.Count |> should equal 10        