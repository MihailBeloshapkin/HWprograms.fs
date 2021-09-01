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
 
let checkThatBalance (tree : AvlTree.BinTree) =
    let isBalanced t =
        match t with
        | Empty -> true
        | Node(_, _, left, right) -> let mutable lHeight = -1
                                     let mutable rHeight = -1
                                     match left with
                                     | Empty -> lHeight <- 0
                                     | Node(_, height, _, _) -> lHeight <- height
                                     match right with
                                     | Empty -> rHeight <- 0
                                     | Node(_, height, _, _) -> rHeight <- height
                                     if (Math.Abs(lHeight - rHeight) < 2) then true else false
    let rec checker (tree : Tree) =
        match tree with
        | Empty -> true
        | Node(value, height, left, right) -> if isBalanced tree then 
                                                  (checker left) && (checker right)
                                              else false
    checker tree.Tree

[<Test>]
let ``Check that balanced`` () =
    let sample = BinTree()
    for i in 1 .. 10 do
        sample.Add(i)
    sample |> checkThatBalance |> should equal true

[<Test>]
let ``Check balance algorithm with huge trees`` () =
    let sample = BinTree()
    [1 .. 500] |> List.map (fun x -> if x % 2 = 0 then sample.Add(x) else sample.Add(-x)) |> ignore
    sample |> checkThatBalance |> should equal true
                                               