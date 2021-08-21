module AvlTreeTest

open NUnit.Framework
open FsUnit
open AvlTree
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
 