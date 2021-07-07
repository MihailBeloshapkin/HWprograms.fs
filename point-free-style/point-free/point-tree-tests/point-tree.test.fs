module point_tree_tests

open NUnit.Framework
open FsUnit
open pointTree


[<Test>]
let ``Point free test`` () =
    (((fun x -> x*2), [1; 2; 5]) ||> f'1) |> should equal [4; 10]
