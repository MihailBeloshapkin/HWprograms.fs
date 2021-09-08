module PointTreeTests

open NUnit.Framework
open FsUnit
open PointTree
open FsCheck

[<Test>]
let ``Point free test`` () =
    (((fun x -> x * 2), [1; 2; 5]) ||> f'1) |> should equal [4; 10]

[<Test>]
let ``Another point tree test`` () =
    Check.QuickThrowOnFailure(fun g l-> if l |> List.isEmpty |> not then (f'1 g l) = (f g l) else true)
