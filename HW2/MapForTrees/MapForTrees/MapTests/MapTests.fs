module MapTests

open NUnit.Framework
open FsUnit
open MapForTree

[<Test>]
let ``Simple map test`` () =
    let tree = Element(5, Element(3, Empty, Empty), Element(7, Empty, Empty))
    tree |> mapForTree (fun x -> x * 2) |> should equal (Element(10, Element(6, Empty, Empty), Element(14, Empty, Empty)))

[<Test>]
let ``Leaf test`` () =
    Element(7, Empty, Empty) |> mapForTree (fun x -> x * 2) |> should equal (Element(14, Empty, Empty)) 

[<Test>]
let ``Bigger tree map test`` () =
    let tree = Element(30, Element(20, Element(3, Empty, Empty), Element(27, Empty, Empty)), Element(15, Empty, Empty))
    tree |> mapForTree (fun x -> x + 1) |> should equal (Element(31, Element(21, Element(4, Empty, Empty), Element(28, Empty, Empty)), Element(16, Empty, Empty)))

[<Test>]
let ``Empty tree map test`` () =
    (Empty |> mapForTree (fun x -> x + 1)) = Empty |> should equal true