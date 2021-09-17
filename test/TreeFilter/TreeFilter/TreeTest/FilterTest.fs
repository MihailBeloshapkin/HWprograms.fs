module TreeTest

open FilterForTree
open NUnit.Framework
open FsUnit


type filterTests () =
    static member Samples = [|
        (fun x -> x % 2 = 0), Element(2, (Element(4, Tree.Empty, Tree.Empty)), (Element(3, Tree.Empty, Tree.Empty))), [2; 4]
        (fun x -> x % 3 = 0), Element(3, (Element(9, Tree.Empty, Tree.Empty)), (Element(12, Tree.Empty, Tree.Empty))), [3; 9; 12]
     |]


    [<TestCaseSource("Samples")>]
    [<Test>]
    member this.``Simple test`` (testCase) =
        let funcSample, tree, result = testCase
        filterForTree funcSample tree |> should equal result
    
    