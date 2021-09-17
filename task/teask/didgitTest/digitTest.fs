module didgitTest

open NUnit.Framework
open FsUnit
open sumOfDigits

type digitTests () =
    static member Samples = [|
        124, 7
        1, 1
        100, 1
    |]

    [<TestCaseSource("Samples")>]
    [<Test>]
    member this.``Simple cases`` (testCase) =
        let number, rightResult = testCase
        digit number |> should equal rightResult    