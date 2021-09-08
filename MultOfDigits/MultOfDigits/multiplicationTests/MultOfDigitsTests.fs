module MultiplicationTests

open NUnit.Framework
open FsUnit
open MultOfDigits


/// <summary>
/// Tests multOfDigits.  
/// </summary>
type DigitTests () =
    static member Samples = [|
        124, 8
        1, 1
        100, 0 
        -234, 24 
        0, 0 |]

    [<TestCaseSource("Samples")>]
    [<Test>]
    member this.``Simple cases`` (testCase) =
        let number, rightResult = testCase
        multOfDigits number |> should equal rightResult 