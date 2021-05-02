module BracketsTests

open NUnit.Framework
open FsCheck
open FsUnit
open Brackets

type BracketsTests () =
    static member ExamplesSuccess = [|
        "{}"
        "[()a]"
        "()[b][]e{()}"
        "(a{b}[c(d)w])"
    |]

    static member ExamplesFailed = [|
        "(a"
        "d][]"
        "n([)]"
    |]

    [<TestCaseSource("ExamplesSuccess")>]
    [<Test>]
    member this.``Simple test with right brackets`` (testCase) =
        let brackets = testCase
        Brackets.bracketChecker brackets |> should equal true

    [<TestCaseSource("ExamplesFailed")>]
    [<Test>]
    member this.``Simple test with wrong brackets`` (testCase) = 
        Brackets.bracketChecker testCase |> should equal false