module ExpressionTest

open ExpressionCalculator
open NUnit.Framework
open FsUnit

type ExpressionCalculatorTests () =
    static member SimpleExpressions = [|
        Number 30, 30
        Addition(Number 1, Number 2), 3
        Subtraction(Number 30, Number 29), 1
        Multiplication(Number 7, Number 5), 35
        Division(Number 30, Number 10), 3
    |]

    [<TestCaseSource("SimpleExpressions")>]
    [<Test>]
    member this.``Simple cases`` (testCase) =
        let expression, rightResult = testCase
        calculate expression |> should equal rightResult