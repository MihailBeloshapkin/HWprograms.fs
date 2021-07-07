module HW2T1Tests

open NUnit.Framework
open HW2T1.functions
open FsCheck
open FsUnit


// This function checks results are equal.
let checkThatFunctionsAreEqual (list: list<int>) =
    calculateEvenCountWithMap list = calculateEvenCountWithFilter list &&
    calculateEvenCountWithFilter list = calculateEvenCountWithFold list

[<Test>]
let checkThatFunctionsAreEqualTest () =
    Check.QuickThrowOnFailure checkThatFunctionsAreEqual

[<Test>]
let calculateEvenCountWithMapTest () =
    calculateEvenCountWithMap [0; 1; 2; 3; 4] |> should equal 3
    calculateEvenCountWithMap [] |> should equal 0

[<Test>]
let calculateEvenCountWithFilter () = 
    calculateEvenCountWithFilter [0; 1; 2; 3; 4] |> should equal 3
    calculateEvenCountWithFilter [] |> should equal 0

[<Test>]
let calculateEvenCountWithFold () = 
    calculateEvenCountWithFold [0; 1; 2; 3; 4] |> should equal 3
    calculateEvenCountWithFold [] |> should equal 0
