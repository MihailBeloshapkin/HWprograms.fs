module PointTreeTests

open NUnit.Framework
open PointTree
open FsCheck

[<Test>]
let ``Simple test`` () =
    Check.QuickThrowOnFailure(fun n l -> (n, l) ||> multiplyByNumber = ((n, l) ||> multiplyNyNumber'3()))
