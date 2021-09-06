module SearcherTests

open System
open NUnit.Framework
open FsUnit
open Searcher

[<Test>]
let ``Simple search test`` () =
    searcher 2 [1; 2; 3] |> should equal 1

[<Test>]
let ``Empty list search test`` () =
    (fun () -> searcher 30 [] |> ignore) |> should (throwWithMessage "List does not contain this value") typeof<System.Exception> 

[<Test>]
let ``Single element list test`` () =
    searcher 30 [30] |> should equal 0


[<Test>]
let ``List doesn't contain value`` () =
    (fun () -> searcher 30 [1; 2; 3; 4] |> ignore) |> should (throwWithMessage "List does not contain this value") typeof<System.Exception>