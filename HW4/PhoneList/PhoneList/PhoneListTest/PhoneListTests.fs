module PhoneListTest

open System.Collections.Generic
open NUnit.Framework
open FsUnit
open PhoneList

[<Test>]
let ``Simple additioon in empty list test`` () =
    [] |> addData "A" "30" |> should equal [("A", "30")]

[<Test>]
let ``Simple search by name test`` () =
    [] |> addData "A" "30" |> searchPhoneByName "A" |> should equal "30"

[<Test>]
let ``Simple search by phone test`` () =
    [] |> addData "A" "30" |> searchNameByPhone "30" |> should equal "A"

[<Test>]
let ``Simple get data from file test`` () =
    "../../../../testData/data.txt" |> getDataFromFile |> should equal [("A", "30")]

[<Test>]
let ``Try to search incorrect name`` () =
     (fun () -> [] |> addData "A" "30" |> searchPhoneByName "B" |> ignore) |> should throw typeof<KeyNotFoundException> 

[<Test>]
let ``Try to search incorrect phone`` () =
    (fun () -> [] |> addData "A" "30" |> searchNameByPhone "10" |> ignore) |> should throw typeof<KeyNotFoundException>

