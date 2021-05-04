module PhoneListTest

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



