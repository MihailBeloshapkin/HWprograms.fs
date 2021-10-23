module AutosAlgTest

open NUnit.Framework
open FsUnit
open Parking
open System.Threading.Tasks
open System.Collections.Concurrent

[<Test>]
let ``Simple car entrance test`` () =
    let park = Parking(1, 1)
    park.AutoIn(0) |> should equal Success

[<Test>]
let ``Simple full parking case test`` () =
    let park = Parking(1, 1)
    park.AutoIn(0) |> ignore
    park.AutoIn(0) |> should equal NotSuccess


[<Test>]
let ``Simple exit case test`` () =
    let park = Parking(1, 1)
    park.AutoIn(0) |> ignore
    park.AutoOut(0)
    park.Occupied |> should equal 0

[<Test>]
let ``Simple parallel test`` () =
    let park = Parking(5, 5)
    Parallel.For(0, 5, (fun i -> park.AutoIn(i) |> should equal Success)) |> ignore
    park.Occupied |> should equal 5

[<Test>]
let ``Parallel entrance to a full park`` () =
    let park = Parking(5, 5)
    for i in 0 .. 4 do park.AutoIn(i) |> ignore
    Parallel.For(0, 5, (fun i -> park.AutoIn(i) |> should equal NotSuccess)) |> ignore
    
[<Test>]
let ``Parallel enterance but there are no places for everyone`` () =
    let park = Parking(2, 5)
    park.AutoIn(0) |> ignore
    let results = [0; 1] |> List.map (fun i -> async { return park.AutoIn(i) })
                         |> Async.Parallel
                         |> Async.RunSynchronously
    (results = [|Success; NotSuccess|] || results = [|NotSuccess; Success|]) |> should equal true