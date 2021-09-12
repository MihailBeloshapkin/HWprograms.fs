module netTests

open NUnit.Framework
open FsUnit
open Net
open Virus
open PC
open Os

let variety pair = 
    match pair with
    | (Windows, Windows) -> 0.9
    | (Windows, Linux) -> 0.4
    | (Windows, MacOS) -> 1.0
    | (Linux, Linux) -> 0.2
    | (Linux, MacOS) ->  0.7
    | (Linux, Windows) -> 0.3
    | (MacOS, MacOS) -> 0.5
    | (MacOS, Linux) -> 0.9
    | (MacOS, Windows) -> 0.7


[<Test>]
let ``Simple net test`` () =
    let v = Virus(variety)
    let pc1 = PC("1", MacOS)
    let pc2 = PC("2", Linux)
    let pc3 = PC("3", Windows)
    let pc4 = PC("4", Linux)
    let n = Net([(pc3, pc1); (pc1, pc2); (pc2, pc4)], pc3, v)
    Assert.Pass()