module netTests

open System
open NUnit.Framework
open FsUnit
open Foq
open Net
open Virus
open PC
open Os

type NetTests () =

    let variety pair = 
        match pair with
        | (Windows, Windows) -> 0.9
        | (Windows, Linux) -> 0.4
        | (Windows, MacOS) -> 1.0
        | (Linux, Linux) -> 0.2
        | (Linux, MacOS) ->  0.7
        | (Linux, Windows) -> 0.3
        | (MacOS, MacOS) -> 0.4
        | (MacOS, Linux) -> 0.5
        | (MacOS, Windows) -> 0.7


    let zeroVariety =
        Mock<System.Random>()
          .Setup(fun x -> <@ x.NextDouble() @>).Returns(0.0)
          .Create()

    let oneVariety =
        Mock<System.Random>()
          .Setup(fun x -> <@ x.NextDouble() @>).Returns(1.0)
          .Create()

    [<Test>]
    member this.``Simple net test with two computers`` () =
        let v = Virus(variety)
        let pc1 = PC("1", MacOS)
        let pc2 = PC("2", Linux)
        let n = Net([(pc1, pc2)], pc1, v, oneVariety)
        n.Process() |> should equal 1
 
    [<Test>]
    member this.``Simple net test with four computers`` () =
        let v = Virus(variety)
        let pc1 = PC("1", MacOS)
        let pc2 = PC("2", Linux)
        let pc3 = PC("3", Windows)
        let pc4 = PC("4", MacOS)
        let net = new Net([(pc1, pc2); (pc1, pc3); (pc1, pc4)], pc1, v, oneVariety)
        net.Process() |> should equal 1


    [<Test>]
    member this.``Net contains isolated components`` () =
        let v = Virus(variety)
        let pc1 = PC("1", MacOS)
        let pc2 = PC("2", Windows)
        let pc3 = PC("3", Linux)
        let pc4 = PC("4", Linux)
        let net = new Net([(pc1, pc2); (pc3, pc4)], pc1, v, oneVariety)
        net.Process() |> should equal 1
