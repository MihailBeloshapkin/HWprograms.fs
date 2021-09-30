module LambdaInterTests

open System
open NUnit.Framework
open FsUnit
open LambdaInterpreter


type LambdaInterTest() =
    static member testTerms() =
        let x = Guid.NewGuid()
        let y = Guid.NewGuid()
        let I x = Abstraction(x, Var x)
        let K x y = Abstraction(x, Abstraction(y, Var x))
        let K_s x y = Abstraction(x, Abstraction(y, Var y))
        [|
            Application(I x, I x), I x
            Application(I x, Application(I x, I x)), I x
            Application(K x y, I x), K_s y x
            Application(K_s x y, Application(I x, I x)), I y
        |]

     [<TestCaseSource("testTerms")>]
     member this.``Simple tests`` (case : Lambda<Guid> * Lambda<Guid>) =
         case |> fst |> reduce |> should equal (case |> snd)



