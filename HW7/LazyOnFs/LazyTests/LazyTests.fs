module LazyTests


open FsUnit
open NUnit.Framework
open Factory
open System.Threading

[<Test>]
let ``Simple lazy test`` () =
    let sample = LazyFactory.CreateSimpleLazy(fun () -> 1 + 1)
    sample.Get() |> should equal 2

[<Test>]
let ``Parallel lazy test`` () =
    let sample = LazyFactory.CreateParallelLazy(fun () -> 1 + 1)
    sample.Get() |> should equal 2

[<Test>]
let ``LockFreeLazy test`` () =
    let sample = LazyFactory.CreateLockFreeLazy(fun () -> 1 + 1)
    sample.Get() |> should equal 2
