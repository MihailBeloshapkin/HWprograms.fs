module ParallelLazy

open ILazy
open System

type ParallelLazy<'a> (supplier : unit -> 'a) =
    let locker = new Object()
    let mutable result = None

    let calculate =
        if result.IsNone then
            result <- Some(supplier())

    interface ILazy<'a> with
        member this.Get() =
            if result.IsNone |> not then lock locker (fun () -> calculate)
            result.Value
