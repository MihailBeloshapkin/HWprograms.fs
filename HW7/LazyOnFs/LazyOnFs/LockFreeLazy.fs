module LockFreeLazy

open ILazy
open System.Threading

type LockFreeLazy<'a> (supplier : unit -> 'a) =
    let mutable result = None

    interface ILazy<'a> with
        member this.Get() =
            if result.IsNone then
                let startValue = result
                let desiredValue = Some <| supplier()
                Interlocked.CompareExchange(&result, desiredValue, startValue) |> ignore
            result.Value

