module Parking

open System
open System.Threading

type Responce =
    | Success
    | NotSuccess

type Machine (sizeOfParking : int, occupied : int ref) =
    member this.In () = match !occupied < sizeOfParking with
                        | true ->  Interlocked.Increment(occupied) |> ignore
                                   Success
                        | false -> NotSuccess

    member this.Out () = if !occupied > 0 then Interlocked.Decrement(occupied) |> ignore else ()

type Parking (sizeOfPark : int, countOfMachines : int) =
    let occupied = ref 0

    member this.Machines = [for i in 0 .. countOfMachines do Machine(sizeOfPark, occupied)]



let park = Parking(5, 2)
park.Machines.[0].In()
park.Machines.[1].In()