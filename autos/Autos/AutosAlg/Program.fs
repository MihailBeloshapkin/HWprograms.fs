module Parking

open System
open System.Threading

type Responce =  Success | NotSuccess | SomethingIsWrong

/// Machine that controls count of cars in the parking.
type Machine (sizeOfParking : int, occupied : int ref) =
    /// When car is trying to get into the parking lot.
    member this.In () = match !occupied < sizeOfParking with
                        | true ->  Interlocked.Increment(occupied) |> ignore
                                   Success
                        | false -> NotSuccess

    /// When car is leaving the parking lot.
    member this.Out () = if !occupied > 0 then Interlocked.Decrement(occupied) |> ignore else ()

type Parking (sizeOfPark : int, countOfMachines : int) =
    let occupied = ref 0

    /// Machines that control parking.
    member private this.Machines = [for i in 0 .. countOfMachines do Machine(sizeOfPark, occupied)]

    /// Auto In member.
    member this.AutoIn (number : int) = if number > -1 && number < this.Machines.Length then this.Machines.[number].In() else SomethingIsWrong

    ///  Auto Out member.
    member this.AutoOut (number : int) = if number > -1 && number < this.Machines.Length then this.Machines.[number].Out() else ()


let park = Parking(5, 2)
park.AutoIn(1)
park.AutoIn(2)