module Parking

open System
open System.Threading

/// Machine response.
type Response =  Success | NotSuccess | SomethingIsWrong

/// Machine that controls count of cars in the parking.
type Machine (sizeOfParking : int, occupied : int ref) =

    /// When car is trying to get into the parking lot.
    member this.In () =  let startValue = !occupied
                         match !occupied < sizeOfParking with
                         | true ->  let start = Interlocked.CompareExchange(occupied, startValue + 1, startValue)
                                    if start = startValue then Success else NotSuccess 
                         | false -> NotSuccess
                        

    /// When car is leaving the parking lot.
    member this.Out () = if !occupied > 0 then Interlocked.Decrement(occupied) |> ignore else ()

/// Manages parking process.
type Parking (sizeOfPark : int, countOfMachines : int) =
    let occupied = ref 0

    /// Number of occupied.
    member this.Occupied with get () = occupied

    /// Machines that control parking.
    member private this.Machines = [for i in 0 .. countOfMachines do Machine(sizeOfPark, occupied)]

    /// Auto In member.
    member this.AutoIn (number : int) = if number > -1 && number < this.Machines.Length then this.Machines.[number].In() else SomethingIsWrong

    ///  Auto Out member.
    member this.AutoOut (number : int) = if number > -1 && number < this.Machines.Length then this.Machines.[number].Out() else ()


// let mutable a = 1
// Interlocked.CompareExchange(&a, 5, 1) |> ignore
let park = Parking(2, 5)
park.AutoIn(0) |> ignore
let results = [0; 1] |> List.map (fun i -> async { return park.AutoIn(i) })
                     |> Async.Parallel
                     |> Async.RunSynchronously
printfn "%A" results
// printfn "%A" a