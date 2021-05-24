module PC

open System
open OS
open Virus

type PC(os : OS, id : string) = 
    member val OS = os with get
    member val IsInfected = false with get, set
    member val AdjactedPCs : List<PC> = [] with get, set
    member val JustInfected = false with get, set
    member val Id = id with get

    member this.tryInfect(virus : Virus, random : Random) = 
//        if this.IsInfected |> not then
//            this.IsInfected <- 




