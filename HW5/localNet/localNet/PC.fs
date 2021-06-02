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
        if this.IsInfected |> not then
            this.IsInfected <- this.AdjactedPCs |> List.exists (fun (adjacent : PC) ->
            (adjacent.IsInfected && (adjacent.JustInfected |> not) 
            && (random.NextDouble() < virus.Check(this.OS, adjacent.OS)))
            )
            if this.IsInfected then this.JustInfected <- true

    member this.canBeInfected(virus : Virus) = 
        this.IsInfected && List.exists (fun (adjacent : PC) -> (adjacent.IsInfected |> not) && 
        virus.Check(this.OS, adjacent.OS) > 0.0) this.AdjactedPCs