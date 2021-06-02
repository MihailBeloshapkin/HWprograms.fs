module LocalNet

open OS
open Virus
open PC
open System

type localNet(pcSet : List<PC>, virus : Virus, random : Random) =
    member this.turn =
        pcSet |> List.iter (fun (pc : PC) -> pc.tryInfect(virus, random))
        List.iter (fun (pc :PC) -> pc.JustInfected <- false) pcSet

    member this.canChange =
        List.exists (fun (pc : PC) -> pc.canBeInfected(virus)) pcSet

    member this.run =
        if this.canChange then 
            this.turn
            let infectedStr (pc : PC) = if pc.IsInfected then "Infected" else "Not Infected"
            List.iter (fun (pc : PC) -> printfn $"{pc.Id} with OS {pc.OS} is {infectedStr pc}") pcSet
            printfn "Press\n"
            Console.ReadKey |> ignore
            this.run
        else 
            printfn "Process is finished"
