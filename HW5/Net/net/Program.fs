module Net

open System
open System.Collections.Generic
open Os
open Virus
open PC


/// Net emulation.
type Net (edges : list<PC * PC>, infectedPC : PC, virus : Virus, random : Random) =
    /// Infected vertexes.
    let mutable infected : list<PC> = []

    /// All Vertexes of the current net.    
    let allVertexes =  (List.map fst edges) @ (List.map snd edges) |> List.distinct

    /// Check that net doesn't contain any isolated components.
    member this.CountOfAvailable () =
        let rec sub visited i =
            let notVisited = allVertexes |> List.filter (fun x -> visited |> List.contains x |> not)
            let newVisited = edges |> List.filter (fun x -> List.contains (x |> fst) notVisited && List.contains (x |> snd) visited
                                                            || (List.contains (x |> snd) notVisited && List.contains (x |> fst) visited))
                                   |>  List.map (fun x -> if List.contains (fst x) notVisited then fst x else snd x) 
            match newVisited with
            | [] -> List.length visited
            | _ ->  sub (newVisited @ visited) (i + 1)
        sub [infectedPC] 0            

    /// One step of infection.
    member this.Step () =
        infected @ List.fold (fun acc x ->  let variety = random.NextDouble()
                                            let adjacent = edges |> List.filter (fun e -> e |> fst = x || e |> snd = x)
                                                                  |> List.map (fun a -> if a |> fst = x then a |> snd else a |> fst)
                                                                  |> List.filter (fun a -> infected |> List.contains a |> not)
                                            acc @ adjacent |> List.filter (fun y -> (1.0 - virus.Variety(x.OS, y.OS)) <= random.NextDouble())) [] infected
        

    /// Process.
    member this.Process () =
        let info = this.CountOfAvailable()
        infected <- [infectedPC]
        
        let rec sub iter =
            match infected.Length = info with
            | true -> iter
            | _ -> infected <- this.Step()
                   printfn "Step %d" iter
                   infected |> List.map (fun x -> printfn "%s" x.Id) |> ignore
                   sub (iter + 1)
        sub 0
        