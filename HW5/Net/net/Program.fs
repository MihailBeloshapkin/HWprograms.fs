module Net

open System
open System.Collections.Generic
open Os
open Virus
open PC


/// Net emulation.
type Net (edges : list<PC * PC>, infectedPC : PC, virus : Virus) =
    let mutable infected : list<PC> = []
    // let mutable justInfected : list<PC> = []
    let allVertexes =  (List.map fst edges) @ (List.map snd edges) |> List.fold (fun acc x -> if (List.contains x acc) |> not then x :: acc else acc) []

    let random = Random()

    member private this.CheckNulls () =
        let notInfected = allVertexes |> List.filter (fun x -> infected |> List.contains x |> not)
        edges |> List.filter (fun x -> virus.Variety((fst x).OS, (snd x).OS) > 0.0)

    /// Check that net doesn't contain any isolated components.
    member this.CountOfAvailble () =
        let rec sub visited i =
            let notVisited = allVertexes |> List.filter (fun x -> visited |> List.contains x |> not)
            let newVisited = edges |> List.filter (fun x -> List.contains (x |> fst) notVisited && List.contains (x |> snd) visited
                                                            || (List.contains (x |> snd) notVisited && List.contains (x |> fst) visited))
                                   |>  List.map (fun x -> if List.contains (fst x) notVisited then fst x else snd x) 
            match newVisited with
            | [] -> i
            | _ ->  sub (newVisited @ visited) (i + 1)
        sub [List.head allVertexes] 0            

    /// One step of infection.
    member this.Step () =
        infected @ List.fold (fun acc x ->  let variety = random.NextDouble()
                                            let adjackted = edges |> List.filter (fun e -> e |> fst = x || e |> snd = x)
                                                                  |> List.map (fun a -> if a |> fst = x then a |> snd else a |> fst)
                                                                  |> List.filter (fun a -> infected |> List.contains a |> not)
                                            acc @ adjackted |> List.filter (fun y -> (1.0 - virus.Variety(x.OS, y.OS)) <= variety)) [] infected
        

    /// Process.
    member this.Process () =
        let info = this.CountOfAvailble()
    ///    if this.CheckConnection() |> snd |> not then failwith "Net contains isolated components!"
        infected <- [infectedPC]
        
        let rec sub iter =
            match infected.Length = info with
            | true -> ()
            | _ -> infected <- this.Step()
                   printfn "Step %d" iter
                   infected |> List.map (fun x -> printfn "%s" x.Id) |> ignore
                   sub (iter + 1)
        sub 0
        

 
let variety pair = 
    match pair with
    | (Windows, Windows) -> 0.9
    | (Windows, Linux) -> 0.4
    | (Windows, MacOS) -> 1.0
    | (Linux, Linux) -> 0.2
    | (Linux, MacOS) ->  0.7
    | (Linux, Windows) -> 0.3
    | (MacOS, MacOS) -> 0.5
    | (MacOS, Linux) -> 0.9
    | (MacOS, Windows) -> 0.7

let v = Virus(variety)
let pc1 = PC("1", MacOS)
let pc2 = PC("2", Linux)
let pc3 = PC("3", Windows)
let pc4 = PC("4", Linux)
let pc5 = PC("5", MacOS)
let n = Net([(pc1, pc2); (pc1, pc3); (pc1, pc4); (pc4, pc5)], pc1, v)
n.Process() |> ignore
()
