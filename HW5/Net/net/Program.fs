// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open System.Collections.Generic

/// OS.
type OS =
    | Windows
    | Linux
    | MacOS

/// PC.
type PC (id : string, os : OS) =
    member val Id = id with get
    member val OS = os with get


/// Virus.
type Virus () =
    member this.Variety pair = 
        match pair with
        | (Windows, Windows) -> 0.9
        | (Windows, Linux) -> 0.4
        | (Windows, MacOS) -> 1.0
        | (Linux, Linux) -> 0.2
        | (Linux, MacOS) ->  0.1
        | (Linux, Windows) -> 0.3
        | (MacOS, MacOS) -> 0.5
        | (MacOS, Linux) -> 0.1
        | (MacOS, Windows) -> 0.7

/// Net emulation.
type Net (edges : list<PC * PC>, infectedPC : PC, virus : Virus) =
    let mutable infected : list<PC> = []
    // let mutable justInfected : list<PC> = []
    let allVertexes =  (List.map fst edges) @ (List.map snd edges) |> List.fold (fun acc x -> if (List.contains x acc) |> not then x :: acc else acc) []

    let random = Random()

    /// One step.
    member this.Step () =
        if List.isEmpty infected then infected <- [infectedPC] else ()
        let mutable justInfected : list<PC> = []
        infected |> List.map (fun x ->  let variety = random.NextDouble()
                                        let adjackted = edges |> List.filter (fun e -> e |> fst = x || e |> snd = x)
                                                              |> List.map (fun a -> if a |> fst = x then a |> snd else a |> fst)
                                                              |> List.filter (fun a -> infected |> List.contains a |> not)
                                        justInfected <- justInfected @ adjackted |> List.filter (fun y -> (1.0 - virus.Variety(x.OS, y.OS)) <= variety)) |> ignore
        infected <- infected @ justInfected

    /// Process.
    member this.Process () =
        let rec sub iter =
            match infected.Length = allVertexes.Length with
            | true -> ()
            | _ -> this.Step()
                   printfn "Step %d" iter
                   infected |> List.map (fun x -> printfn "%s" x.Id) |> ignore
                   sub (iter + 1)
        sub 0
        

 
let v = Virus()
let pc1 = PC("1", MacOS)
let pc2 = PC("2", Linux)
let pc3 = PC("3", Windows)
let pc4 = PC("4", Linux)
let n = Net([(pc3, pc1); (pc1, pc2); (pc2, pc4)], pc3, v)
n.Process() |> ignore
()
