module StrWorkFlow

open System

type StrWorkFlow () =
    member this.Bind (x : string, f) = 
        match Int32.TryParse x with
        | (true, value) -> f value
        | _ -> None

    member this.Return x =
        Some(x)