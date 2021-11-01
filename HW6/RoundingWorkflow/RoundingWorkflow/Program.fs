module Workflow

open System

/// <summary>
/// Computing with corresponding accuracy.
/// </summary>
type Computing(accuracy : int) = 
    do if accuracy < 0 then raise (System.ArgumentException("Incorrect accuracy value"))
    member this.Bind (x : float, f) = 
        f (Math.Round(x, accuracy))

    member this.Return(x : float) =
        Math.Round(x, accuracy)