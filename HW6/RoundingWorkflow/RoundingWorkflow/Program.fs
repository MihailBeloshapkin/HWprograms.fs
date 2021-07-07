module Workflow

open System

// Computing with corresponding accuracy.
type Computing(accuracy : int) = 
    do if accuracy < 0 then raise (System.ArgumentException("Incorrect accuracy value"))
    member this.Bind (x : float, f) = 
        f (Math.Round(x, accuracy))

    member this.Return(x : float) =
        Math.Round(x, accuracy)