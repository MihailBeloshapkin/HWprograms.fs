module Virus
open Os

/// Virus.
type Virus (variety) =
    member this.Variety : OS * OS -> float = variety