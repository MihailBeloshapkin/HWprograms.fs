module PC
open Os

/// PC.
type PC (id : string, os : OS) =
    /// Unique id of PC.
    member val Id = id with get

    /// OS Type.
    member val OS = os with get
