module Virus

open OS

type Virus() =
    member val WinWin = 0.9 with get, set
    member val WinLin = 0.5 with get, set
    member val WinMac = 0.3 with get, set
    member val LinLin = 0.6 with get, set
    member val LinWin = 0.5 with get, set
    member val LinMac = 0.4 with get, set
    member val MacMac = 0.2 with get, set
    member val MacLin = 0.3 with get, set
    member val MacWin = 0.1 with get, set

    member this.Check = function
        | (Windows, Windows) -> this.WinWin
        | (Windows, Linux) -> this.WinLin
        | (Windows, Mac) -> this.WinMac
        | (Linux, Linux) -> this.LinLin
        | (Linux, Windows) -> this.LinWin
        | (Linux, Mac) -> this.LinMac
        | (Mac, Mac) -> this.MacMac
        | (Mac, Linux) -> this.MacLin
        | (Mac, Windows) -> this.MacWin

