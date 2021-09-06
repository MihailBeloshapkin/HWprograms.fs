module MapForTree

/// <summary>
/// Simple binary tree.
/// </summary>
type Tree<'a> =
    | Element of 'a * Tree<'a> * Tree<'a>
    | Empty


/// <summary>
/// Map function for binary trees.
/// </summary>
let rec mapForTree func tree =
    match tree with
    | Element(data, less, more) -> Element(func data, mapForTree func less, mapForTree func more)
    | Empty -> Empty 

let a = Empty |> mapForTree (fun x -> x * 2)

printfn "A"