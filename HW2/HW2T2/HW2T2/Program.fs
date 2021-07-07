module MapForTree

type Tree<'a> =
    | Element of 'a * Tree<'a> * Tree<'a>
    | Empty

// Map for binary trees.
let rec mapForTree func tree =
    match tree with
    | Element(data, less, more) -> Element(func data, mapForTree func less, mapForTree func more)
    | Empty -> Empty