module FilterForTree

type Tree<'a> =
    | Element of 'a * Tree<'a> * Tree<'a>
    | Empty

// Map for binary trees.
let filterForTree func tree =
    let rec filtering func tree list =
        match tree with
        | Element(data, less, more) when func data -> let l1 = filtering func less list
                                                      let l2 = filtering func more list
                                                      data :: List.concat [l1; l2]                         
        | Empty -> list
        | Element(data, less, more) when not (func data) -> filtering func less list
    filtering func tree []


let sample:Tree<int> = Element(2, (Element(4, Empty, Empty)), (Element(3, Empty, Empty)))

filterForTree (fun x -> x % 2 = 0) sample
