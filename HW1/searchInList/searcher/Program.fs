module Searcher
open System

/// <summary>
/// Searches num position in list.
/// </summary>
let searcher num list =
    let rec subSearcher list num pos =
        match list with
        | head :: tail -> if head = num then pos else subSearcher tail num (pos + 1)
        | [] -> failwith "List does not contain this value"
    subSearcher list num 0

