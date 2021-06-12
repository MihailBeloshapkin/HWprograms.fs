// Learn more about F# at http://fsharp.org

open System
open System.Collections.Generic

type Tree<'a> =
    | Element of 'a * Tree<'a> * Tree<'a>
    | Empty

type Data () =
    let mutable tree : Tree<int> = Tree.Empty

    member this.AddData (newData : int) =
        let rec adding (tree : Tree<int>, newData : int) =
            match tree with
            | Element(data, Empty, more) when newData < data -> Element(data, Tree.Element(newData, Empty, Empty), more)
            | Element(data, less, more) when newData > data -> adding (more, newData)
            | Element(data, less, more) when newData < data -> adding (less, newData)
            | Element(data, less, more) when newData = data -> Element(data, less, more)
            | Empty -> Element(newData, Empty, Empty)
        tree <- adding (tree, newData)
            

    member this.GetData () =
        1


let set = new Data()

set.AddData 5
set.AddData 7
    