let listReverse list =
    List.fold (fun list elem -> elem::list) [] list

let list = [1; 2 ;3]
printfn "%A" (listReverse list)