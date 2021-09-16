///<summary>
/// Reverse list.
/// </summary>
let reverseList list =
    List.fold (fun list element -> element :: list) [] list

let list = [1; 2; 3]
printfn "%A" (reverseList list)