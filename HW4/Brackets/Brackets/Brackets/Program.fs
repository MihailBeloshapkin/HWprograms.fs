module Brackets

let brackets = Map.ofList [('(', ')'); ('[', ']'); ('{', '}')]


/// Checks that char is bracket.
let isBracket char = 
    match char with
    | _ when brackets.ContainsKey(char) || brackets |> Map.toList |> List.map (fun x -> x |> snd) |> List.contains char -> true
    | _ -> false

/// Get list of brackets from the string.
let getBrackets str = 
    let rec loop string bracketList = 
        match string with
        | head :: tail when isBracket head -> loop tail (head :: bracketList)
        | head :: tail -> loop tail bracketList
        | [] -> bracketList |> List.rev
    let list = Seq.toList str
    loop list []

/// Checks that str is balanced.
let bracketChecker str = 
    let rec loop (str : list<char>) bracketStack =
        match str with
        | head :: tail when brackets.ContainsKey(head) ->
            loop tail (head :: bracketStack)
        | head :: tail -> match bracketStack with
                          | h :: t when brackets.TryGetValue(h) = (true, head) -> loop tail t
                          | _ -> false   
        | [] -> bracketStack.IsEmpty
    let input = Seq.toList str |> getBrackets
    loop input []
