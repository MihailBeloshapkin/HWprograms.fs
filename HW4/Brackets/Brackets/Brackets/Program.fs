open System.Collections.Generic

let isBracket char = 
    match char with
    | '(' -> true
    | ')' -> true
    | '[' -> true
    | ']' -> true
    | '{' -> true
    | '}' -> true
    | _ -> false

let getBrackets str = 
    let rec loop string bracketList = 
        match string with
        | head :: tail when isBracket head -> loop tail (bracketList @ [head])
        | head :: tail -> loop tail bracketList
        | [] -> bracketList
    let list = Seq.toList str
    loop list []

let bracketChecker str = 
    let brackets = [('(', ')'); ('[', ']'); ('{', '}')]
    let rec loop (str : list<char>) bracketStack =
        match str with
        | head :: tail when brackets |> List.exists (fun x -> fst x = head) ->
            loop tail (head :: bracketStack)
        | head :: tail when brackets |> List.exists (fun x -> snd x = head) ->
            match head with
            | _ when brackets |> List.findIndex (fun x -> snd x = head) <> (brackets |> List.findIndex (fun x -> snd x = bracketStack.Head)) 
                -> false
            | _ -> loop str.Tail bracketStack.Tail
        | [] -> true
    let input = Seq.toList str |> getBrackets
    loop input []


let str = "{}[][r]ew["

printfn "%A" (getBrackets str)