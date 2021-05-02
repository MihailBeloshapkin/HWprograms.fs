module Brackets

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
            match str.Head with
            | _ when bracketStack.Length = 0 -> false
            | _ when (brackets |> List.findIndex (fun x -> snd x = head)) <> (brackets |> List.findIndex (fun x -> fst x = bracketStack.Head)) 
                -> false
            | _ when (brackets |> List.findIndex (fun x -> snd x = head)) = (brackets |> List.findIndex (fun x -> fst x = bracketStack.Head))
                -> loop str.Tail bracketStack.Tail
        | [] -> bracketStack.Length = 0
    let input = Seq.toList str |> getBrackets
    loop input []
