module LambdaInterpreter

// Lambda term.
type Lambda =
    | Var of int
    | Application of Lambda * Lambda
    | Abstraction of int * Lambda

// Get free vars from term.
let rec getFreeVars term =
    let rec sub term acc =
        match term with
        | Var name -> acc |> Set.add name
        | Application(left, right) -> sub left acc + sub right acc
        | Abstraction(var, inner) -> sub inner acc - set[var]
    Set.empty |> sub term

let rec getNewValFromSet set =
    let newVal = System.Guid.NewGuid()
    if set |> Set.contains newVal then
        set |> getNewValFromSet
    else
        newVal

//let rec substitute term variableToChange substitutedTerm =
//    match term with
//    | 