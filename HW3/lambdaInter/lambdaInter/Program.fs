module LambdaInterpreter

/// Lambda term.
type Lambda<'a> =
    | Var of 'a
    | Application of Lambda<'a> * Lambda<'a>
    | Abstraction of 'a * Lambda<'a>

/// <summary>
/// Get free vars from term.
/// </summary>
let rec getFreeVars term =
    let rec sub term acc =
        match term with
        | Var name -> acc |> Set.add name
        | Application(left, right) -> sub left acc + sub right acc
        | Abstraction(var, inner) -> sub inner acc - set[var]
    Set.empty |> sub term

/// <summary>
/// Get value from the set.
/// </summary>
let rec getNewValFromSet set =
    let newVal = System.Guid.NewGuid()
    if set |> Set.contains newVal then
        set |> getNewValFromSet
    else
        newVal

/// <summary>
/// Substitutes variable in term.
/// </summary>
let rec substitute term variableToChange substitutedTerm =
    match term with
    | Var name when name = variableToChange -> substitutedTerm
    | Var _ -> term
    | Application(left, right) -> Application(substitute left variableToChange substitutedTerm, substitute right variableToChange substitutedTerm)
    | Abstraction(variable, innerTerm) -> 
        match variable with 
        | name when name = variableToChange -> term
        | _ when getFreeVars innerTerm |> Set.contains variableToChange |> not || getFreeVars substitutedTerm |> Set.contains variable |> not
            -> Abstraction(variable, substitute innerTerm variableToChange substitutedTerm)
        | _ -> let newVar = getFreeVars innerTerm + getFreeVars substitutedTerm |> getNewValFromSet 
               Abstraction(newVar, innerTerm |> substitute (Var newVar) variable |> substitute substitutedTerm variableToChange) 

/// <summary>
/// Beta reduction by normal strategy.
/// </summary>
let rec reduce term =
    match term with
    | Var _ -> term
    | Application(left, right) -> match left with
                                  | Abstraction(variable, innerTerm) -> substitute innerTerm variable right |> reduce
                                  | _ -> Application(reduce left, reduce right)
    | Abstraction(variable, innerTerm) -> Abstraction(variable, reduce innerTerm)