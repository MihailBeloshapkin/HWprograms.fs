module ExpressionCalculator

type Expression =
    | Number of int
    | Addition of Expression * Expression
    | Subtraction of Expression * Expression
    | Multiplication of Expression * Expression
    | Division of Expression * Expression

    
let rec calculate expression = 
    match expression with
    | Number x -> x
    | Addition(x, y) -> calculate x + calculate y
    | Subtraction(x, y) -> calculate x - calculate y
    | Multiplication(x, y) -> calculate x * calculate y
    | Division(x, y) -> calculate x / calculate y