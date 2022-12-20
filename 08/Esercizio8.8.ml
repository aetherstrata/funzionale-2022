type expr =
  | Jolly
  | Int of int
  | Var of string
  | Sum of expr * expr
  | Diff of expr * expr
  | Mult of expr * expr
  | Div of expr * expr

let rec pattern_matching pattern expr =
  match (expr,pattern) with
  | (_,Jolly) -> true
  | Sum(e1,e2),Sum(m1,m2)
  | Diff(e1,e2),Diff(m1,m2)
  | Mult(e1,e2),Mult(m1,m2)
  | Div(e1,e2),Div(m1,m2) ->
    pattern_matching m1 e1 && pattern_matching m2 e2
  | e,m -> e = m