type expr =
  | Int of int
  | Var of string
  | Sum of expr * expr
  | Diff of expr * expr
  | Mult of expr * expr
  | Div of expr * expr

(* Esercizio 8.1a *)
(* expr -> expr -> bool *)
let rec subexpr expr1 expr2 =
  expr1 = expr2
  ||  match expr1 with
      | Sum (e1,e2) | Diff (e1,e2) | Mult (e1,e2) | Div (e1,e2) ->
        (subexpr e1 expr2) || (subexpr e2 expr2)
      | _ -> false

(* Esercizio 8.1b *)
(* expr -> string -> expr -> expr *)
let rec subst_in_expr expr1 var expr2 =
  match expr1 with
  | Var x -> if var=x then expr2 else expr1
  | Int _ -> expr1
  | Sum (e1,e2) -> Sum(subst_in_expr e1 var expr2, subst_in_expr e2 var expr2)
  | Diff (e1,e2) -> Diff(subst_in_expr e1 var expr2, subst_in_expr e2 var expr2)
  | Mult (e1,e2) -> Mult(subst_in_expr e1 var expr2, subst_in_expr e2 var expr2)
  | Div (e1,e2) -> Div(subst_in_expr e1 var expr2, subst_in_expr e2 var expr2)