type 'a ntree = Tr of 'a * 'a ntree list

type multi_expr =
  | MultiInt of int
  | MultiVar of string
  | MultiDiff of multi_expr * multi_expr
  | MultiDiv of multi_expr * multi_expr
  | MultiSum of multi_expr list
  | MultiMult of multi_expr list

(* Esercizio 9.1a *)
let rec subexpr expr sub =
  expr = sub
  ||
  match expr with
  | MultiDiff (e1, e2) | MultiDiv (e1, e2) -> subexpr e1 sub || subexpr e1 sub
  | MultiSum list | MultiMult list ->
      List.exists (fun elem -> subexpr elem sub) list
  | _ -> false

(* Esercizio 9.1b *)
let rec subst subst_expr label = function
  | MultiVar str ->
      if str = label then
        subst_expr
      else
        MultiVar str
  | MultiInt x -> MultiInt x
  | MultiDiff (e1, e2) ->
      MultiDiff (subst subst_expr label e1, subst subst_expr label e2)
  | MultiDiv (e1, e2) ->
      MultiDiv (subst subst_expr label e1, subst subst_expr label e2)
  | MultiSum list ->
      MultiSum (List.map (fun elem -> subst subst_expr label elem) list)
  | MultiMult list ->
      MultiMult (List.map (fun elem -> subst subst_expr label elem) list)
