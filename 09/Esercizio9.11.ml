type 'a ntree = Ntree of 'a * 'a ntree list

let rec same_structure t1 t2 =
  match (t1, t2) with
  | Ntree (_, []), Ntree (_, []) -> true
  | Ntree (_, tlist1), Ntree (_, tlist2) -> auxlist tlist1 tlist2

and auxlist tlist1 tlist2 =
  match (tlist1, tlist2) with
  | [], [] -> true
  | x :: rest1, y :: rest2 -> same_structure x y && auxlist tlist1 tlist2
  | _ -> false
