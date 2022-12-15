type 'a pattern = Jolly | Val of 'a

let rec most_general_match l1 l2 =
  match (l1,l2) with
  | ([],[]) -> []
  | (x::rest1,y::rest2) ->
    if x=y then
      Val x::(most_general_match rest1 rest2)
    else
      Jolly::(most_general_match rest1 rest2)
  | _ -> failwith "M.G.M. Fail"