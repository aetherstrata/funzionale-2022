(* 'a * 'b list -> ('a * 'b) list *)
let rec pairwith y lst =
  match lst with
  | [] -> []
  | x::rest -> (y,x)::pairwith y rest