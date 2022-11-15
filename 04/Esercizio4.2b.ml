(* int list -> bool *)
let rec nondec lst =
  match lst with
  | [] | [_] -> true
  | a::b::rest -> a<=b && nondec(b::rest)