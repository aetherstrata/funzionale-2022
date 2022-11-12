(* int list -> list *)
let rec sumof l =
  match l with
  | [] -> 0
  | head::rest -> head + sumof rest