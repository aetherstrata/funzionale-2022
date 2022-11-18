(* int -> int * int -> bool *)
let in_labirinto dim (r,c) =
  (* assume che il labirinto è quadrato *)
  r >= 0 && c >= 0 && r < dim && c < dim

let filter_vicini dim lst =
  (* (int * int) list -> (int * int) list -> (int * int) list *)
  let rec aux tmp = function
  | [] -> tmp
  | casella::rest ->
    if in_labirinto dim casella
      then aux (casella::tmp) rest
    else aux tmp rest
  in
  aux [] lst
