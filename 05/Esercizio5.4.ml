(* int -> int list *)
let rec int_range n =
  (* int -> int list -> int list *)
  let rec int_range_aux n list =
    if n=0 then
      list
    else
      int_range_aux (n-1) (n::list)
  in
  int_range_aux n []

(* int -> (int * int) list *)
let intpairs n =
  let range = int_range n in
  (* (int * int) list -> int -> int list -> (int * int) list *)
  let rec combine_aux tmp n = function
  | [] -> tmp
  | x::rest -> combine_aux ((n,x)::tmp) n rest
  in
  (* (int * int) list -> int -> (int * int) list *)
  let rec intpairs_aux tmp n =
    if n=0 then
      tmp
    else
      intpairs_aux ((combine_aux [] n range)@tmp) (n-1)
  in
  intpairs_aux [] n