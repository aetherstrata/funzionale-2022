(* versione iterativa *)
(* 'a list -> ('a * 'a * 'a) list *)
let rec trips_it = function
  | [] | [_] | [_;_] -> []
  | a::b::c::rest -> (a,b,c)::(trips_it (b::c::rest))

(* versione tail recursive *)
(* 'a list -> ('a * 'a * 'a) list *)
let trips list =
  (* ('a * 'a * 'a) list -> 'a list -> ('a * 'a * 'a) list *)
  let rec trips_aux tmp = function
    | [] | [_] | [_;_] -> tmp
    | a::b::c::rest -> trips_aux (tmp@[(a,b,c)]) (b::c::rest)
  in
  trips_aux [] list