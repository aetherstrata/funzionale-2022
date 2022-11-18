(* ('a * 'b) list -> 'a -> ('a * 'b) list -> ('a * 'b) list *)
let rec cancella_aux tmp k = function
  | [] -> tmp
  | (key,value)::rest ->
    if key = k
      then cancella_aux tmp k rest
    else cancella_aux ((key,value)::tmp) k rest

let cancella k dict = cancella_aux [] k dict

(* 'a -> ('a * 'b) list -> ('a * 'b) list *)
let cancella_it k dict =
  match dict with
  | [] -> []
  | (key,value)::rest ->
    if key = k
      then cancella k rest
    else (key,value)::cancella k dict