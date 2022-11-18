(* versione tail recursive *)
(* ('a * 'a) list -> 'a -> bool *)
let rec has_range_in list k =
  match list with
  | [] -> false
  | (x,y)::rest ->
    (* gli operatori di confronto possono essere applicati a qualunque tipo *)
    if x<=k && k<=y
      then true
    else has_range_in rest k

(* ('a * 'a) list -> 'a list -> 'a list *)
let compito ranges list =
  (* ('a * 'a) -> 'a -> bool *)
  let in_range range input =
    match range with
    | a,b -> min a b <= input && input <= max a b
  in

  (* ('a * 'a) list -> 'a -> bool *)
  let rec has_range_in range_list k =
    match range_list with
    | [] -> false
    | (x,y)::rest ->
      (* gli operatori di confronto possono essere applicati a qualunque tipo *)
      if in_range (x,y) k
        then true
      else has_range_in rest k
  in

  (* 'a list -> 'a list -> 'a list *)
  let rec aux tmp = function
  | [] -> tmp
  | x::rest ->
    if has_range_in ranges x
      then aux (x::tmp) rest
    else aux tmp rest
  in

  aux [] list