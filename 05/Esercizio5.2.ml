(* per definizione le singole liste non hanno ripetizioni quindi basta solo
   controllare la presenza di doppioni *)

(* 'a -> 'a list -> bool *)
let rec has_element k = function
  | [] -> false
  | x :: rest ->
      if k = x then
        true
      else
        has_element k rest

(* 'a list -> 'a list -> 'a list *)
let rec union list1 list2 =
  match list1 with
  | [] -> list2
  | x :: rest ->
      if has_element x list2 then
        union rest list2
      else
        union rest (x :: list2)

(* 'a list -> 'a list -> 'a list *)
let intersection list1 list2 =
  let rec intersection_aux tmp = function
    | [] -> tmp
    | x :: rest ->
        if has_element x list2 then
          intersection_aux (x :: tmp) rest
        else
          intersection_aux tmp rest
  in
  intersection_aux [] list1

(* 'a list -> 'a list -> 'a list *)
let setdiff list1 list2 =
  let rec setdiff_aux tmp = function
    | [] -> tmp
    | x :: rest ->
        if has_element x list2 then
          setdiff_aux tmp rest
        else
          setdiff_aux (x :: tmp) rest
  in
  setdiff_aux [] list1

(* 'a list -> 'a list -> bool *)
let rec subset list1 list2 =
  match list1 with
  | [] -> true
  | x::rest ->
    if has_element x list2
      then subset rest list2
    else false