type metro = (int * int * string) list

let setadd x lst =
  if List.mem x lst then
    lst
  else
    x :: lst

(* line: metro -> string -> int list *)
let line metro linea =
  let rec aux tmp = function
    | [] -> tmp
    | (a, b, c) :: rest ->
        if c = linea then
          aux (setadd a (setadd b tmp)) rest
        else
          aux tmp rest
  in
  aux [] metro

(* vicini: int -> metro -> (int * string) list *)
let vicini x metro =
  (* aux: metro -> (int * string) list *)
  let rec aux = function
    | [] -> []
    | (a, b, c) :: rest ->
        if a = x then
          (b, c) :: aux rest
        else if b = x then
          (a, c) :: aux rest
        else
          aux rest
  in
  aux metro

exception NotFound

(* raggiungi: metro -> int -> int -> int -> int list *)
let raggiungi m cmax start goal =
  let rec from_node visited limit last_ln (s, ln) =
    if List.mem s visited || limit = 0 then
      raise NotFound
    else if s = goal then
      [ s ]
    else if last_ln != ln then
      s :: from_list (s :: visited) (limit - 1) ln (vicini s m)
    else
      s :: from_list (s :: visited) limit ln (vicini s m)
  and from_list visited limit last_ln = function
    | [] -> raise NotFound
    | x :: rest -> (
        try from_node visited limit last_ln x
        with NotFound -> from_list visited limit last_ln rest)
  in
  from_node [] (cmax+1) "" (start,"")