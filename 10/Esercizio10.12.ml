type form =
  | Prop of string
  | Not of form
  | And of form * form
  | Or of form * form

type 'a graph = ('a * 'a) list

let succ_list n graph =
  List.map (fun (x, y) -> y) (List.filter (fun (x, y) -> x = n) graph)

let rec contradicts list = function
  | Not f -> List.mem (Not (Not f)) list || List.mem f list
  | f -> List.mem (Not f) list

let non_contradictory_path graph start goal =
  let rec from_node visited n =
    if List.mem n visited || contradicts visited n then
      raise Not_found
    else if n = goal then
      [ n ]
    else
      n :: from_list (n :: visited) (succ_list n graph)
  and from_list visited = function
    | [] -> raise Not_found
    | x :: rest ->
      try from_node visited x
      with Not_found -> from_list visited rest
  in
  from_node [] start