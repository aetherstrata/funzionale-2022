type 'a graph = ('a * 'a) list

let succ_list n g =
  List.map (fun (x, y) -> y) (List.filter (fun (x, y) -> x = n) g)

let rec remove n = function
  | [] -> []
  | x :: rest ->
      if x = n then
        rest
      else
        x :: remove n rest

let cammino_con_nodi graph start req_list =
  let rec from_node visited required n =
    if List.mem n visited || required = [] then
      raise Not_found
    else if required = [ n ] then
      [ n ]
    else
      n :: from_list (n :: visited) (remove n required) (succ_list n graph)
  and from_list visited required = function
    | [] -> raise Not_found
    | x :: rest ->
        try from_node visited required x
        with Not_found -> from_list visited required rest
  in
  from_node [] req_list start