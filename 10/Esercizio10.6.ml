type 'a graph = 'a list * ('a * 'a) list

let succ_list n graph =
  List.map (fun (x, y) -> y) (List.filter (fun (x, y) -> x = n) graph)

let rec remove n = function
  | [] -> raise Not_found
  | x :: rest ->
      if x = n then
        rest
      else
        remove n rest

let cammino graph node_lst start goal =
  let rec aux needed n =
    if not (List.mem n needed) then
      raise Not_found
    else if n = goal && needed = [ n ] then
      [ n ]
    else
      n :: auxlist (remove n needed) (succ_list n graph)
  and auxlist needed = function
    | [] -> raise Not_found
    | x :: rest -> (try aux needed x with _ -> auxlist needed rest)
  in
  aux node_lst start

let hamiltoniano (nodes, edges) =
  let n = List.hd nodes in
  let rec auxlist needed = function
    | [] -> raise Not_found
    | x :: rest ->
        (try cammino edges needed x n with _ -> auxlist needed rest)
  in
  n :: auxlist (List.tl nodes) (succ_list n edges)
