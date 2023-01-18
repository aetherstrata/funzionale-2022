type 'a graph = ('a * 'a) list
type 'a graph2 = 'a list * ('a * 'a) list

let succ_list n graph =
  List.map (fun (x, y) -> y) (List.filter (fun (x, y) -> x = n) graph)

let rec neighbor_list n = function
  | [] -> []
  | (a, b) :: rest ->
      if a = n then
        b :: neighbor_list n rest
      else if b = n then
        a :: neighbor_list n rest
      else
        neighbor_list n rest

let setadd x set =
  if List.mem x set then
    set
  else
    x :: set

(* Esercizio 10.4 *)
let valid_path edges start goal =
  let rec search visited = function
    | [] -> false
    | x :: rest ->
        if List.mem x visited then
          search visited rest
        else
          x = goal || search (x :: visited) (neighbor_list x edges @ rest)
  in
  search [] [ start ]

let grafo_connesso graph =
  let rec aux edges first = function
    | [] -> true
    | x :: rest -> valid_path edges first x && aux edges first rest
  in
  let nodes, edges = graph in
  aux edges (List.hd nodes) (List.tl nodes)

