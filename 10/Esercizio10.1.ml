type 'a graph = ('a * 'a) list

let succ_list graph node =
  List.map (fun (x,y) -> y) (List.filter (fun (x,y) -> x=node) graph)

let rec neighbor_list node = function
| [] -> []
| (x,y)::rest ->
  if x = node then
    y::neighbor_list node rest
  else if y = node then
    x::neighbor_list node rest
  else
    neighbor_list node rest

(* Esercizio 10.1 *)
let test_connessi graph start goal =
  let rec search visited = function
  | [] -> false
  | x::rest ->
    if List.mem x visited then
      search visited rest
    else x=goal ||
      search (x::visited) (succ_list graph x @ rest)
  in
  search [] [start]
