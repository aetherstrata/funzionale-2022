type 'a graph = ('a * 'a) list

let succ_list n graph =
  List.map (fun (x,y) -> y) (List.filter (fun (x,y) -> x=n) graph)

let rec neighbor_list n = function
| [] -> []
| (a,b)::rest ->
  if a=n then
    b::neighbor_list n rest
  else if b=n then
    a::neighbor_list n rest
  else
    neighbor_list n rest

let ciclo graph n =
  let rec search visited x =
  if List.mem x visited then
    raise Not_found
  else if x=n then
    [x]
  else
    x::auxlist (x::visited) (succ_list x graph)
  and auxlist visited = function
  | [] -> failwith "ciclo"
  | x::rest ->
    try search visited x
    with _ -> auxlist visited rest
  in
  search [] (succ_list n graph)