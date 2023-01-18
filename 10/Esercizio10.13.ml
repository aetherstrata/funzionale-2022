type 'a graph = ('a * 'a) list

let succ_list n graph =
  List.map (fun (x, y) -> y) (List.filter (fun (x, y) -> x = n) graph)

let path_n_p graph predicate depth start =
  let rec from_node visited limit n =
    if List.mem n visited || not (predicate n) then
      raise Not_found
    else if limit = 0 then
      [ n ]
    else
      n :: from_list visited (limit - 1) (succ_list n graph)
  and from_list visited limit = function
    | [] -> raise Not_found
    | x :: rest ->
        try from_node visited limit x
        with Not_found -> from_list visited limit rest
  in
  from_node [] depth start