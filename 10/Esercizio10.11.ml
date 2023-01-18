type 'a graph = ('a * 'a) list

let succ_list n graph =
  List.map (fun (x, y) -> y) (List.filter (fun (x, y) -> x = n) graph)

let prime = function
  | 0 -> false
  | 1 -> true
  | n ->
      let rec aux n x =
        if n > x then
          n mod x != 0 && aux n (x + 1)
        else
          true
      in
      aux n 2

let cammino_di_primi graph start goal =
  let rec search visited = function
    | [] -> false
    | x :: rest ->
        if List.mem x visited then
          search visited rest
        else if x = goal then
          prime x
        else
          prime x && search (x :: visited) (succ_list x graph)
  in
  search [] [ start ]

let prime_only_path graph start goal =
  let rec from_node visited n =
    if List.mem n visited || not (prime n) then
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