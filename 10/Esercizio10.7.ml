type 'a graph = ('a * 'a) list
type col = Rosso | Giallo | Verde | Blu
type 'a col_assoc = (col * 'a list) list

let rec color x = function
  | [] -> raise Not_found
  | (c, list) :: rest ->
      if List.mem x list then
        c
      else
        color x rest

let rec remove n = function
  | [] -> raise Not_found
  | x :: rest ->
      if x = n then
        rest
      else
        x :: remove n rest

let succ_list n graph =
  List.map (fun (x, y) -> y) (List.filter (fun (x, y) -> x = n) graph)

let colori_alterni graph assoc_lst start goal =
  let rec from_node visited last_col n =
    if List.mem n visited then
      raise Not_found
    else
      let col = color n assoc_lst in
      if col != last_col then
        if n = goal then
          [ n ]
        else
          n :: from_list (n :: visited) col (succ_list n graph)
      else
        raise Not_found
  and from_list visited last_col = function
    | [] -> raise Not_found
    | x :: rest -> (
        try from_node visited last_col x
        with _ -> from_list visited last_col rest)
  in
  start :: from_list [ start ] (color start assoc_lst) (succ_list start graph)
