type 'a graph = ('a * 'a) list

let rec succ_list n = function
| [] -> []
| (a,b)::rest ->
  if a=n then
    b::(succ_list n rest)
  else
    (succ_list n rest)

let rec neighbor_list n = function
| [] -> []
| (a,b)::rest ->
  if a=n then
    b::(neighbor_list n rest)
  else if b=n then
    a::(neighbor_list n rest)
  else
    (neighbor_list n rest)

(* Esercizio 10.2 *)
let esiste_ciclo graph x =
  let rec search visited = function
  | [] -> false
  | n::rest ->
    if List.mem n visited then
      search visited rest
    else
      n=x || search (n::visited) (succ_list n graph @ rest)
  in search [] (succ_list x graph)