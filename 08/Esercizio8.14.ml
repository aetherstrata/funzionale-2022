type col = Rosso | Giallo | Verde | Blu
type 'a col_assoc = (col * 'a list) list
type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree

(* Esercizio 8.14a *)
let rec colore x = function
  | [] -> failwith "colore"
  | (col, assoc) :: rest ->
      if List.mem x assoc then
        col
      else
        colore x rest

(* Esercizio 8.14b *)
let path_to leaf assoc tree =
  let rec aux last = function
    | Empty -> failwith "path_to"
    | Tr (x, Empty, Empty) ->
        if x != leaf || colore x assoc = last then
          failwith "path_to"
        else
          [ x ]
    | Tr (x, a, b) ->
        if colore x assoc != last then
          x :: (try aux (colore x assoc) a with _ -> aux (colore x assoc) b)
        else
          failwith "path_to"
  in
  match tree with
  | Empty -> failwith "path_to"
  | Tr (x, a, b) ->
      let first_color = colore x assoc in
      x :: (try aux first_color a with _ -> aux first_color b)
