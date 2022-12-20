type 'a ntree = Ntree of 'a * 'a ntree list
type col = Rosso | Giallo | Verde | Blu
type 'a col_assoc = (col * 'a list) list

let rec colore x = function
  | [] -> failwith "colore"
  | (col, list) :: rest ->
      if List.mem x list then
        col
      else
        colore x rest

let rec ramo_colorato (x : 'a) (assoc_list : 'a col_assoc) (tree : 'a ntree) =
  let col_diverso y (Ntree (r, _)) =
    colore y assoc_list <> colore r assoc_list
  in
  let rec fromnode = function
    | Ntree (y, []) ->
        if x = y then
          [ x ]
        else
          raise Not_found
    | Ntree (y, tlist) -> y :: fromlist (List.filter (col_diverso y) tlist)
  and fromlist = function
    | [] -> raise Not_found
    | t :: rest -> ( (try fromnode t with Not_found -> fromlist rest))
  in
  fromnode tree
