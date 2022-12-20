type 'a ntree = Ntree of 'a * 'a ntree list

let rec max_pair = function
  | [] -> failwith "palle"
  | [ (x, y) ] -> (x, y)
  | (x, y) :: (x2, y2) :: rest ->
      if y >= y2 then
        max_pair ((x, y) :: rest)
      else
        max_pair ((x2, y2) :: rest)

let rec foglia_costo = function
  | Ntree (x, []) -> (x, x)
  | Ntree (x, list) ->
      let leaf, total = max_pair (List.map foglia_costo list) in
      (leaf, total + x)
