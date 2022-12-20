type 'a ntree = Ntree of 'a * 'a ntree list

let rec path_non_pred pred = function
  | Ntree (x, []) ->
      if pred x then
        failwith "path_non_pred"
      else
        [ x ]
  | Ntree (x, tlist) ->
      if pred x then
        failwith "path_non_pred"
      else
        x :: auxlist pred tlist

and auxlist pred = function
  | [] -> failwith "path_non_pred"
  | t :: trest -> ( (try path_non_pred pred t with _ -> auxlist pred trest))
