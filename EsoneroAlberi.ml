type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree

let rec bin limit = function
  | Empty -> failwith "bin"
  | Tr (leaf, Empty, Empty) ->
      if leaf <= limit then
        ([ leaf ], leaf, leaf)
      else
        failwith "bin"
  | Tr (node, tleft, tright) ->
      if limit < 0 then
        failwith "bin"
      else
        let path, leaf, cost =
          (try bin (limit - node) tleft with _ -> bin (limit - node) tright)
        in
        (node :: path, leaf, cost + node)

(********)

type 'a ntree = Ntree of 'a * 'a ntree list

let rec ntree limit = function
  | Ntree (leaf, []) ->
      if leaf <= limit then
        ([ leaf ], leaf, leaf)
      else
        failwith "ntree"
  | Ntree (node, tlist) ->
    if limit < 0 then
      failwith "ntree"
    else
      let path, leaf, cost = auxlist (limit - node) tlist in
      (node :: path, leaf, cost + node)

and auxlist limit = function
  | [] -> failwith "ntree"
  | t :: trest -> ( (try ntree limit t with _ -> auxlist limit trest))
