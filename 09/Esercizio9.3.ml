type 'a ntree = Ntree of 'a * 'a ntree list

let folgie_in_lista leaf_list tree =
  let rec aux = function
    | Ntree (x, []) -> List.mem x leaf_list
    | Ntree (x, list) -> List.for_all aux list
  in
  aux tree
