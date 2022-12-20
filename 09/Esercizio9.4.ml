type 'a ntree = Ntree of 'a * 'a ntree list

let rec sumof = function
  | [] -> 0
  | x::rest -> x+sumof rest

let rec num_di_foglie = function
  | Ntree(x,[]) -> 1
  | Ntree(_,list) -> sumof (List.map num_di_foglie list)