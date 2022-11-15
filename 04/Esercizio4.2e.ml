let enumera lst =
  let rec enumera_rec i = function
  | [] -> []
  | x::rest -> (i,x)::enumera_rec (i+1) rest in
  enumera_rec 0 lst