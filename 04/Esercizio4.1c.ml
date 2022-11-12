let rec maxlist l =
  match l with
    [] -> failwith "Lista vuota"
  | [x] -> x
  | x::rest -> max x (maxlist rest)
