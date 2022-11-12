let rec length l =
  match l with
  | [] -> 0
  | head::rest -> 1 + length rest