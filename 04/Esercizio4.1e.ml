let rec append lst1 lst2 =
  match lst1 with
  | [] -> lst2
  | x::rest -> x::append rest lst2