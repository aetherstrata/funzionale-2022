let rec drop n lst =
  match n with
  | 0 -> lst
  | _ -> drop (n-1) (
    match lst with
    | [] -> []
    | x::rest -> rest
  )