let rec nth n lst =
  match lst with
  | [] -> failwith ("OutOfBound")
  | x::rest ->
    if n=0 then x
    else nth (n-1) rest