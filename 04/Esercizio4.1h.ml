let rec remove x lst =
  match lst with
  | [] -> []
  | head::rest ->
    if head=x
      then remove x lst
    else head::remove x rest