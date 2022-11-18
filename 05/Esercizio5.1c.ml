let rec split_aux input first second =
  match input with
  | [] -> first,second
  | (x,y)::rest -> split_aux rest (x::first) (y::second)

let split list = split_aux list [] []