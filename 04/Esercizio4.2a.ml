(* int -> 'a -> 'a list *)
let rec copy n x =
  if n < 0 then failwith("Negative count");
  match n with
  | 0 -> []
  | _ -> x::(copy (n-1) x)