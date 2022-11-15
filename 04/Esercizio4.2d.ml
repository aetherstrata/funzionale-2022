(* 'a list -> 'a list *)
let rec duplica = function
| [] -> []
| x::rest -> x::x::duplica rest