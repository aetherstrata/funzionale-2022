(* ’a list -> ’a list *)
let rec alternate_pari = function
| [] -> []
| [x] -> [x]
| x::_::rest -> x::alternate_pari rest

let rec alternate_dispari = function
| [] | [_] -> []
| _::y::rest -> y::alternate_dispari rest
