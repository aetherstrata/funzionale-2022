(* ’a -> ’a list -> int *)
let position a =
  let rec aux a i = function
  | [] -> failwith("Element Not Found")
  | x::rest ->
    if x=a then i
    else aux a (i+1) rest in
  function
  | [] -> failwith("Empty List")
  | x -> aux a 0 x