(* int -> 'a list -> 'a list *)
let rec drop n lst =
  let aux = function
  | [] -> []
  | x::rest -> rest in

  match n with
  | 0 -> lst
  | _ -> drop (n-1) (aux lst)

(* prende in input una lista e restituisce una tupla contenente la prima e seconda metà della lista separate *)
(* 'a list -> 'a list * 'a list *)
let split2 lst =
  let rec take n = function
    | [] -> []
    | x::rest -> if n<=0 then []
               else x::take (n-1) rest in

  let n = (List.length lst)/2 in
  take n lst , drop n lst