(* string -> char list *)
let string_to_list s =
  let len = String.length s in
  let rec aux i lst =
    if i >= 0 then
      aux (i - 1) (s.[i] :: lst)
    else
      lst
  in
  aux (len - 1) []

(* char list -> string *)
let list_to_string lst =
  let rec aux str = function
    | [] -> str
    | x :: rest -> aux (String.make 1 x ^ str) rest
  in
  aux "" lst