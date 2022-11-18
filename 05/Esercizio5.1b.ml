(* 'a list -> int *)
let length list =
  (* int -> 'a list -> int *)
  let rec aux n = function
    | [] -> n
    | x::rest -> aux (n+1) rest
  in aux 0 list

let combine lst1 lst2 =
  let rec aux tmp l1 l2 =
    match l1,l2 with
    | [],[] -> tmp
    | x::rest1,y::rest2 -> aux ((x,y)::tmp) rest1 rest2
    | _ -> failwith "combine"
  in
  if length lst1 != length lst2
    then failwith "Length mismatch"
  else aux [] (List.rev lst1) (List.rev lst2)