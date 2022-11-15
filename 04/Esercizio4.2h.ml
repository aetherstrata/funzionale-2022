(*** versione dichiarativa ***)
(* int list list -> int *)
let rec min_dei_max =
  let rec max_list = function
  | [] -> failwith "Lista vuota"
  | [x] -> x
  | x::rest -> max x (max_list rest) in
  function
  | [] -> failwith "Struttura vuota"
  | [x] -> max_list x
  | x::rest -> min (max_list x) (min_dei_max rest)


(*** versione tail recursive ***)
(* 'a list -> 'a *)
let max_list_tail =
 let rec aux tmp = function
    | [] -> tmp
    | x::rest -> aux (max tmp x) rest in

  function
  | [] -> failwith "Lista Vuota"
  | x::rest -> aux x rest

(* 'a list list -> 'a *)
let rec min_dei_max_tail =
  let rec aux tmp = function
  | [] -> tmp
  | x::rest -> aux (min tmp (max_list_tail x)) rest in

  function
  | [] -> failwith "Struttura vuota"
  | x::rest -> aux (max_list_tail x) rest