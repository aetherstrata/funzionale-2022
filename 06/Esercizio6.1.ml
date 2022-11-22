(* ('a -> bool) -> 'a list -> 'a *)
let rec find predicate = function
  | [] -> failwith "ElementNotFound"
  | x::rest ->
    if predicate x then
      x
    else
      find predicate rest

(* int list -> int *)
let find_applicata list =
  let predicato n = (n*2 < 30) in
  find predicato list

(* ('a -> bool) -> 'a list -> 'a list *)
let rec takewhile predicate = function
  | [] -> []
  | x::rest ->
    if predicate x then
      x::takewhile predicate rest
    else
      []

(* ('a -> bool) -> 'a list -> 'a list *)
let rec dropwhile predicate list =
  match list with
  | [] -> []
  | x::rest ->
  if predicate x then
    dropwhile predicate rest
  else
    list

(* ('a -> bool) -> 'a list -> ('a list * 'a list)  *)
let partition predicate list =
  (* 'a list -> 'a list -> 'a list -> ('a * 'a) list *)
  let rec aux tmp_yes tmp_no = function
  | [] -> (tmp_yes, tmp_no)
  | x::rest ->
    if predicate x then
      aux (x::tmp_yes) tmp_no rest
    else
      aux tmp_yes (x::tmp_no) rest
  in aux [] [] list

(* 'a -> 'b list -> ('a * 'b) list *)
let pairwith elem list =
    List.map (fun (x) -> (elem,x)) list

(* int -> int list list -> bool *)
let rec verifica_matrice cond matrix =
  let predicate = fun (x) -> x<cond in
  List.exists (List.for_all predicate) matrix

(* 'a list -> 'a list -> 'a list *)
let setdiff set_a set_b =
  List.filter (fun (x) -> not(List.mem x set_b)) set_a

(* 'a list -> 'a list -> bool *)
let subset set subset =
  List.for_all (fun (x) -> List.mem x set) subset

(* int list -> int list *)
let raddoppia list =
  List.map (fun (x) -> 2*x) list

let mapcons list elem =
  List.map (fun (a,b) -> (a,elem::b)) list

(* int -> 'a -> 'a -> 'a list list *)
let rec tutte_liste_con n a b =
  let cons x rest = x::rest in
  if n=0 then
    [[]]
  else
    let tmp = tutte_liste_con (n-1) a b in
    List.map (fun (x) -> a::x) tmp @ List.map (fun (x) -> b::x) tmp

(* 'a -> 'a list -> 'a list list *)
let rec interleave n lst =
  match lst with
  | [] -> [[n]]
  | x::rest ->
    (* aggiungi il valore in cima alla lista passata *)
    (n::lst) :: (* e concatenala con la prossima *)
    List.map (fun (elem) -> print_int x; print_newline(); x::elem) (interleave n rest)

let rec perms lst =
  match lst with
  | [] -> [[]]
  | x::rest ->
    List.flatten (List.map (interleave x) (perms rest))