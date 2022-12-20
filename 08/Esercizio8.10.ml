type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree

(* Esercizio 8.10a *)
let rec stessa_struttura tr1 tr2 =
  match (tr1, tr2) with
  | Empty, Empty -> true
  | Tr (_, _, _), Empty | Empty, Tr (_, _, _) -> false
  | Tr (_, a1, b1), Tr (_, a2, b2) ->
      stessa_struttura a1 a2 && stessa_struttura b1 b2

(* Esercizio 8.10b *)
let rec mapping_list tr1 tr2 =
  match (tr1, tr2) with
  | Empty, Empty -> []
  | Tr (_, _, _), Empty | Empty, Tr (_, _, _) -> failwith "mapping_list"
  | Tr (x, a1, b1), Tr (y, a2, b2) ->
      ((x, y) :: mapping_list a1 a2) @ mapping_list b1 b2

(* Un mapping è una funzione se ogni elemento del dominio è collegato a un solo
   elemento del codominio *)
let rec is_function = function
  | [] -> true
  | (input, output) :: rest ->
      List.for_all (fun (any_in, any_out) -> any_in <> input || any_out = output)
        rest
      && is_function rest

let esiste_mapping tr1 tr2 = is_function (mapping_list tr1 tr2)