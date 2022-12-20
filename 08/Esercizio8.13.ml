type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree

(* togli un elemento da una lista: 'a -> 'a list -> 'a list *)
(* visitando un nodo si scorre la lista e si tolgie l'elemento con l'etichetta
   corrispondete *)
let rec togli elem = function
  | [] -> []
  | x :: rest ->
      if x = elem then
        rest
      else
        x :: togli elem rest

let rec cammino_coprente list = function
  | Empty -> failwith "cammino_coprente"
  | Tr (x, Empty, Empty) -> (
    match list with
    | [] -> [x]
    | [x] -> [x]
    | _ -> failwith "ca")
  | Tr (x, a, b) ->
      let updated = togli x list in
      x :: (try cammino_coprente updated a with _ -> cammino_coprente updated b)
