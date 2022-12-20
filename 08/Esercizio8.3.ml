type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree

(* 'a list -> 'a tree -> bool *)
let rec foglie_in_lista lst = function
  | Empty -> true
  | Tr (x, a, b) ->
      if a = Empty && b = Empty then
        List.mem x lst
      else
        foglie_in_lista lst a && foglie_in_lista lst b

(* 'a list -> 'a tree -> bool *)
let rec foglie_in_lista2 lst = function
  | Empty -> true
  | Tr (x, Empty, Empty) -> List.mem x lst
  | Tr (x, a, b) -> foglie_in_lista lst a && foglie_in_lista lst b
