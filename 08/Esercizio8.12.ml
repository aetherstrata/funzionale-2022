type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree
type 'a sostituzione = ('a * 'a tree) list

let rec applica list = function
  | Empty -> Empty
  | Tr (x, Empty, Empty) as tree -> (
      (try List.assoc x list with Not_found -> tree))
  | Tr (x, a, b) -> Tr (x, applica list a, applica list b)
