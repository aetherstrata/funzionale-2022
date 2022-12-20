type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree

let rec num_foglie = function
  | Empty -> 0
  | Tr (_, Empty, Empty) -> 1
  | Tr (_, a, b) -> num_foglie a + num_foglie b