type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree

let rec max_common_subtree tr1 tr2 =
  match (tr1, tr2) with
  | Empty, Empty -> Empty
  | Empty, Tr (_, _, _) | Tr (_, _, _), Empty -> Tr ("@", Empty, Empty)
  | Tr (x1, a1, b1), Tr (x2, a2, b2) ->
      if x1 = x2 then
        Tr (x1, max_common_subtree a1 a2, max_common_subtree b1 b2)
      else
        Tr ("@", Empty, Empty)
