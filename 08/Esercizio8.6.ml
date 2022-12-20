type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree

let rec foglia_costo = function
  | Empty -> 0,0
  | Tr(x,Empty,Empty) -> x,x
  | Tr(x,a,b) ->
    let (nodo,max) = max (foglia_costo a) (foglia_costo b) in
    nodo, x + max