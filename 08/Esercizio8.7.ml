type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree

(* si ho sbagliato a leggere la consegna, so un tossico *)
let cammino_costo tree =
  let rec aux m = function
  | Empty -> []
  | Tr(x,a,b) -> (x,m+x)::max (aux (m+x) a) (aux (m+x) b) in
  aux 0 tree

(* l'esercizio vero *)
let rec foglie_costi tree =
  let rec aux m = function
  | Empty -> []
  | Tr(x,Empty,Empty) -> [x,x+m]
  | Tr(x,a,b) -> aux (m+x) a @ aux (m+x) b in
  aux 0 tree