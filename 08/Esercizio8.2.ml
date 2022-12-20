type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree

(* Esercizio 8.1a *)
let rec reflect = function
  | Empty -> Empty
  | Tr (x, a, b) -> Tr (x, reflect b, reflect a)

(* Esercizio 8.1b *)
let fulltree height =
  let rec aux k n =
    if n = 0 then
      Empty
    else
      Tr (k, aux (2 * k) (n - 1), aux ((2 * k) + 1) (n - 1))
  in
  aux 1 height

(* Esercizio 8.1c *)
let rec height = function
  | Empty -> 0
  | Tr (_, a, b) -> 1 + max (height a) (height b)

let rec balanced = function
  | Empty -> true
  | Tr (_, a, b) -> balanced a && balanced b && abs (height a - height b) <= 1

(* Esercizio 8.1d *)
let rec preorder = function
  | Empty -> []
  | Tr (x, a, b) -> [ x ] @ preorder a @ preorder b

let rec inorder = function
  | Empty -> []
  | Tr (x, a, b) -> inorder a @ [ x ] @ inorder b

let rec postorder = function
  | Empty -> []
  | Tr (x, a, b) -> postorder a @ postorder b @ [ x ]

(* Esercizio 8.1e *)
let rec take n = function
  | [] -> []
  | x::rest ->
    if n <= 0
    then []
    else x::(take (n-1) rest)

let rec drop n = function
  | [] -> []
  | x::rest ->
    if n > 0
    then drop (n-1) rest
    else x::rest

let rec balpreorder = function
  | [] -> Empty
  | x::rest ->
    let height = (List.length rest)/2 in
    Tr(x, balpreorder (take height rest), balpreorder (drop height rest))

let rec balinorder = function
  | [] -> []
  | lst ->
    let height = (List.length lst)/2 in
    let y::ys = drop height lst in
    Tr(y, balinorder (take height xs), balinorder ys)

(* ??? *)
(* fuck you and I'll see you tomorrow *)