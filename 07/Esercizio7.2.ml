(*** Consegna : Definire il prodotto sul tipo nat ***)

type nat = Zero | Succ of nat

(* somma : nat -> nat -> nat *)
let rec somma n m =
  match n with
  | Zero -> m
  | Succ k -> Succ(somma k m)

let rec prodotto n m =
  match n with
  | Zero -> Zero
  | Succ Zero -> m
  | Succ k -> somma (prodotto k m) m