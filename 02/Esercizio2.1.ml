(* Definire una funzione ultime_cifre: int -> int * int che riporti il valore intero delle due ultime cifre di un int. *)

(* int -> int * int *)
let ultime_cifre x =
  let modulo = abs x in
  (modulo/10 mod 10, modulo mod 10)
