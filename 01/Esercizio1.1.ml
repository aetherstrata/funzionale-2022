let pi = 3.14159

(* qui viene creata la funzione e viene collegata al valore di pi corrente *)
(* float -> float *)
let area x = pi *. x

(* qui pi viene sovrascritto ma l'istanza di riferimento di area rimane 3.1415 *)
let pi = 0.0

(* ? *)
let x = "pippo"

let a = area 3.0;;

(* stampa 9.qualcosa *)
print_float a