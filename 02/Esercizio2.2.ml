(* int -> int * int *)
let ultime_cifre x =
  let modulo = abs x in
  (modulo/10 mod 10, modulo mod 10)

(* int -> bool *)
let rec bello x =
  if x <10 && x > -10 then (* una sola cifra *)
    match abs x with
      0 -> true
    | 3 -> true
    | 7 -> true
    | _ -> false
  else
    let (penultima,ultima) = ultime_cifre x in
    bello ultima && not (bello penultima)
