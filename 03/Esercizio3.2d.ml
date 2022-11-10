(* int -> bool *)
let rec occorre n =
  let x = read_int() in
  try occorre n || x == n
  with _ -> false