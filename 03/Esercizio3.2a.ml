(* read_max: unit -> int *)
let rec read_max () =
  try
    let n = read_int() in
      try max n (read_max())
      with _ -> n
  with _ -> failwith("Numero non valido");;

print_string ("Inserisci una sequenza di numeri:\n");;
let massimo = read_max();;

print_string ("Massimo:");;
print_int (massimo)