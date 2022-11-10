let is_minore soglia =
  let n = read_int () in
  try n<soglia
  with _ -> failwith "Sequenza invalida"

let rec tutti_minori soglia =
  let x = is_minore soglia in
  try is_minore soglia && x
  with _ -> true