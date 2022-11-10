let rec num_di_stringhe() =
  let stringa = read_line() in
  if stringa = "" then 0
  else 1 + num_di_stringhe()