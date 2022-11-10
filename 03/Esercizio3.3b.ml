let rec sumto x =
  if x < 0 then raise (Invalid_argument "Numero negativo")
  else if x = 0 then 0
  else x + sumto (x-1)