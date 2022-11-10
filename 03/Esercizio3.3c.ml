let rec power n k =
  if k = 1 then n
  else n * power n (k-1)