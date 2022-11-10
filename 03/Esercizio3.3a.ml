let rec sum_between a b =
  if a = b then b
  else a + sum_between (a+1) b