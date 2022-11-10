let read_max_min () =
  let rec aux nmin nmax =
    try
      let n = read_int() in
      let tmpmin = min n nmin in
      let tmpmax = max n nmax in
        aux (tmpmin) (tmpmax)
    with _ -> nmax, nmin
  in
  let n = read_int () in
    try
      aux n n
    with _ -> failwith "Inserimento invalido"