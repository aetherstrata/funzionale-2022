let maxstring stringa =
  let rec aux maxchar i =
    try
      aux (max maxchar stringa.[i]) (i+1)
    with _ -> maxchar (* stringa terminata *)
  in
  try aux stringa.[0] 0
  with _ -> raise( Invalid_argument "Stringa vuota" )