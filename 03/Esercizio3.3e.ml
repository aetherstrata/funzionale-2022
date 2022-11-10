let maxstring stringa =
  let rec aux maxchar i =
    try
      aux (max maxchar stringa.[i]) (i+1)
    with _ -> maxchar (* stringa terminata *)
  in
  try aux stringa.[0] 0
   (* inizializzazione del ciclo con il carattere in posizione 0,
      iniziando dal successivo *)
  with _ -> failwith "Stringa vuota"
   (* questa eccezione sara' sollevata solo nel caso in cui s="",
      per evitare che sia sollevata l'eccezione predefinita
      Invalid_argument "index out of bounds" *)