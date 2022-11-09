open Format

(* string -> bool *)
let meseValido mese =
  match mese with
    | "gennaio" -> true
    | "febraio" -> true
    | "marzo" -> true
    | "aprile" -> true
    | "maggio" -> true
    | "giugno" -> true
    | "luglio" -> true
    | "agosto" -> true
    | "settembre" -> true
    | "ottobre" -> true
    | "novembre" -> true
    | "dicembre" -> true
    | _ -> false

let giornoMax mese =
  match mese with
  | "gennaio" -> 31
  | "febraio" -> 28
  | "marzo" -> 31
  | "aprile" -> 30
  | "maggio" -> 31
  | "giugno" -> 30
  | "luglio" -> 31
  | "agosto" -> 31
  | "settembre" -> 30
  | "ottobre" -> 31
  | "novembre" -> 30
  | "dicembre" -> 31
  | _ -> 0

(* int * string -> bool *)
let data (n, str) =
  if(meseValido str && n > 0 && n <= giornoMax str) then
    true
  else
    false;;
