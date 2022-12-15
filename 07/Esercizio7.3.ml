type chiave = Aperta | Chiusa
type cassaforte = chiave list

let gira = function
| Aperta -> Chiusa
| Chiusa -> Aperta

let giraPrima cassaforte =
  match cassaforte with
  | [] -> failwith "giraPrima"
  | x::rest -> (gira x)::rest

let rec giraDopoChiusa = function
  | Chiusa::x::rest -> Chiusa::(gira x)::rest
  | Aperta::rest -> Aperta::(giraDopoChiusa rest)
  | _ -> failwith "giraDopoChiusa"

let successori cassaforte =
  (giraPrima cassaforte)::(try [giraDopoChiusa cassaforte] with _ -> [])