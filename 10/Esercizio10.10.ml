type chiave = Aperta | Chiusa
type cassaforte = chiave list

exception Fail

let gira = function
  | Aperta -> Chiusa
  | Chiusa -> Aperta

let giraPrima = function
  | x :: rest -> gira x :: rest
  | _ -> raise Fail

let rec giraDopoChiusa = function
  | Chiusa :: x :: rest -> Chiusa :: gira x :: rest
  | Aperta :: rest -> Aperta :: giraDopoChiusa rest
  | _ -> raise Fail

let succ_list config =
  giraPrima config :: (try [ giraDopoChiusa config ] with Fail -> [])

let rec nodi n =
  if n = 0 then
    [ [] ]
  else
    let tmp = nodi (n - 1) in
    List.map (fun c -> Aperta :: c) tmp @ List.map (fun c -> Chiusa :: c) tmp

