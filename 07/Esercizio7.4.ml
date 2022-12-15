exception Impossibile

type obj = Miss | Cann | Barca
type situazione = obj list * obj list

let initial = ([Miss;Miss;Miss;Cann;Cann;Cann;Barca], [])

type azione =
| DaSinistra of obj list
| DaDestra of obj list

let rec conta tipo = function
| [] -> 0
| x::rest ->
  if x=tipo then
    1+(conta tipo rest)
  else
    (conta tipo rest)

let sicuro (sinistra,destra) =
  (conta Cann sinistra)<(conta Miss sinistra)
  && (conta Cann destra)<(conta Miss sinistra)

(* togli un elemento del tipo specificato dalla lista *)
let rec togli_un x = function
  | [] -> raise Impossibile
  | y::rest ->
    if y=x then rest
    else y::(togli_un x rest)

(* togli tutti gli elementi della sorgente dalla lista *)
let rec togli source = function
  | [] -> source
  | x::rest ->
    togli (togli_un x source) rest

let applica azione (destra,sinistra) =
  let result =
    match azione with
    | DaSinistra lst ->
	    if List.length lst > 2 || lst=[] then
        raise Impossibile
	    else
        (togli_un Barca (togli sinistra lst),Barca::lst @ destra)
    | DaDestra lst ->
	    if List.length lst > 2 || lst=[] then
        raise Impossibile
	    else (Barca::lst @ sinistra,togli_un Barca (togli destra lst))
  in
  if sicuro result then
    result
  else
    raise Impossibile