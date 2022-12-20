type 'a ntree = Ntree of 'a * 'a ntree list

let rec togli elem = function
  | [] -> raise Not_found
  | x :: rest ->
      if x = elem then
        rest
      else
        x :: togli elem rest

let rec ramo_da_lista tree lst label =
  (* prende la lista *)
  let rec aux list label = function
    | [] -> raise Not_found
    | t :: rest -> (
      (* controlla il primo e scorri se riporta un eccezione *)
        (try ramo_da_lista t list label with _ -> aux list label rest))
  in

  match tree with
  | Ntree (x, []) ->
      (* caso base: verifica che la foglia coincide con l'etichetta indicata *)
      if x = label && lst = [ x ] then
        [ x ]
      else
        failwith "ramo_lista"
  | Ntree (x, tlist) ->
      (* aux riporta il primo sottoalbero compatibile *)
      x :: aux (togli x lst) label tlist
