type 'a ntree = Ntree of 'a * 'a ntree list

(* List.nth solleva un eccezione quando si tenta di accedere a dati
   inesistenti *)
let rec lista_guida (tree : 'a ntree) =
  let (Ntree (label, sub_trees)) = tree in
  function
  | [] -> label
  | x :: rest -> lista_guida (List.nth sub_trees x) rest

