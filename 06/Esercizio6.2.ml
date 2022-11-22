(* (int * ((int * int ) * 'a) list) -> int -> 'a -> bool *)
let in_riga (_,list) row value =
  List.exists (fun ((x,_),item) -> x=row && item=value) list

(* (int * ((int * int) * 'a) list) -> int -> 'a -> int *)
let trova_colonna (_,list) row value =
  let result = List.find (fun ((x,_),item) -> x=row && item=value) list in
  match result with ((_,c),_) -> c

(* int -> int -> int list *)
let rec upto inizio fine =
  if inizio > fine then []
  else inizio :: upto (inizio+1) fine

(* (int * ((int * int) 'a) list) -> 'a -> bool *)
let in_tutte matrice value =
  let (dim,list) = matrice in
  (* scorre la lista usando la dimensione della matrice e per ogni int (riga)
     controlla che ci sia un elemento corrispondente *)
  List.for_all (fun riga -> in_riga matrice riga value) (upto 0 (dim-1))


(* 'a -> 'a list -> ('a list * 'a list) *)
let find x lst =
  (* 'a list -> 'a list -> 'a list -> ('a list * 'a list) *)
  let rec aux first second = function
  | [] -> (first,second)
  | y::rest ->
    if x=y then
      aux first rest []
    else
      aux (first@[y]) [] rest
  in
  aux [] [] lst

(* 'a -> 'a list -> ('a list * 'a list) *)
let spezza n lst =
  let (_,work) = find n lst in
  find n work

(* ('a -> bool) -> 'a list -> ('a * 'a list) *)
let prendi predicato lst =
  if not (List.exists predicato lst) then
    failwith "prendi"
  else
    let paolo = List.find predicato lst in
    let rec remove paolo = function
    | [] -> []
    | x::rest ->
      if paolo=x then
        (* qui metto `rest` invece che `remove paolo rest`
           per rimuovere solo il primo caso *)
        rest
      else x::remove paolo rest
    in
  (paolo, remove paolo lst)