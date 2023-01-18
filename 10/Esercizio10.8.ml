type 'a graph = ('a * 'a) list

let succ_list n g =
  List.map (fun (x, y) -> y) (List.filter (fun (x, y) -> x = n) g)

let valid_path graph start goal =
  let rec search visited = function
    | [] -> false
    | x :: rest ->
        if List.mem x visited then
          search visited rest
        else
          x = goal || search (x :: visited) (succ_list x graph @ rest)
  in
  search [] [ start ]

let connessi_in_glist g_list start goal =
  List.exists (fun g -> valid_path g start goal) g_list