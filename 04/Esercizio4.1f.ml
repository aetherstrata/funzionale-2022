let rec reverse_aux a b =
    match b with
    | [] -> a
    | x::rest -> reverse_aux (x::a) rest

let reverse lst = reverse_aux [] lst
