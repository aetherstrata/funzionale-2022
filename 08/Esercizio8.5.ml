type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree

let rec segui_bool path = function
  | Empty -> failwith "segui_bool"
  | Tr (x,a,b) ->
    match path with
    | [] -> x
    | y::rest ->
      if y then
        segui_bool rest a
      else
        segui_bool rest b