type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree

let path p tree =
  let rec aux = function
    | Empty -> true
    | Tr (x, a, b) ->
        if p x then
          false
        else
          aux a || aux b
  in
  if aux tree then true else failwith "path"