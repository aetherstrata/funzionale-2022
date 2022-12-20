type 'a ntree = Ntree of 'a * 'a ntree list

let rec postorder = function
| Ntree (x,[]) -> [x]
| Ntree (x,list) -> List.flatten( List.map (fun sub -> postorder sub) list) @ [x]

let rec inorder = function
| Ntree (x,[]) -> [x]
| Ntree (x,left::rest) -> inorder left @ [x] @ List.flatten( List.map (fun sub -> inorder sub) rest)