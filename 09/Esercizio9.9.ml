type 'a ntree = Ntree of 'a * 'a ntree list

let is_prime =
  let rec aux div n =
    if div = 1 then
      true
    else
      n mod div <> 0 && aux (div - 1) n
  in
  function
  | 0 | 1 -> false
  | n ->  aux (n - 1) n

let rec ramo_primi = function
  | Ntree(n,[]) -> is_prime n
  | Ntree(n,tlist) -> is_prime n && List.exists ramo_primi tlist