let quadrupla = (5,('c',"antonio",(),if 3>4 then 0 else 1),"pippo",true)

(* 'a*'b*'c*'d -> 'a *)
let pi1 (x,_,_,_) = x

(* 'a*'b*'c*'d -> 'b *)
let pi2 (_,x,_,_) = x

(* 'a*'b*'c*'d -> 'c *)
let pi3 (_,_,x,_) = x

(* 'a*'b*'c*'d -> 'd *)
let pi4 (_,_,_,x) = x

let pippo = pi3 (pi2 quadrupla)