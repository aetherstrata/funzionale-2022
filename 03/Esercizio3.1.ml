exception Invalid_Date of string;;

(* (int * int ) -> (int * int ) -> int * int *)
let somma_ore (o1,m1) (o2,m2) =
  if (o1 < 0 || o1 > 23
   || o2 < 0 || o2 > 23
   || m1 < 0 || m1 > 59
   || m2 < 0 || m2 > 59)
  then
    raise(Invalid_Date "The date is invalid!")
  else
    let (minutiTotali1 , minutiTotali2) = (o1*60 + m1 , o2*60 + m2) in
      (minutiTotali1 + minutiTotali2) / 60, (minutiTotali1 + minutiTotali2) mod 60