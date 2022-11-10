let rec stringa_max() =
  let len = String.length (read_line ()) in
  if len = 0 then 0
  else
    let x = stringa_max() in
    if len > x then len
  else x