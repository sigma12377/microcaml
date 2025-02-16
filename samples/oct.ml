let oct = fun num ->
  let modu = fun x -> fun y -> x - (x/y) * y in
  let rec aux = fun n -> fun k -> fun acc ->
    if n = 0 then acc
    else ((aux (n/8)) (k*10)) (k * ((modu n) 8) + acc)
  in
  ((aux num) 1) 0
in
oct 7777