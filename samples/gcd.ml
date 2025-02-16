let rec gcd = fun a -> fun b ->
  if a = b then a
  else if a > b then (gcd (a-b)) b
  else (gcd a) (b-a)
in
(gcd 8064) 256