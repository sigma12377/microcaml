let rec binomial = fun n -> fun k ->
  if k = 0 then 1
  else if n = k then 1
  else (binomial (n-1)) (k-1) + (binomial (n-1)) k
in
(binomial 30) 7
