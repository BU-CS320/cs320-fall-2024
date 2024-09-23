let is_prime n =
  let rec go i =
    if i = n
    then true
    else if n mod i = 0
    then false
    else go (i + 1)
  in
  if n < 2
  then false
  else go 2
