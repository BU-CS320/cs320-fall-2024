let is_prime n =
  if n <= 1 then false
  else
    let rec check i =
      if i * i > n then true
      else if n mod i = 0 then false
      else check (i + 1)
    in
    check 2

let nth_prime n =
  let rec aux count current =
    if is_prime current then
      if count = n then current
      else aux (count + 1) (current + 1)
    else
      aux count (current + 1)
  in
  aux 0 2

