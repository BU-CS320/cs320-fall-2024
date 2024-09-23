let sqrt n =
  let rec go k =
    if k * k < n
    then go (k + 1)
    else k
  in go 0

let is_prime n =
  let s = sqrt n in
  let rec go k =
    if k > s
    then true
    else if n mod k = 0
    then false
    else go (k + 1)
  in n = 2 || go 2

let next_prime k =
  let rec go n =
    if is_prime n
    then n
    else go (n + 1)
  in go (k + 1)

let rec nth_prime n =
  if n <= 0
  then 2
  else next_prime (nth_prime (n - 1))
