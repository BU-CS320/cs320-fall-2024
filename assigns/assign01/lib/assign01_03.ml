(* Check if a number is prime *)
let is_prime n =
  if n <= 1 then false
  else
    let rec check i =
      if i * i > n then true
      else if n mod i = 0 then false
      else check (i + 1)
    in
    check 2

(* Function to generate primes up to the required index *)
let generate_primes limit =
  let rec aux n primes =
    if List.length primes > limit then primes
    else if is_prime n then aux (n + 1) (n :: primes)
    else aux (n + 1) primes
  in
  List.rev (aux 2 [])

(* Function to extract the i-th element from the encoded sequence *)
let nth s i =
  let primes = generate_primes (i + 1) in
  let rec aux s i = function
    | [] -> 0
    | prime :: rest ->
      let rec count_exponent s exponent =
        if s mod prime = 0 then count_exponent (s / prime) (exponent + 1)
        else exponent
      in
      let exponent = count_exponent s 0 in
      if i = 0 then exponent
      else aux s (i - 1) rest
  in
  aux s i primes
