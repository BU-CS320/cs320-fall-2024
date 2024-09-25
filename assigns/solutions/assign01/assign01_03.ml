open Assign01_02

let nth seq n =
  let p = nth_prime n in
  let rec go seq out =
    if seq mod p = 0
    then go (seq / p) (out + 1)
    else out
  in go seq 0
