
let sum =
  let rec go acc l =
    match l with
    | [] -> acc
    | x :: l -> go (acc + x) l
  in go 0

let gen_fib =
  let rec go l k =
    match l, k with
    | x :: _, 0 -> x
    | _ :: xs, _ -> go (xs @ [sum l]) (k - 1)
    | [], _ -> assert false
  in go

(* a fast version *)
let gen_fib_again =
  let rec cont l1 l2 k =
    match l1, l2, k with
    | _, out :: _, 0 -> out
    | [x], y :: _, _ -> cont (List.rev l2) [2 * y - x] (k - 1)
    | x :: xs, y :: _, _ -> cont xs (2 * y - x :: l2) (k - 1)
    | _ -> assert false
  in
  let go l k =
    let rec init xs sum k =
      match xs, k with
      | x :: _, 0 -> x
      | x :: xs, _ -> init xs (x + sum) (k - 1)
      | [], k -> cont l [sum] k
    in init l 0 k
  in go
