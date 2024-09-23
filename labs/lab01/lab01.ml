(* Modular Arithmetic (Written Problem)

   Implement the function `modulo` of type `int -> int -> int` which,
   given

   n : a nonnegative integer
   k : a positive integer

   returns the value `n mod k` WITHOUT using the operator `(mod)`.

   Bonus Question: Is your solution tail recursive? Explain why or why not.

   Note to TF/TAs: Breifly talk about what makes a function
   tail-recursive when going over the solution.
*)

let rec modulo (n : int) (k : int) : int =
  if n < k then n
  else
    modulo (n - k) k

(* Perfect numbers

   A positive integer `n` is perfect if it is equal to the sum of its
   proper divisors.

   Please implement the function `is_perfect` of type `int -> bool`
   which, given an positive integer `n`, returns `true` if `n` is
   perfect and `false` otherwise.

   Examples:
   let _ = assert (is_perfect 6)        (* 1 + 2 + 3 = 6 *)
   let _ = assert (is_perfect 28)       (* 1 + 2 + 4 + 7 + 14 = 28 *)
   let _ = assert (not (is_perfect 24)) (* 1 + 2 + 3 + 4 + 6 + 8 + 12 <> 24 *)

 *)

let is_perfect (n : int) : bool =
  let rec loop i acc =
    if i < n then
      if n mod i = 0
      then loop (i + 1) (acc + i)
      else loop (i + 1) acc
    else acc
  in
  n = loop 1 0

(* String Triangle

   Implement the function `string_tri` of type `string -> string`
   which given

   s : string conisting only of capital english letters [A-Z]

   returns the string with the same characters but organized as a
   right triangle.  If the last line as too few letters, then pad it with '*'.

   Examples:
   let _ = assert (string_tri "ABCDE" = "A\nBC\nDE*")
   let _ = assert (string_tri "ABCDEFGH" = "A\nBC\nDEF\nGH**")
   let _ = assert (string_tri "AAA" = "A\nAA")

*)

let string_tri s = 
  let l = String.length s in
  let rec loop i j acc =
    let sz = ((j + 1) * j) / 2 in
    if i < l then
      let c = String.sub s i 1 in
      if i < sz
      then loop (i + 1) j (acc ^ c)
      else loop i (j + 1) (acc ^ "\n")
    else
      acc ^ String.make (sz - i) '*'
  in
  loop 0 1 ""

(* Pythagorean Triples

   Implement the function `py_trip_hyp` of type `int -> bool` which
   given

   n : nonnegative integer

   returns `true` if `n` can be the hypotenuse of a right triangle
   with integer side lengths (i.e., there are `a` and `b` such that `a
   * a + b * b = n * n` is `true`).

   Examples:
   let _ = assert (py_trip_hyp 5)
   let _ = assert (py_trip_hyp 13)
   let _ = assert (py_trip_hyp 17)
   let _ = assert (py_trip_hyp 29)
   let _ = assert (not (py_trip_hyp 28))
   let _ = assert (not (py_trip_hyp 6))

*)

let perfect_sq x =
  let rec loop i =
    if i <= x / 2 then
      if i * i = x then true
      else loop (i + 1)
    else false
  in
  loop 0

let py_trip_hyp n =
  let n2 = n * n in
  let rec loop i =
    if i < n then
      let m2 = n2 - i * i in
      if perfect_sq m2 then true
      else loop (i + 1)
    else false
  in
  loop 1
