(* basic unit literal test *)
let _ =
  let u : unit = () in
  assert (u = ())

(* basic int literal test *)
let _ =
  let n : int = -12320 in
  assert (n = (-12320))

(* basic float literal test *)
let _ =
  let n : float = 023.0324 in
  assert (n = 23.0324)

(* basic bool literal test *)
let _ =
  let b = true in
  let b : bool = false in
  assert true

(* basic list literal test *)
let _ =
  let l = [] in
  let l = [1;2;3] in
  let l : bool list = true :: false :: true :: [] in
  assert (l = true :: false :: true :: [])

(* basic option literal test *)
let _ =
  let op = None in
  let op : int option = None in
  let op : bool option = Some true in
  assert (op = Some true)

(* basic pair literal test *)
let _ =
  let p = (1, 2) in
  let p : unit * int = ((), -20) in
  assert (p = ((), -20))

(* basic fun literal test *)
let _ =
  let id = fun x -> x in
  let id : int -> int = fun x -> x in
  let id : unit -> unit = fun (x : unit) -> x in
  let k = fun x y -> x in
  assert (k 2 true = 2)

(* basic type variable test *)
let f (x : int) : 'a = x
let g (x : 'a) : int = x
let _ = assert (f 20 = g 20)

(* basic add test *)
let _ =
  let sum = 30 + 35 in
  assert (sum = 65)

(* basic sub test *)
let _ =
  let diff = 30 - 35 in
  assert (diff = (-5))

(* basic mul test *)
let _ =
  let mul = 30 * 2 in
  assert (mul = 60)

(* basic div test *)
let _ =
  let div : int = 30 / 2 in
  assert (div = 15)

(* basic mod test *)
let _ =
  let m : int = 30 mod 7 in
  assert (m = 2)

(* basic add float test *)
let _ =
  let sum : float = 2. +. 3. in
  assert (sum = 5.)

(* basic sub float test *)
let _ =
  let diff : float = 2. -. 3. in
  assert(diff = (-1.))

(* basic mul float test *)
let _ =
  let mul = 2. *. 3. in
  assert (mul = 6.)

(* basic div float test *)
let _ =
  let div = 2. /. 3. in
  assert (div = 2. /. 3.)

(* basic pow float test *)
let _ =
  let pow = 2. ** 3. in
  assert (pow = 8.)

(* basic concat test *)
let _ =
  let l : int list = [] in
  let r = [1;2;3] in
  assert (l @ r @ r = [1;2;3;1;2;3])

(* basic lt test *)
let _ =
  let _ = assert (3 < 4) in
  let _ = assert (3. < 4.) in
  let _ = assert (false < true) in
  let _ = assert ([3;3] < [3;4]) in
  ()

(* basic lte test *)
let _ =
  let _ = assert (344. <= 344.) in
  let _ = assert (Some 3 < Some 4) in
  ()

(* basic gt test *)
let _ = assert ((2, 3) > (2, 1))

(* basic gte test *)
let _ = assert (() >= ())

(* basic neq test *)
let _ =
  let _ = assert (3 <> 4) in
  let _ = assert (true <> false) in
  ()

(* basic and test *)
let _ = assert (true && true)

(* basic or test *)
let _ =
  let _ = assert (true || false) in
  let _ = assert (false || true) in
  let _ = assert (true || true) in
  ()

(* basic if test *)
let _ =
 let b = true in
 let e1 x = x + 1 in
 let e2 x = x - 1 in
 let f = if b then e1 else e2 in
 assert (f 3 = 4)

(* basic list match *)
let _ =
  let hd l =
    match l with
    | x :: _ -> x
    | [] -> -1
  in assert (hd [1;20;-3] = 1)

(* basic option match *)
let _ =
  let hd l =
    match l with
    | x :: _ -> Some x
    | [] -> None
  in
  let out =
    match hd [1;2;3] with
    | Some n -> n
    | None -> -1
  in assert (out = 1)

(* basic pair match *)
let _ =
  let fst p =
    match p with
    | x, _ -> x
  in
  let snd p =
    match p with
    | _, y -> y
  in
  let p = (20, 30.) in
  ( assert (fst p = 20)
  , assert (snd p = 30.)
  )

(* basic app test *)
let _ =
  let id1 x = x in
  let id2 x = x in
  let id3 x = x in
  assert (id1 id2 id3 3 = 3)

(* basic annot test *)
let _ =
  let x : int = (2 : int) in
  assert ((x : int) = (2 : int))

(* basic type var test *)
let _ =
  let x : 'a = 2 in
  let y : 'b = 3 in
  assert (x + 1 = y)

(* stdlib functions *)

let not b = if b then false else true

(* accessor functions for pairs *)
let fst p = match p with | x, _ -> x
let snd p = match p with | _, y -> y

let _ =
  let p = (17, true) in
  assert (p = (fst p, snd p))

(* length of a list *)
let rec length l =
  match l with
  | _ :: l -> 1 + length l
  | [] -> 0

let _ = assert (length [1;2;3;4;5] = 5)
let _ = assert (length [] = 0)
let _ = assert (length [true;false;true] = 3)

(* nth element of a list *)
let nth_opt =
  let rec go l i =
    match l with
    | x :: l ->
      if i = 0
      then Some x
      else go l (i - 1)
    | [] -> None
  in go

let _ = assert (nth_opt [1;2;3;4;5] 3 = Some 4)
let _ = assert (nth_opt [();();()] (-1) = None)
let _ = assert (nth_opt [true;false;true] 5 = None)

(* reverse a list *)
let rec rev l =
  match l with
  | x :: l -> rev l @ [x]
  | [] -> []

let _ = assert (rev [] = [])
let _ = assert (rev [1.;3.;5.] = [5.;3.;1.])

(* concatenate a lists of lists *)
let concat =
  let rec go ls =
    match ls with
    | l :: ls -> l @ go ls
    | [] -> []
  in go

let _ = assert (concat [[1;2;3];[4;5;6];[7;8;9]] = [1;2;3;4;5;6;7;8;9])
let _ = assert (concat [[true];[false]] = [true;false])

(* list mapping *)
let map f =
  let rec go l =
    match l with
    | x :: l -> f x :: go l
    | [] -> []
  in go

let _ = assert (map (fun x -> x + 1) [1;2;3] = [2;3;4])

(* list filtering *)
let filter f =
  let rec go l =
    match l with
    | x :: l ->
      if f x
      then x :: go l
      else go l
    | [] -> []
  in go

let _ = assert (filter (fun x -> x > 0) [-1;2;0;34;2] = [2;34;2])

(* list folding *)
let fold_left op =
  let rec go acc l =
    match l with
    | x :: l -> go (op acc x) l
    | [] -> acc
  in go

let fold_right op =
  let rec go l base =
    match l with
    | x :: l -> op x (go l base)
    | [] -> base
  in go

let _ = assert (fold_left (fun x y -> x - y) 0 [1;2;3] = (-6))
let _ = assert (fold_right (fun x y -> x - y) [1;2;3] 0 = 2)

(* option predicates *)
let is_some o =
  match o with
  | Some _ -> true
  | None -> false

let is_none o = not (is_some o)

(* absolute value *)
let abs n = if n < 0 then 0 - n else n
let abs_float n = if n < 0. then 0. -. n else n

(* Newton's method a la SICP *)
let newtons_method g =
  let tolerance = 0.00001 in
  let close_enough v1 v2 = abs_float (v1 -. v2) < tolerance in
  let fixed_point f =
    let rec go guess =
      let next = f guess in
      if close_enough guess next
      then next
      else go next
    in go
  in
  let dx = 0.00001 in
  let deriv g x = (g (x +. dx) -. g x) /. dx in
  let newton_transform g x = x -. (g x /. deriv g x) in
  fixed_point (newton_transform g)

let sqrt n = newtons_method (fun y -> y *. y -. n) 1.

let _ = assert (abs_float (sqrt 4. -. 2.) < 0.0001)
let _ = assert (abs_float (sqrt 2. -. 1.414213) < 0.0001)
