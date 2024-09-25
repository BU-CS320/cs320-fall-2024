(*


Gamma |- m : A        Gamma, x : A |- n : B
--------------------------------------------
Gamma |- let x = m in n : B





. |- 1 : int    . |- 1 : int      x : int |- x : int      x : int |- 1 : int
----------------------------      -------------------------------------------
. |- (1 + 1) : int                x : int |- (x + 1) : int
-----------------------------------------------------------
. |- let x = (1 + 1) in (x + 1) : int



m $ v1       [v1/x]n $ v2
--------------------------
let x = m in n $ v2



m $ v1   n $ v2
-------------------
m + n $ (v1 $+ v2)



1 $ 1    1 $ 1
---------------
(1 + 1) $ 2       1 $ 1
------------------------
((1 + 1) + 1) $ 3              (3 + 1) $ 4
-------------------------------------------
let x = (1 + 1 + 1) in (x + 1) $ 4



*)


type mylist =
  | Nil
  | Cons of int * mylist

let zs = Cons (1, Cons (2, Cons (3, Nil)))


type tree =
  | Leaf
  | Node of tree * int * tree

let tr = Node (Leaf, 2, Leaf)
let tr = Node (tr, 2, tr)


type animal =
  | Dog of string
  | Cat of string * int
  | Cow of string * float


let animal1 = Dog "Doug"
let animal2 = Cat ("Sally", 25)


let get_name (x : animal) : string =
  match x with
  | Dog name -> name
  | Cat (name, temp) -> name
  | Cow (name, weight) -> name



let string_explode (s : string) : char list =
  let l = String.length s in
  List.init l (fun i -> String.get s i)




let rec list_append (xs : int list) (ys : int list) : int list =
  match xs with
  | [] -> ys
  | x :: rest -> x :: list_append rest ys




let xs = List.init 100000000 (fun i -> i)



let rec revapp (xs : int list) (ys : int list) : int list =
  match xs with
  | [] -> ys
  | x :: rest -> revapp rest (x :: ys)


(* [1;2;3] = 1 :: (2 :: (3 :: [])) *)


let rev (xs : int list) : int list =
  revapp xs []


let list_append_trec (xs : int list) (ys : int list) : int list =
  revapp (rev xs) ys