open OUnit2
open Assign02_03

(* We'll assume unnecessarily large error *)
let is_close f1 f2 =
  abs_float (f1 -. f2) <= 0.0000001

let test p p_name e =
  let d =
    String.concat " "
      [ "testing dist"
      ; p_name
      ]
  in
  let a = dist p in
  let t _ = assert_bool "not close to correct distance" (is_close a e) in
  d >:: t

let p1 = [North; North; South; South]
let p2 = [North; North; South; East; East; East]
let p3 = [North; North; North; West; West; West; West]

let tests =
  "basic dist examples" >:::
    [ test p1 "p1" 0.
    ; test p2 "p2" (sqrt 10.)
    ; test p3 "p3" (sqrt (9. +. 16.))
    ]
