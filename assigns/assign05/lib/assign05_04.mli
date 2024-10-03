open Assign05_04_intf

module ListSet : INT_SET with type t = int list

type set_info =
  { ind : int -> bool
  ; mn : int
  ; mx : int
  }

module FuncSet : INT_SET with type t = set_info
