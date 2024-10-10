module type INT_SET = sig
  type t
  val mem : int -> t -> bool
  val empty : t
  val singleton : int -> t
  val card : t -> int
  val union : t -> t -> t
end
