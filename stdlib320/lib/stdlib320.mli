(**
   The following is the standard library that we will use for {i CAS
   CS 320: Concepts of Programming Languages} at Boston University in
   the Fall 2024 semester ({i Note.} This is the first time we're doing this,
   so it's something of an experiment).

   The primary motivation for this library is to give a small subset
   of the OCaml Standard Library with a bit more documentation and
   more control on the end of the instructor to decide what functions
   are "allowed" for assignments.  It is also minimally
   side-effectful.

   Since nearly all functions are available in the OCaml Standard
   Library, you can take a look at
   {{:https://ocaml.org/manual/5.2/api/Stdlib.html}[Stdlib]
   documentation} for more information on what you see below.

 *)

(** {1 Integer arithmetic} *)

(** The parentheses in functions below indicate that the following
    functions are {i infix} operators.  This means we can, for
    example, write [11 + 13] instead of [(+) 11 13] (though both are
    allowed). The associativity and precedence of all operators follow
    those of the OCaml Standard library.  See
    {{:https://ocaml.org/manual/5.2/api/Ocaml_operators.html}this
    table} for details. *)

val ( + ) : int -> int -> int
(** Integer addition *)

val ( - ) : int -> int -> int
(** Integer subtraction *)

val ( * ) : int -> int -> int
(** Integer multiplication *)

val ( / ) : int -> int -> int
(** Integer division, which rounds towards 0.

    {[
    let _ =
      let _ = assert (5 / 3 = 1) in
      let _ = assert ((- 5) / 3 = -1) in
      let _ = assert (5 / (- 3) = 1) in
      ()
    ]}

Raises a [Division_by_zero] exception if the second argument is 0 ({i
Note.} We won't be dealing with exceptions all that much in this
course). *)

val abs : int -> int
(** Integer absolute value *)

val ( mod ) : int -> int -> int
(** Integer remainder.  It satisfies the properties:
    - [x mod y = x mod (abs y)]
    - [(- x) mod y = - (x mod y)]

    And for positive integers it is the remainder of [x / y] in the
    grade-school sense.  *)

val max_int : int
(** The greatest representable integer *)

val min_int : int
(** The smallest representable integer *)

(** {1 Floating-point arithmetic}

From the OCaml Standard Library: "OCaml's floating-point numbers
follow the IEEE 754 standard, using double precision (64 bits)
numbers. Floating-point operations never raise an exception on
overflow, underflow, division by zero, etc. Instead, special IEEE
numbers are returned as appropriate..." See
{{:https://ocaml.org/manual/5.2/api/Stdlib.html#1_Floatingpointarithmetic}this
paragraph} for more details. *)

val ( +. ) : float -> float -> float
(** Floating-point addition (Note the [.]!) *)

val ( -. ) : float -> float -> float
(** Floating-point subtraction *)

val ( *. ) : float -> float -> float
(** Floating-point multiplication *)

val ( /. ) : float -> float -> float
(** Floating-point division *)

val ( ** ) : float -> float -> float
(** Exponentiation *)

val sqrt : float -> float
(** Square root *)

val exp : float -> float
(** Exponential *)

val log : float -> float
(** Natural logarithm *)

val log10 : float -> float
(** Base 10 logarithm *)

val ceil : float -> float
(** Ceiling, i.e., round up to an integer (returned as a [float]) *)

val floor : float -> float
(** Floor, i.e., round down to an integer *)

val abs_float : float -> float
(** Floating point absolute value *)

(** {1 Boolean operations} *)

val not : bool -> bool
(** Boolean negation *)

val ( && ) : bool -> bool -> bool
(** Boolean conjunction (and) *)

val ( || ) : bool -> bool -> bool
(** Boolean disjunction (or) *)

(** {1 Comparing} *)

val ( = ) : 'a -> 'a -> bool
(** Structural equality. This is [==] in most other languages.  You
    should generally avoid using this for floating-point numbers. *)

val ( <> ) : 'a -> 'a -> bool
(** Structural inequality, i.e., [x <> y] is equivalent to [not (x = y)] *)

val ( < ) : 'a -> 'a -> bool
(** Structural less-than.  It behaves as expected on Boolean values ([false < true]), characters (by integer value), string (lexicographically), etc.  *)

val ( > ) : 'a -> 'a -> bool
(** Structural greater-than *)

val ( <= ) : 'a -> 'a -> bool
(** Structural less-than-or-equal-to, i.e., [x <= y] is equivalent to [not (x > y)] *)

val ( >= ) : 'a -> 'a -> bool
(** Structural greater-than-or-equal-to *)

val compare : 'a -> 'a -> int
(** Comparison, i.e., if [x = y] then [compare x y = 0], if [x > y] then
    [compare x y > 0], and if [x < y] then [compare x y < 0] *)

val min : 'a -> 'a -> 'a
(** Minimum of its two arguments *)

val max : 'a -> 'a -> 'a
(** Maximum of its two arguments *)

(** {1 Converting} *)

val float_of_int : int -> float
(** Converts an integer into a floating-point number. *)

val int_of_float : float -> int
(** Converts a floating-point number into an integer, rounding towards 0 (i.e., truncating). *)

val int_of_char : char -> int
(** Converts a character into its ASCII code. *)

val char_of_int : int -> char
(** Converts an ASCII code into a character.

    Raises an [Invalid_argument] exception if its argument is not a valid ASCII code.
 *)

val string_of_bool : bool -> string
(** Converts a Boolean value to a string. *)

val bool_of_string : string -> bool
(** Converts a string to a Boolean value.  Raises a [Failure] exception
    in the case the argument is not ["true"] or ["false"]. *)

val bool_of_string_opt : string -> bool option
(** Same as the previous function, but returns [None] in the case of failure. *)

val string_of_int : int -> string
(** Converts an integer to a string *)

val int_of_string : string -> int
(** Converts a string to an integer.  Raises a [Failure]exception in
    the case the argument does not represent a integer. *)

val int_of_string_opt : string -> int option
(** Same as the previous function, but returns [None] in the case of failure. *)

(** {1 Composing} *)

val ( |> ) : 'a -> ('a -> 'b) -> 'b
(** [x |> f |> g] is equivalent to [g (f x)].  This operator is useful
    for
    {{:https://cs3110.github.io/textbook/chapters/basics/functions.html#pipeline}pipelining}. *)

val ( @@ ) : ('a -> 'b) -> 'a -> 'b
(** [f @@ g @@ x] is equivalent to [f (g x)].  This operator has low
    precedence, which is useful if you want to avoid parentheses,
    e.g., we can write [abs @@ 2 + 3] instead of [abs (2 + 3)]. *)

(** {1 IO} *)

(** {2 Printing} *)

val print_int : int -> unit
(** Prints an integer to standard output. *)

val print_float : float -> unit
(** Prints a floating-point number to standard output. *)

val print_char : char -> unit
(** Prints a character to standard output. *)

val print_string : string -> unit
(** prints a string to standard output. *)

val print_endline : string -> unit
(** Prints a string followed by a newline character to standard output. *)

val print_newline : unit -> unit
(** Prints a newline character. *)

val print_byte : int -> unit
(** Prints a byte, where the argument is taken modulo 256. *)

(** {2 Reading} *)


val read_file : string -> string
(** Reads the contents of a file given the filename. {b We will primarily be using this function.} *)

val read_line : unit -> string
(** Reads a line (upto a newline character) from standard input, where
    the output does not include a the newline character. *)

val read_byte : unit -> int
(** Reads a byte from standard input. *)

(** {1 Course-Specific Utilities} *)

val gensym : unit -> string
(** Generates a string that can be used as a fresh variable in the
    interpreters we implement in this course. *)

type 'a env
(** Type for a map (i.e. dictionary) structure with strings for
    keys *)

module Env : sig

  (** {1 Basic functions} *)

  val empty : 'a env
  (** Empty map *)

  val is_empty : 'a env -> bool
  (** Equivalent to [(=) empty]. *)

  val mem : string -> 'a env -> bool
  (** [mem s e] is
      - [true] if [s] is a key in [e]
      - [false] otherwise *)

  val add : string -> 'a -> 'a env -> 'a env
  (** [add s x e] is the same as [e] except that [s] maps to [x]. *)

  val remove : string -> 'a env -> 'a env

  (** [remove s e] is the same as [e] except that [s] does not map to anything. *)

  val union : (string -> 'a -> 'a -> 'a option) -> 'a env -> 'a env -> 'a env
  (** [union f e1 e2] is the combination of [e1] and [e2], using [f] as the combiner.  More formally, if [mem s e1] and [mem s e2] then
      - [find s (union f e1 e2) = x] if [f (find s e1) (find s e2) = Some x]
      - [not (mem s (union f e1 e2)] otherwise
   *)

  (** {1 Higher-order functions} *)

  val map : ('a -> 'b) -> 'a env -> 'b env
  (** [map f e] is [e] with [f] applied to each value.  More formally, [find_opt s (map f e) = Option.map (find s e)]. *)

  val filter : (string -> 'a -> bool) -> 'a env -> 'a env
  (** [filter f e] is [e] with the only the mappings [(k, v)] such that [f k v] holds. *)

  (** {1 Finding} *)

  val find : string -> 'a env -> 'a
  (** [find s e] is [v] if [mem s e] and [s] maps to [v] in [e]

      Raises a [Not_found] exception otherwise.
   *)

  val find_opt : string -> 'a env -> 'a option
  (** Same as the previous function, but is [None] in the case of failure. *)

  (** {1 Converting} *)

  val to_list : 'a env -> (string * 'a) list
  (** [to_list e] is a list of the key-value pairs in [e].  More formally, [find_opt s e = assoc_opt s (to_list e)]. *)

  val of_list : (string * 'a) list -> 'a env
                                         (** [of_list l] is an map in which each key value pair in [l] is added to the empty map, in order from left to right.  In particular, if a key is mapped to multiple values in [l], then only the {i last} appears in the [of_list l]. *)
end

(** {1 Basic OCaml Types} *)

(** {2 Pairs} *)

val fst : 'a * 'b -> 'a
val snd : 'a * 'b -> 'b

(** {2 Strings} *)

val ( ^ ) : string -> string -> string
(** String concatenation, e.g., ["hello" ^ "world" ^ "!" = "hello world!"] *)

module Char : sig
  val lowercase_ascii : char -> char

  (** [lowercase_ascii c] is the lowercase form of [c] if [c] is a letter, and is [c] otherwise.

      {[
      let _ =
        let _ = assert (lowercase_ascii 'A' = 'a') in
        let _ = assert (lowercase_ascii 'a' = 'a') in
        let _ = assert (lowercase_ascii '2' = '2') in
        ()
      ]}
   *)

  val uppercase_ascii : char -> char
  (** [uppercase_ascii c] is the uppercase form of [c] if [c] is a letter, and is [c] otherwise.

      {[
      let _ =
        let _ = assert (lowercase_ascii 'a' = 'A') in
        let _ = assert (lowercase_ascii 'A' = 'A') in
        let _ = assert (lowercase_ascii '2' = '2') in
        ()
      ]}
   *)
end

module String : sig

  (** {1 Basic functions} *)

  val init : int -> (int -> char) -> string
  (** Creates a string based on an index function. [init n f] is a
      string of length [n] whose [i]th character is [f i].

      {[
      let _ =
        let _ = assert (init 3 (fun x -> char_of_int (x + 65)) = "ABC") in
        let _ = assert (init 5 (fun _ -> 'z') = "zzzzz") in
        ()
      ]}
   *)

  val length : string -> int
  (** [length s] is the number of characters in [s], e.g., [length "ABC" = 3]. *)

  val get : string -> int -> char
  (** Gets the character in a given string at a given index. [get s i]
      is the [i]th character of [s] if [i >= 0] and [i < length s].
      It is equivalent to writing [s.[i]].

      Raises an [Invalid_argument] exception otherwise. *)

  val concat : string -> string list -> string
  (** Combines a list of string with a given delimiter, e.g., [concat "," ["A";"B";"C"] = "A,B,C"] *)

  (** {1 Higher-order functions}

      These work the same as they would on lists of char, e.g., [map f s = List.map f (to_list s)].

   *)

  val map : (char -> char) -> string -> string
  val fold_left : ('acc -> char -> 'acc) -> 'acc -> string -> 'acc
  val fold_right : (char -> 'acc -> 'acc) -> string -> 'acc -> 'acc

  (** {1 Modifying} *)

  val trim : string -> string
  (** Removes whitespace from the beginning and end of its argument. *)

  val uppercase_ascii : string -> string
  (** Equivalent to [map Char.upper_case_ascii] *)

  val lowercase_ascii : string -> string
  (** Equivalent to [map Char.lower_case_ascii] *)

  (** {1 Indexing} *)

  val index : string -> char -> int
  (** [index s c] is the index of the first occurrence of [c] in [s].

      If [c] does not appear in [s], then it raises an
      [Invalid_argument] expection.
   *)

  val index_opt : string -> char -> int option
  (** Same as the above function, but is [None] in the case of
      failure. *)

  val to_list : string -> char list
  (** Converts a string to a list of characters, e.g., [to_list "ABC" = ['A';'B';'C']] *)

  val of_list : char list -> string
  (** Converts a list of characters to a string, e.g., [of_list ['A';'B';'C'] = "ABC"] *)
end

(** {2 Lists} *)

val ( @ ) : 'a list -> 'a list -> 'a list
(** List concatenation, e.g., [[1;2;3] @ [4;5;6] @ [7;8;9] = [1;2;3;4;5;6;7;8;9]] *)

module List : sig

  (** {1 Basic functions} *)

  val length : 'a list -> int
  (** [length l] is the number of elements in [l], e.g, [length [1;2;3] = 3] *)

  val nth : 'a list -> int -> 'a
  (** [nth l i] is the [i]th element of [l] if [i >= 0] and [i < length l].

      Raises an exception otherwise.
   *)

  val nth_opt : 'a list -> int -> 'a option
  (** Same as the previous function, but is [None] in the case of failure. *)

  val rev : 'a list -> 'a list
  (** Reverses a list, e.g., [rev [1;2;3] = [3;2;1]] *)

  val concat : 'a list list -> 'a list
  (** Combines a list of lists from left to right, e.g., [concat [[1;2;3];[4;5;6];[7;8;9]] = [1;2;3;4;5;6;7;8;9]] *)

  (** {1 Higher-order functions} *)

  val map : ('a -> 'b) -> 'a list -> 'b list
  (** Mapping function for lists. [map f l] applies [f] to every element of [l], e.g, [map abs [-1;-2;-3] = [1;2;3]]. *)

  val filter : ('a -> bool) -> 'a list -> 'a list
  (** Filtering function for lists. [filter f l] is the element of [l] (in order) which satisfy the predicate [f], e.g, [filter ((<) 5) [3;4;5;6;7] = [6;7]]. *)

  val fold_left : ('acc -> 'a -> 'acc) -> 'acc -> 'a list -> 'acc
  (** Left-associative folding function for lists. [fold_left op init [x_1;x_2;...;x_n]] is equivalent to [op (...(op (op init x_1) x_2)...) x_n].
   *)

  val fold_right : ('a -> 'acc -> 'acc) -> 'a list -> 'acc -> 'acc
  (** Right-associative folding function for lists. [fold_right op [x_1;x_2;...;x_n]] is equivalent to [op x_1 (op x_2 (...(op x_n base)...))].
   *)

  val rev_map : ('a -> 'b) -> 'a list -> 'b list
  (** [rev_map f l] is equivalent to [rev (map f l)] *)

  val filter_map : ('a -> 'b option) -> 'a list -> 'b list
  (** [filter_map f l] is equivalent to the list [filter Option.is_some (map f l)] but with the [Some] constructors removed from each element. *)

  val concat_map : ('a -> 'b list) -> 'a list -> 'b list
  (** [concat_map f l] is equivalent to [concat (map f l)] *)

  (** {1 Finding} *)

  val mem : 'a -> 'a list -> bool
  (** Membership predicate for lists. [mem x l] is
      - [true] if [x] appears in [l]
      - [false] otherwise. *)

  val find : ('a -> bool) -> 'a list -> 'a
  (** Finds based on a predicate.  [find f l] is the first appearance of an element of [l] which satisfies [f].

      Raises a [Not_found] exception otherwise.
   *)

  val find_opt : ('a -> bool) -> 'a list -> 'a option
  (** Same as the previous function, but is [None] in the case of failure. *)


  (** {1 Taking/dropping} *)

  val take : int -> 'a list -> 'a list
  (** Take a prefix of a list.  [take i l] is the list containing the first [min i (length l)] elements of [l], given [i >= 0].

      Raises an [Invalid_argument] exception otherwise.

   *)

  val drop : int -> 'a list -> 'a list
  (** drops the first [min i (length l)] elements of [l] and returns the remaining elements, given [i >= 0].

      Raises an [Invalid_argument] exception otherwise.
   *)

  val take_while : ('a -> bool) -> 'a list -> 'a list
  (** [take_while f l] is the longest prefix of [l] in which all elements satisfy the predicate [f]. *)

  val drop_while : ('a -> bool) -> 'a list -> 'a list
  (** [drop_while f l] is equivalent to [drop (length (take_while f l)) l] *)

  (** {1 Sorting} *)

  val sort : ('a -> 'a -> int) -> 'a list -> 'a list
  (** Generic sorting function for lists. [sort f l] has the same elements as [l], but sorted according to the comparing function [f]. *)

  (** {1 Association lists} *)

  val assoc : 'a -> ('a * 'b) list -> 'b
  (** Membership function for association lists. [assoc x l] is equivalent to

      [snd (find (fun (k, v) -> k = x))] *)

  val assoc_opt : 'a -> ('a * 'b) list -> 'b option
  (** Same as the previous function, but is [None] in the case of failure. *)

  val remove_assoc : 'a -> ('a * 'b) list -> ('a * 'b) list
  (** [remove_assoc k l] is [l] but with the first appearance of a pair of the form [(k, v)] removed. *)

end

(** {2 Options} *)

module Option : sig

  (** {1 Basic functions} *)

  val value : 'a option -> default:'a -> 'a
  (** Gets the value of an option if it is not [None]. [value o ~default:y] is
      - [x] if [o = Some x]
      - [y] if [o = None]
   *)

  val default : 'a -> 'a option -> 'a
  (** A slightly more convenient variant of the previous function, where [default y o] is equivalent to [value o ~default:y]. *)

  val is_none : 'a option -> bool
  (** [is_none o] is
      - [true] if [o = Some x]
      - [false] if [o = None]
   *)

  val is_some : 'a option -> bool
  (** [is_some o] is equivalent to [not (is_none o)] *)

  (** {1 Higher-order functions} *)

  val bind : 'a option -> ('a -> 'b option) -> 'b option
  (** Monadic bind for options.  It "does something" to the value of the option if it is not [None], and passes along the [None] otherwise.  [bind o f] is
      - [f x] if [o = Some x]
      - [None] if [o = None]
   *)

  val join : 'a option option -> 'a option
  (** Monadic join for options.  If collapses an option whose value is an option into a single option. [join oo] is
      - [Some x] if [oo = Some (Some x)]
      - [None] if [oo = Some None] or [oo = None]
   *)

  val map : ('a -> 'b) -> 'a option -> 'b option
  (** Mapping function for options.  It applies a function to the value of the option if the option is not [None]. [map f o] is
      - [Some (f x)] if [o = Some x]
      - [None] if [o = None]
   *)

  val fold : none:'a -> some:('b -> 'a) -> 'b option -> 'a
  (** Folding function for options.  [fold ~none:x some:f o] is equivalent to [default x (map f o)]. *)

  (** {1 Converting} *)

  val to_result : none:'e -> 'a option -> ('a, 'e) result
  (** Converts an option to a result. [to_result ~none:e o] is
      - [Ok x] if [o = Some x]
      - [Error e] if [o = None]
   *)

  val to_list : 'a option -> 'a list
  (** Converts an option into a list with zero or one element. It is equivalent to

      [fold ~none:[] ~some:(fun x -> x :: [])]

    *)
end

(** {2 Results} *)

type ('a, 'b) result = Ok of 'a | Error of 'b

module Result : sig

  (** {1 Basic functions} *)

  val value : ('a, 'e) result -> default:'a -> 'a
  (** Gets the value of result if it is not an error. [value r ~default:y] is
      - [x] if [r = Ok x]
      - [y] if [r = Error e] *)

  val default : 'a -> ('a, 'e) result -> 'a
  (** A slightly more useful variant of [value], where [default y r]
      is equivalent to [value r ~default:y] *)

  val is_ok : ('a, 'e) result -> bool
  (** [is_ok r] is
      - [true] if [r = Ok x]
      - [false] if [r = Error e]
   *)

  val is_error : ('a, 'e) result -> bool
  (** [is_error r] is equivalent to [not (is_ok e)] *)


  (** {1 Higher-order functions} *)

  val bind : ('a, 'e) result -> ('a -> ('b, 'e) result) -> ('b, 'e) result
  (** Monadic bind for results.  It "does something" to the value of
      a result if it is not an error, and passes along the error
      otherwise. [bind r f] is

      - [f x] if [r = Ok x]
      - [Error e] if [r = Error e]
   *)

  val join : (('a, 'e) result, 'e) result -> ('a, 'e) result
  (** Monadic join for results.  It collapses a result whose value is
      result into a single result.  [join rr] is

      - [Ok x] if [rr = Ok (Ok x)]
      - [Error e1] if [rr = Error e1]
      - [Error e2] if [rr = Ok (Error e2)]
   *)

  val map : ('a -> 'b) -> ('a, 'e) result -> ('b, 'e) result
  (** Mapping function for result values.  It applies a function to the
      value of the result if the result is not an error and passes along
      the error otherwise. [map f r] is

      - [Ok (f x)] if [r = Ok x]
      - [Error e] if [r = Error e]
   *)

  val map_error : ('e -> 'f) -> ('a, 'e) result -> ('a, 'f) result
  (** Mapping function for result errors.  Similar to the previous function but on errors.  [map f r] is
      - [Ok x] if [r = Ok x]
      - [Error (f e)] if [r = Error e]
   *)

  val fold : ok:('a -> 'c) -> error:('e -> 'c) -> ('a, 'e) result -> 'c
  (** Folding function for results. Similar to the folding for options. [fold ~ok:f ~error:g r] is
      - [f x] if [r = Ok x]
      - [g e] if [r = Error e]
   *)

  (** {1 Converting} *)

  val to_option : ('a, 'e) result -> 'a option
  (** Converts a result into an option, dropping the error information. [to_option r] is
      - [Some x] if [r = Ok x]
      - [None] if [r = Error e]
   *)

  val to_list : ('a, 'e) result -> 'a list
  (** [to_list r] is equivalent to [Option.to_list (to_option r)] *)
end

(** {1 Extra Utilities}

    There are some functions we've included for completeness, others
    because we silently need them for other things to work, and others
    just in case ({i Note.} Again, this is a new system we're trying
    out, there may be some things we add or remove throughout the
    semester).

 *)

val ignore : 'a -> unit
(** Evaluates its argument and discards it, returning [()].  We will
    use this when we start building interpreters to trace information
    during evaluation in order to maintain that we're writing valid
    OCaml programs. *)

(** {2 Exception Handling}

    Again, we won't do too much exception handling in this course.
 *)

val raise : exn -> 'a
(** Raise an exception *)

val failwith : string -> 'a
(** Raise a [Failure] exception with a given message *)

(** {2 IO} *)

type in_channel
type out_channel

val open_out : string -> out_channel
val flush : out_channel -> unit
val output_string : out_channel -> string -> unit
val output_byte : out_channel -> int -> unit
val close_out : out_channel -> unit

val open_in : string -> in_channel
val input_line : in_channel -> string
val input_byte : in_channel -> int
val close_in : in_channel -> unit

module Filename : sig
  val extension : string -> string
  val remove_extension : string -> string
  val dirname : string -> string
  val basename : string -> string
end

(** {2 Trigonometric Functions } *)

module Float : sig
  val pi : float
end
(** A module containing a floating-point approximation of pi.  It
    is in its own module because we want to be consistent with the
    OCaml Standard Library. *)

val cos : float -> float
val sin : float -> float
val tan : float -> float
val acos : float -> float
val asin : float -> float
val atan : float -> float
val atan2 : float -> float -> float
val hypot : float -> float -> float
val cosh : float -> float
val sinh : float -> float
val tanh : float -> float
val acosh : float -> float
val asinh : float -> float
val atanh : float -> float

(** {2 Randomness} *)

module Random : sig
  val int : int -> int
  val float : float -> float
  val bool : unit -> bool
end

(** {2 Other Modules} *)

module Format : module type of Format
module Printf : module type of Printf
module Sys : module type of Sys
module Lexing : module type of Lexing
module Parsing : module type of Parsing
