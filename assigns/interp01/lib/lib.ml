open Utils

(* Helper function for integer addition *)
let int_add n1 n2 = n1 + n2

(* Parse function that uses My_parser, if necessary *)
let parse (input : string) : Utils.prog option =
  My_parser.parse input

(* Recursive evaluation function with basic implementation to use `expr` *)
let rec eval (expr : expr) : (value, error) result =  (* Added `rec` here *)
  match expr with
  | Num n -> Ok (VNum n)  (* Evaluates a number directly *)
  | Var x -> Error (UnknownVar x)  (* Placeholder for handling variables *)
  | True -> Ok (VBool true)
  | False -> Ok (VBool false)
  | Unit -> Ok VUnit
  | Bop (Add, e1, e2) -> (
      match eval e1, eval e2 with
      | Ok (VNum n1), Ok (VNum n2) -> Ok (VNum (int_add n1 n2))
      | Ok _, Ok _ -> Error (InvalidArgs Add)  (* Error if operands are not numbers *)
      | _ -> Error (InvalidArgs Add)
    )
  | _ -> Error InvalidApp  (* Placeholder for unimplemented cases *)

(* Interpreter function that uses parse and eval *)
let interp (input : string) : (value, error) result =
  match parse input with
  | Some prog -> eval prog
  | None -> Error ParseFail
