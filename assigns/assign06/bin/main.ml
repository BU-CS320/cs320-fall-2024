
let () =
  let ( let* ) = Option.bind in
  let _ =
    let* input =
      (* check that an argument is given on the command line *)
      if Array.length Sys.argv > 1
      then
        let filename = Sys.argv.(1) in
        (* read in the file *)
        try Some (Stdlib320.read_file filename) with _ ->
          print_endline
            ("Error: Could not read \'" ^ filename ^ "\'");
          None
      else (
        print_endline "Error: No file given"
        ; None
      )
    in
    (* run the lexer *)
    let _ = print_endline "lexing..." in
    let* tokens = Assign06_01.lex input in
    (* run the parser *)
    let _ = print_endline "parsing..." in
    let* expr = Assign06_02.parse tokens in
    (* run the type checker *)
    let _ = print_endline "type checking..." in
    let* _ = Assign06_03.type_of expr in
    (* run the evaluator *)
    let _ = print_endline "evaluating..." in
    let v = Assign06_04.eval expr in
    let _ = print_endline "DONE.\n" in
    let _ =
      (* print the value *)
      match v with
        | VNum n -> print_endline (string_of_int n)
        | VBool b -> print_endline (string_of_bool b)
    in Some v
  in ()
