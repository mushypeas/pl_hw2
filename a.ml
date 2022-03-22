type ae = CONST of int
        | VAR of string
        | POWER of string * int
        | TIMES of ae list
        | SUM of ae list

let rec diff: ae * string -> ae = fun (eq, var) ->
  match eq with
  | TIMES ae_list -> (
    match ae_list with
      | VAR a :: [VAR d; VAR e;POWER (b, c)] ->
        let _ = print_endline (a) in
        VAR a
  )

(* let a = TIMES (VAR "x"::VAR "y"::POWER ("x", 2)::[]) *)
let a = TIMES (VAR "x"::VAR "x"::VAR "y"::POWER ("x", 2)::[])
let _ = diff (a, "x")