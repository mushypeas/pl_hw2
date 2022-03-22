type ae = CONST of int
        | VAR of string
        | POWER of string * int
        | TIMES of ae list
        | SUM of ae list

let rec diff: ae * string -> ae = fun (eq, var) ->
  match eq with
  | CONST n -> CONST (0)
  | SUM ae_list -> (
    match ae_list with
    | [] -> SUM(ae_list)
    | head::tail -> SUM((diff (head, var)) :: tail)
  )
  | TIMES ae_list -> (
    match ae_list with
    | hd :: tl -> SUM [
      TIMES (diff (hd, var)::TIMES tl);
      TIMES [hd;diff (TIMES tl, var)]
    ]
    | CONST a :: CONST b :: [] -> CONST 0
    | CONST a :: VAR b :: [] -> TIMES [CONST a; diff (b, var)]
    | CONST a :: POWER b :: [] -> 
    | VAR a :: POWER (b, c) :: [] -> 
    | 
    | CONST a::[VAR b] -> TIMES((diff (head, var)) :: tail)
    | VAR a
  )
  | POWER (base, exp) ->
    if (base = var) then (
      match exp with
        | 2 -> TIMES [CONST (2); VAR (base)]
        | _ -> TIMES [CONST (2); POWER (base, exp - 1)]
    )
    else CONST (0)
  | VAR (base) ->
    if (base = var) then CONST (1)
    else VAR (base)