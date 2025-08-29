(*****************************************************************************)
(*                            Rational expressions                           *)
(*****************************************************************************)

type re =
  | Empty
  | Epsilon
  | Char of char
  | Alt of re * re
  | Concat of re * re
  | Star of re

let rec string_of_re (r: re) : string =
  match r with
  | Empty             -> "∅"
  | Epsilon           -> "ε"
  | Char a            -> String.make 1 a
  | Alt (re1, re2)    -> string_of_alt re1 re2
  | Concat (re1, re2) -> string_of_concat re1 re2
  | Star re0          -> string_of_star re0

(**
  [string_of_alt r r'] returns the string representation of [r+r'].
*)
and string_of_alt (r: re) (r': re) : string =
  match r, r' with
  | re1, Alt (re2, re3) | Alt (re1, re2), re3 ->
    string_of_re re1 ^ "+" ^ string_of_re re2 ^ "+" ^ string_of_re re3
  | _, Concat _ | Concat _, _ ->
    string_of_re r ^ "+" ^ string_of_re r'
  | _ -> string_of_binop r ^ "+" ^ string_of_binop r'

(**
    [string_of_concat r r'] returns the string representation of [r.r'].
*)
and string_of_concat (r: re) (r': re) : string =
  match r, r' with
  | re1, Concat (re2, re3) | Concat (re1, re2), re3 ->
    string_of_re re1 ^ string_of_re re2 ^ string_of_re re3
  | _ -> string_of_binop r ^ string_of_binop r'

(**
    [string_of_star r] returns the string representation of [r*].
*)
and string_of_star (r: re) : string =
  match r with
  | Empty | Epsilon | Char _ ->       string_of_re r ^  "*"
  | _                        -> "(" ^ string_of_re r ^ ")*"

(**
  [string_of_binop r] adds parentheses before and after [r] iff
  [r] is of the form [r1+r2] or [r1.r2].
*)
and string_of_binop (r: re) : string =
  match r with
  | Empty | Epsilon | Char _ | Star _ ->       string_of_re r
  | _                                 -> "(" ^ string_of_re r ^ ")"

let rec has_epsilon (r: re) : bool =
  match r with
  | Empty             -> false
  | Epsilon           -> true
  | Char _            -> false
  | Alt (re1, re2)    -> has_epsilon re1 || has_epsilon re2
  | Concat (re1, re2) -> has_epsilon re1 && has_epsilon re2
  | Star _            -> true