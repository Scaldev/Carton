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

let rec has_epsilon (r: re) : bool =
  match r with
  | Empty             -> false
  | Epsilon           -> true
  | Char _            -> false
  | Alt (re1, re2)    -> has_epsilon re1 || has_epsilon re2
  | Concat (re1, re2) -> has_epsilon re1 && has_epsilon re2
  | Star _            -> true

(*****************************************************************************)
(*                       String of rational expression                       *)
(*****************************************************************************)

let rec string_of_re (r: re) : string =
  match r with
  | Empty           -> "∅"
  | Epsilon         -> "ε"
  | Char a          -> String.make 1 a
  | Alt (r1, r2)    -> string_of_alt r1 r2
  | Concat (r1, r2) -> string_of_concat r1 r2
  | Star r1         -> string_of_star r1

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

(*****************************************************************************)
(*                                Local languages                            *)
(*****************************************************************************)

(**
  {b Precondition:} [xs] and [ys] are sorted using [<] ordering,
  and don't contain any duplicates.

  [union xs ys] returns a sorted list of the elements from
  [xs] and [ys] without any duplicates.

  {b Complexity:} in [O(n)] time, [n] being the length of the result.
*)
let rec union (xs: char list) (ys: char list): char list =
  match xs, ys with
  | [], _ -> ys
  | _, [] -> xs
  | u :: us, v :: vs ->
    if      u = v then u :: union us vs
    else if u < v then u :: union us ys
    else               v :: union xs vs

let rec first (r: re) : char list =
  match r with
  | Empty | Epsilon -> []
  | Char a          -> [a]
  | Alt (r1, r2)    -> union (first r1) (first r2)
  | Concat (r1, r2) ->
    if has_epsilon r1 then union (first r1) (first r2) else first r1
  | Star r1         -> first r1

let rec last (r: re) : char list =
  match r with
  | Empty | Epsilon -> []
  | Char a          -> [a]
  | Alt (r1, r2)    -> union (last r1) (last r2)
  | Concat (r1, r2) ->
    if has_epsilon r2 then union (last r1) (last r2) else last r2
  | Star r1         -> last r1

let rec follow (r: re) (c: char) : char list =
  match r with
  | Empty | Epsilon | Char _ -> []
  | Alt (r1, r2)             ->
    union (follow r1 c) (follow r2 c)
  | Concat (r1, r2)          ->
    union (union (follow r1 c) (follow r2 c))
    (if List.mem c (last r1) then first r2 else [])
  | Star r1                  ->
    union (follow r1 c)
    (if List.mem c (last r1) then first r1 else [])

let linearize (r: re) : int * char array * re =
  let i = ref 0 in
  let arr = Array.make 256 (Char.chr 0) in
  let rec aux (r: re) : re =
    match r with
    | Empty | Epsilon -> r
    | Char c          -> incr i ; arr.(!i) <- c ; Char (Char.chr !i)
    | Alt (r1, r2)    -> let r1' = aux r1 in Alt (r1', aux r2)
    | Concat (r1, r2) -> let r1' = aux r1 in Concat (r1', aux r2)
    | Star r1         -> Star (aux r1)
  in
  (!i, arr, aux r)