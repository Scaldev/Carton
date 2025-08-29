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

(**
    [string_of_re r] returns the string representation of the rational
    expression [r].
*)
val string_of_re : re -> string

(**
    [has_epsilon r] returns [true] iff [epsilon] is the language given by the
    rational expression [r].
*)
val has_epsilon : re -> bool