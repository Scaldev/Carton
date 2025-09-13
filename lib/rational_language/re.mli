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

(**
  [first r] returns the list of chars that can appear as the first character
  of a word in [r].
*)
val first : re -> char list

(**
  [last r] returns the list of chars that can appear as the last character of
  of a word in [r].
*)
val last : re -> char list

(**
  [follow r c] returns the list chars that can appear after the character [c]
  in a word in [r].
*)
val follow : re -> char -> char list

(**
  {b Precondition:} [r] contains at most 256 [Re.Char] constructors.

  [linearize r] returns a triplet [(n, arr, r')], where:
    - [r'] is a linearized rational expression with [r]'s structure.
    - the [i]th character in [r'] is mapped to [arr.(i)].
    - [n] is the number of [Char] constructors in [r], i.e. only values
      from [1] to [n] in [arr] are defined.
*)
val linearize : re -> int * char array * re