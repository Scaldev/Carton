type auto

(*****************************************************************************)
(*                             Automaton creation                            *)
(*****************************************************************************)

(**
  [create n] returns an automaton of [n] state, with no initial state,
  no final states and no transitions. In other words, it returns the
  automaton
  {[
    (Q, Σ, I, F, δ) = ({0, ..., n-1}, char, ∅, ∅, ∅) ]
  ]}
*)
val create : int -> auto

(**
  [eq a1 a2] returns [true] iff [a1] and [a2] have the same states and
  transitions.
*)
val eq : auto -> auto -> bool

(**
  [copy a] returns a new automaton [a'] such that [a = a'].
*)
val copy : auto -> auto

(*****************************************************************************)
(*                            Automaton properties                           *)
(*****************************************************************************)

(**
  [size a] returns the number of states of the automaton [a].
*)
val size : auto -> int

(**
  [is_initial a q] returns [true] iff [q] is an initial state.

  {b Precondition:} [0 <= q && q < size a].
*)
val is_initial : auto -> int -> bool

(**
  [is_final a q] returns [true] iff [q] is a final state.

  {b Precondition:} [0 <= q && q < size a].
*)
val is_final : auto -> int -> bool

(**
  [has_epsilon a] returns [true] iff there is an ε-transition between
  two states of the automaton [a].
*)
val has_epsilon : auto -> bool

(**
  [is_det a] returns [true] iff [a] is a determistic automaton, i.e. :
    - there is exactly one initial state [0] ;
    - [not (has_epsilon a)] ;
    - for all states [p], [q], [q'] and for char [c], [p -c-> q] and
    [p -c-> q'] implies [q = q'].
*)
val is_det : auto -> bool

(**
  [trans a p c] returns the list [qs] of states such that for each [q] of [qs],
  [p -c-> q] in the automaton [a].

  {b Precondition:} [0 <= p && p < size a].
*)
val trans : auto -> int -> char -> int list

(**
  [trans_eps a p] returns the list [qs] of states such that for each [q] of [qs],
  [p -ε-> q] in the automaton [a].

  {b Precondition:} [0 <= p && p < size a].
*)
val trans_eps : auto -> int -> int list

(**
  [trans_all a p] returns a pair of lists [(l, l')] such that :
    - [l] is the list of outgoing transitions from [p], with each element of
      the form [(c, [q_0; ...; q_n])], such that [p -c-> q_0], ...,
      [p -c-> q_n].
    - [l'] is the list of outgoing ε-transitions from [p]; that is,
      [l' = [q_0; ...; q_n]] such that [p -ε-> q_0], ..., [p -ε-> q_n].
*)
val trans_all : auto -> int -> (char * int list) list * int list

(**
  [trans_all_opt a p] returns a list of [(o, q)] pairs, such that
  [(Some c, q)] means [p -c-> q] and [(None, q)] means [p -ε-> q].
*)
val trans_all_opt : auto -> int -> (char option * int) list

(*****************************************************************************)
(*                              Automaton edits                              *)
(*****************************************************************************)

(**
  [add_trans a p c q] adds the [p -c-> q] transition to the automaton [a].

  {b Precondition:} [0 <= p && p < size a] and [0 <= q && q < size a].
*)
val add_trans : auto -> int -> char -> int -> unit

(**
  [add_trans_eps a p q] adds the [p -ε-> q] transition to the automaton [a].

  {b Precondition:} [0 <= p && p < size a] and [0 <= q && q < size a].
*)
val add_trans_eps : auto -> int -> int -> unit

(**
  [add_trans_opt a p o q] adds the [p -ε-> q] transition if [o] is [None],
  or the [p -c-> q] transition if [o] is [Some c], to the automaton [a].

  {b Precondition:} [0 <= p && p < size a] and [0 <= q && q < size a].
*)
val add_trans_opt : auto -> int -> char option -> int -> unit

(**
  [set_initial a q] sets the state [q] to be initial in the automaton [a].

  {b Precondition:} [0 <= q && q < size a].
*)
val set_initial : auto -> int -> unit

(**
  [unset_initial a q] sets the state [q] {i not} to be initial in the automaton [a].

  {b Precondition:} [0 <= q && q < size a].
*)
val unset_initial : auto -> int -> unit

(**
  [set_final a q] sets the state [q] to be final in the automaton [a].

  {b Precondition:} [0 <= q && q < size a].
*)
val set_final : auto -> int -> unit

(**
  [unset_final a q] sets the state [q] {i not} to be final in the automaton [a].

  {b Precondition:} [0 <= q && q < size a].
*)
val unset_final : auto -> int -> unit

(**
  [remove_stated a qs] returns a new automaton [a'] consisting of the automaton
  [a] without the states found in [qs].
*)
val remove_states : auto -> int list -> auto

(*****************************************************************************)
(*                              Print automaton                              *)
(*****************************************************************************)

val print : Format.formatter -> auto -> unit

val print_dot : Format.formatter -> auto -> unit

val print_dot_to_file : string -> auto -> unit