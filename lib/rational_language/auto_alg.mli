(*****************************************************************************)
(*                            Pruning and cleaning                           *)
(*****************************************************************************)

(**
    [is_empty auto] returns [true] iff the language recognized by [auto] is the
    empty language.
*)
val is_empty : Auto.auto -> bool

(**
    [clean auto] returns the pruned automaton of [auto].
*)
val clean : Auto.auto -> Auto.auto

(**
    [remove_eps_trans auto] returns an automaton [auto'] of the same language
    that [auto] without any ε-transition.
*)
val remove_eps_trans : Auto.auto -> Auto.auto

(*****************************************************************************)
(*                                  Acceptance                               *)
(*****************************************************************************)

(**
    {b Precondition:} [auto] is deterministic.

    [accept_dfa auto s] returns [true] iff [s] is accepted by [auto].

    {b Complexity:} in [O(|s|)].
*)
val accept_dfa : Auto.auto -> string -> bool

(**
    {b Precondition:} [auto] has no ε-transition.

    [accept_nfa auto s] returns [true] iff [s] is accepted by [auto].

    {b Complexity:} in [O((n + |ẟ|) * |s|)], where [|ẟ|] is the
    number of transitions in [auto].
*)
val accept_nfa : Auto.auto -> string -> bool

(*****************************************************************************)
(*                     Automata and rational expressions                     *)
(*****************************************************************************)

(**
    [thompson r] returns an automaton denoting the same language,
    using Thompson's construction.
*)
val thompson : Re.re -> Auto.auto

(**
    [berry_sethi r] returns an automaton denoting the same language,
    using Berry-Sethi's approch with local languages.
*)
val berry_sethi : Re.re -> Auto.auto