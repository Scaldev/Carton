(*****************************************************************************)
(*                             Automaton structure                           *)
(*****************************************************************************)

(* [has_epsilon] and [is_det] are given by the API. *)

(**
    [is_clean auto] returns [true] iff all states in [auto] are accessible
    and co-accessible.
*)
val is_clean : Auto.auto -> bool

(**
    [is_semi_normalized auto] returns [true] iff [auto] is semi-normalized.
*)
val is_semi_normalized : Auto.auto -> bool

(**
    [is_normalized auto] returns [true] iff [auto] is normalized.
*)
val is_normalized : Auto.auto -> bool

(**
    [is_complete sigma auto] returns [true] iff [auto] is complete for the
    alphabet [sigma].
*)
val is_complete : char list -> Auto.auto -> bool

(*****************************************************************************)
(*                                  Modifying                                *)
(*****************************************************************************)

(**
    [clean auto] returns the pruned automaton of [auto].
*)
val clean : Auto.auto -> Auto.auto

(**
    [remove_eps_trans auto] returns an automaton [auto'] of the same language
    that [auto] without any ε-transition.
*)
val remove_eps_trans : Auto.auto -> Auto.auto

(**
    [make_det auto] returns a deterministic automaton [auto'] of the same
    language that [auto].
*)
val make_det : Auto.auto -> Auto.auto

(**
    {b Precondition:} [auto] is a NFA, [sigma] has no duplicates.

    [complet sigma auto] returns an automaton [auto'] of the same language
    that [auto] and complete for the alphabet [sigma].

    {b Postcondition:}
    - if [auto] is a NFA, then [auto'] is a NFA.
    - if [auto] is a DFA, then [auto'] is a DFA.

    {b Complexity:} In [O(n*n*|sigma|)], where [n] is the number of states
    in [auto]. If [auto] is deterministic, it drops to [O(n*|sigma|)].
    Because [sigma] has no duplicates, it has at most 128 elements, so we
    get [O(n)] for [auto] deterministic.
*)
val complete : char list -> Auto.auto -> Auto.auto

(**
    [semi_normalize auto] returns an ε-NFA [auto'] that recognizes the same
    language, with a unique initial state [0] such that there is no transition
    [p -o-> 0].
*)
val semi_normalize : Auto.auto -> Auto.auto

(**
    [normalize auto] returns an ε-NFA [auto'] that recognizes the same
    language, with a unique initial state [0] and a unique final state
    [1], such there is no transition [p -o-> 0] nor [1 -o-> q].
*)
val normalize : Auto.auto -> Auto.auto

(*****************************************************************************)
(*                             Boolean operations                            *)
(*****************************************************************************)

(**
    {b Precondition:} [auto] is a DFA.

    [complement auto] returns an automaton [auto'] that recognizes the
    complement of [auto]'s language.
*)
val complement : Auto.auto -> Auto.auto

(**
    [union auto1 auto2] returns an automaton [auto'] union of [auto1] and
    [auto2], such that:
    - if [auto1] or [auto2] are   DFA, then so if [auto'] ;
    - if [auto1] or [auto2] are   NFA, then so is [auto'] ;
    - if [auto1] or [auto2] are ε-NFA, then so is [auto'].
*)
(* val union : Auto.auto -> Auto.auto -> Auto.auto *)

(**
    [union auto1 auto2] returns an automaton [auto'] intersection of [auto1]
    and [auto2], such that:
    - if [auto1] or [auto2] are   DFA, then so if [auto'] ;
    - if [auto1] or [auto2] are   NFA, then so is [auto'] ;
    - if [auto1] or [auto2] are ε-NFA, then so is [auto'].
*)
(* val inter : Auto.auto -> Auto.auto -> Auto.auto *)

(**
    [diff auto1 auto2] returns an automaton [auto'] such thath
    [auto'] recognizes the same language that [auto1 \ auto2].
*)
(* val diff : Auto.auto -> Auto.auto -> Auto.auto *)

(**
    [included auto1 auto2] returns [true] iff every word
    recognized by [auto1] is recognized by [auto2].
*)
(* val included : Auto.auto -> Auto.auto -> Auto.auto *)

(**
    [equiv sigma auto1 auto2] returns [true] iff [auto1] and [auto2]
    recognizes the same language on the alphabet [sigma].
*)
(* val equiv : char list -> Auto.auto -> Auto.auto -> bool *)

(*****************************************************************************)
(*                             Recognized language                           *)
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

(**
    [is_empty auto] returns [true] iff the language recognized by [auto] is the
    empty language.
*)
val is_empty : Auto.auto -> bool

(**
    [is_full sigma auto] returns [true] iff the language recognized by
    [auto] is [sigma*].
*)
val is_full : char list -> Auto.auto -> bool

(*****************************************************************************)
(*                           Rational expressions                            *)
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
