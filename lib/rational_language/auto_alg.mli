(**
    [forward auto] returns a [(accs, naccs)] pair, such that [accs] is the list
    of accessible states in [auto], and [naccs] the remaining unaccessibles
    states.
*)
val forward : Auto.auto -> int list * int list

(**
    [is_empty auto] returns [true] iff the language recognized by [auto] is the
    empty language.
*)
val is_empty : Auto.auto -> bool

(**
    [backward auto] returns a [(coaccs, ncoaccs)] pair, such that [coaccs] is
    the list of co-accessibles states in [auto], and [ncoaccs] the remaining
    non co-accessibles states.

*)
val backward : Auto.auto -> int list * int list

(**
    [clean auto] returns the pruned automaton of [auto].
*)
val clean : Auto.auto -> Auto.auto