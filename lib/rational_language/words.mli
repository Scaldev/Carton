(*****************************************************************************)
(*                            Letters and factors                            *)
(*****************************************************************************)

(**
    [occurrences w l] returns the number of occurrences on the letter [l] in
    the word [w].
*)
val occurrences : string -> char -> int

(**
    [is_prefix w u] returns [true] iff [u] is a prefix of [w]; that is, there
    exists a word [v] such that [w = uv].
*)
val is_prefix : string -> string -> bool

(**
    [is_suffix w u] returns [true] iff [u] is a suffix of [w]; that is, there
    exists a word [v] such that [w = vu].
*)
val is_suffix : string -> string -> bool

(**
    [is_factor w u] returns [true] iff [u] is a factor of [w]; that is, there
    exists two words [v] and [v'] such that [w = vuv'].
*)
val is_factor : string -> string -> bool

(*****************************************************************************)
(*                                Periodicity                                *)
(*****************************************************************************)

(**
    [is_period_of_word w p] returns [true] iff [w] as a period of [p];
    that is, for every [i] such that [1 <= i <= |w|-p], we have
    [w_i = w_{i+p}].
*)
val is_period_of_word : string -> int -> bool

(**
    [periods_of w] returns the list of periods of [w].
*)
val periods_of : string -> int list

(**
    [is_primitive w] returns [true] iff [w] is primitive;
        that is, [w] can't be written as [u^k].
*)
val is_primitive : string -> bool

type factorized = string * string * int

(**
    [factorize w p] returns a triplet [(u, v, k)] such that
    [|uv| = p] and [w = ((uv)^k)u].
*)
val factorize : string -> int -> factorized

(**
    Guibas-Odlyzsko's theorem states that for each word [w],
    there exists a word [w'] on the binary alphabet [{0,1}]
    such that [periods_of w = periods_of w'].
    [guibas_odlyzko w] returns such a word [w'].
*)
val guibas_odlyzko : string -> string

(**
    The [morphism] type represents a morphism [psi: A* -> B*] such that
    for a word [w = x_0 x_1 ... x_2] in [A*], we have
    [psi(w) = psi(x_0) psi(x_1) ... psi(x_2)]. Thus, we can define [psi]
    by only defining [psi(a)] for all [a] in [A].
*)
type morphism = (char, string) Hashtbl.t

(**
    [morphism_of_list [(a_0, s_0) ; (a_1, s_1) ; ... ; (a_n ; s_n)]]
    returns the morphism [psi] such that [psi(a_i) = s_i].
*)
val morphism_of_list : (char * string) list -> morphism

exception Invalid_word of string

(**
    [app psi w] returns [psi(w)].

    {b Time complexity:} in [Theta(n)], with [n = |psi(w)|].

    {b Space complexity:} in [Theta(n)], with [n = max Im(psi)] and
    [max a b = a] iff [|a| >= |b|].
    
    @raise Invalid_word iff [w] is not in [dom(psi)*].
*)
val app : morphism -> string -> string

(**
    [is_endomorphism psi] returns [true] iff for all words [w] in [Im(psi)],
    each letter [l] of [w] is in [dom(psi)], i.e. iff [Im(psi)] is a subset of
    [dom(psi)*].
    If that is the case, then we can define [psi(w)], [psi(psi(w))], and so on.
*)
val is_endomorphism : morphism -> bool

(**
    [iter_app psi w n] returns [psi^n(w)], using the {!app} function.

    {b Precondition:} [is_endormorphism psi] returns [true].
*)
val iter_app : morphism -> string -> int -> string