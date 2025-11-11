open Auto

let (<<) = Fun.compose

(*****************************************************************************)
(*                                                                           *)
(*                                    UTILS                                  *)
(*                                                                           *)
(*****************************************************************************)

(*****************************************************************************)
(*                                    lists                                  *)
(*****************************************************************************)

(**
  [range a b] returns the list [a ; a+1 ; ...; b-1].
  If [b <= a], returns the empty list.
*)
let range (a: int) (b: int) : int list =
  let n = max 0 (b - a) in
  List.init n (fun i -> a + i)

(**
  {b Precondition:} [xs] and [ys] are sorted using [<] ordering,
  and don't contain any duplicates.

  [union_list xs ys] returns a sorted list of the elements from
  [xs] and [ys] without any duplicates.

  {b Complexity:} in [O(n)] time, [n] being the length of the result.
*)
let rec union_list (xs: 'a list) (ys: 'a list): 'a list =
  match xs, ys with
  | [], _ -> ys
  | _, [] -> xs
  | u :: us, v :: vs ->
    if      u = v then u :: union_list us vs
    else if u < v then u :: union_list us ys
    else               v :: union_list xs vs

(**
  [set_power xs n] returns a list of all possible lists [ps] of length [<= n]
  such that each element in [ps] is in [xs].

  {b Complexity:} in [O(|xs|^n)].

  {b Example:} [set_power ['a';'b'] 2] returns
  [
    [] ; ['a'] ; ['b'] ; ['a';'a'] ; ['a' ; 'b'] ; ['b' ; 'a'] ; ['b' ; 'b']
  ].
*)
let rec set_power (xs: 'a list) (n: int) : 'a list list =
  if n = 0 then [ [] ]
  else
    let parts = set_power xs (n-1) in
    let singletons = List.map (fun x -> [x]) xs in
    let others = xs
      |> List.map (fun x -> List.map (fun part -> x :: part) parts)
      |> List.flatten
    in
    [] :: singletons @ others

(*****************************************************************************)
(*                                    arrays                                 *)
(*****************************************************************************)

(**
  [exists_i f arr] returns [f 0 arr.(0) || ... || f (n-1) arr.(n-1)],
  where [n = |arr|].
*)
let exists_i (f: int -> 'a -> bool) (arr: 'a array) : bool =
  let i = ref (-1) in
  Array.exists (fun a -> incr i ; f !i a) arr

(**
  [split_on_bool arr] returns a [(trues, falses)] pair of lists, such that:
  - [i] is in [trues] iff [arr.(i) = true] ;
  - [i] is in [falses] iff [arr.(i) = false] ;
  - [trues] and [falses] are sorted by [<].
*)
let split_on_bool (arr: bool array) : int list * int list =
  let ids    = List.init (Array.length arr) Fun.id in
  let trues  = List.filter (Array.get arr) ids in
  let falses = List.filter (not << Array.get arr) ids in
  (trues, falses)

(*****************************************************************************)
(*                                     DFS                                   *)
(*****************************************************************************)

(**
  [dfs n before succs after u] executes a depth-first search on a graph of [n]
  nodes, starting at [u0]. This function doesn't know anythig of the graph,
  it's up the caller to provide the necessary data:
  - [before u] executes when first visiting the node [u] ;
  - [succes u] gives the list of successors of [u] in the graph ;
  - [after u] executes after visiting every successor of [u].
  Here, nodes are represented as integers.
*)
let dfs (n: int) (before: int -> unit) (succs: int -> int list) (after: int * int -> unit) (u0: int): unit =
  let visited = Array.make n false in
  let rec visit (u: int) =
    if not visited.(u) then (
      visited.(u) <- true ;
      before u;
      List.iter (fun v -> visit v ; after (u, v)) (succs u)
    )
  in visit u0

(**
  [dfs_iter succs after init] calls [dfs_iter] to each successor [v] of a node
  [u] recursively, starting with [u = init], then call [after u v].
*)
let dfs_iter (succs: 'a -> 'a list) (after: 'a -> 'a -> unit) (init: 'a) : unit =
  let rec loop (u: 'a) : unit =
    List.iter (fun v -> loop v ; after u v) (succs u)
  in loop init

(*****************************************************************************)
(*                                                                           *)
(*                             AUTOMATON STRUCTURE                           *)
(*                                                                           *)
(*****************************************************************************)

(*****************************************************************************)
(*                                sigma_of_auto                              *)
(*****************************************************************************)

(**
  [sigma_of_auto auto] returns the list of chars found on [auto]'s transitions.
*)
let sigma_of_auto (auto: auto) : char list =
  let chars = Array.make 256 false in
  for q = 0 to size auto - 1 do
    let ts = fst (trans_all auto q) in
    List.iter (fun (c, _) -> chars.(Char.code c) <- true) ts;
  done;
  List.map Char.chr (fst (split_on_bool chars))

(*****************************************************************************)
(*                             forward & backward                            *)
(*****************************************************************************)

(**
  [prevs_table auto] returns an array [prevs] such that [prevs.(q)] is the list
  [ps] such that [p -o-> q] in [auto] for all [p] in [ps].
*)
let prevs_table (auto: auto) : int list array =
  
  let n = size auto in
  let prevs = Array.make n [] in
  let set_prev p q = prevs.(q) <- p :: prevs.(q) in

  for p = 0 to n-1 do
    let qs = trans_all_opt auto p in
    List.iter (set_prev p << snd) qs
  done;

  prevs

(**
  [forward auto] returns a [(accs, naccs)] pair, such that [accs] is the list
  of accessible states in [auto], and [naccs] the remaining unaccessibles
  states.
*)
let forward (auto: auto) : int list * int list =

  let n = size auto in
  let visited = Array.make n false in
  let initials = List.filter (is_initial auto) (range 0 n) in

  let before p = visited.(p) <- true in
  let succs  p = List.map snd (trans_all_opt auto p) in
  let dfs_start_at = dfs n before succs ignore in

  List.iter dfs_start_at initials;
  split_on_bool visited

(**
  [backward auto] returns a [(coaccs, ncoaccs)] pair, such that [coaccs] is
  the list of co-accessibles states in [auto], and [ncoaccs] the remaining
  non co-accessibles states.
*)
let backward (auto: auto) : int list * int list =

  let prevs = prevs_table auto in

  let n = size auto in
  let visited = Array.make n false in
  let finals = List.filter (is_final auto) (range 0 n) in

  let before p = visited.(p) <- true in
  let succs  p  = prevs.(p) in
  let dfs_start_at = dfs n before succs ignore in

  List.iter dfs_start_at finals;
  split_on_bool visited

(*****************************************************************************)
(*                                     is_clean                              *)
(*****************************************************************************)

let is_clean (auto: auto) : bool =
  let _, naccs = forward auto in
  let _, ncoaccs = backward auto in
  List.length naccs = 0 && List.length ncoaccs = 0

(*****************************************************************************)
(*                                  is_complete                              *)
(*****************************************************************************)

(**
  [trans_sybmols auto p] returns the list of chararacters [a] such that there
  exists [q] in [auto] such that [p -a-> q], is alphabetical order.
*)
let trans_symbols (auto: auto) (p: int) : char list =
  trans_all_opt auto p
  |> List.map fst
  |> List.filter ((<>) None)
  |> List.map Option.get
  |> List.sort_uniq Char.compare

(**
  [is_complete_state sigma auto p] returns [true] iff for all [a] in [sigma],
  there exists a state [q] in [auto] such that [p -a-> q].
*)
let rec is_complete_state (sigma: char list) (auto: auto) (p: int) : bool =
  if p = size auto then true
  else if sigma <> trans_symbols auto p then false
  else is_complete_state sigma auto (p+1)

let is_complete (sigma: char list) (auto: auto) : bool =
  let ps = range 0 (size auto) in
  List.for_all (is_complete_state sigma auto) ps

(*****************************************************************************)
(*                               is_semi_normalized                          *)
(*****************************************************************************)

let is_semi_normalized (auto: auto) : bool =
  let ps = range 0 (size auto) in
  let inits        = List.filter (is_initial auto) ps in
  let no_init_succ = List.for_all (((<>) 0) << snd) << (trans_all_opt auto) in
  List.length inits = 1 && List.for_all no_init_succ ps

(*****************************************************************************)
(*                                 is_normalized                             *)
(*****************************************************************************)

let is_normalized (auto: auto) : bool =
  let ps     = range 0 (size auto) in
  let finals = List.filter (is_final auto) ps in
  print_string (string_of_bool (is_semi_normalized auto)) ;
  print_string (string_of_bool (List.length finals = 1)) ;
  print_string (string_of_bool (List.is_empty (trans_all_opt auto (List.hd finals)))) ;
  is_semi_normalized auto
  && List.length finals = 1 
  && List.is_empty (trans_all_opt auto (List.hd finals))

(*****************************************************************************)
(*                                                                           *)
(*                                  MODIFYING                                *)
(*                                                                           *)
(*****************************************************************************)

(*****************************************************************************)
(*                                    clean                                  *)
(*****************************************************************************)

let clean (auto: auto) : auto =
  let (_, naccs) = forward auto in
  let (_, ncoaccs) = backward auto in
  Auto.remove_states auto (naccs @ ncoaccs)

(*****************************************************************************)
(*                              remove_eps_trans                             *)
(*****************************************************************************)

(**
  [eps_closure auto] returns an array giving for each state its ε-closure.
*)
let eps_closure (auto: auto) : int list array =

  let n = size auto in
  let visited = Array.make n [] in
  (*
    The ε-closure of [p] is the union of the ε-closure of all [q]
    such that p-ε-> q.
  *)
  let before p     = visited.(p) <- [ p ] in
  let succs        = Auto.trans_eps auto in
  let after (p, q) = visited.(p) <- union_list visited.(p) visited.(q) in
  let dfs_start_at = dfs n before succs after in

  List.iter dfs_start_at (range 0 n) ;
  visited

(**
  [add_trans_no_eps auto p qs] adds the transition [p -a-> q] in [auto] for all
  [q] in [qs], where [a] is a letter (and not [ε]).
*)
let add_trans_no_eps (auto: auto) (p: int) (qs: (char option * int) list) =
  List.iter (fun (o, q) ->
    match o with
    | Some c -> add_trans auto p c q
    | None -> ()
  ) qs

let remove_eps_trans (auto: auto) : auto =

  if not (has_epsilon auto) then copy auto else

  let n = size auto in
  let nauto = create n in
  let closures = eps_closure auto in

  for p = 0 to n-1 do
    if is_initial auto p then set_initial nauto p;
    if is_final   auto p then set_final   nauto p;
    (* if [p -a-> q] in [auto], then add [p -a-> q] in [nauto] *)
    add_trans_no_eps nauto p (trans_all_opt auto p) ;
    (* if [p -ε->* q -a-> r] in [auto], then add [p -a-> r] in [nauto] *)
    List.iter (add_trans_no_eps nauto p << trans_all_opt auto) closures.(p)
  done ;
  clean nauto

(*****************************************************************************)
(*                                   make_det                                *)
(*****************************************************************************)

(* pset -> id, [ -a1-> qset1 ; -a2-> qset2 ; ... ] *)
type trans_table = (int list, int * (char * int list) list) Hashtbl.t
 
(**
  [make_det_new_id] returns a unique id per call, starting at [1].
*)
let make_det_new_id =
  let c = ref ~-1 in
  fun () -> incr c; !c

(**
  [add_trans_in_table trans pset c qset] adds [(c, qset)] in the list
  value of the key [pset] in the table [trans].
*)
let add_trans_in_table (trans: trans_table) (pset: int list) (c: char) (qset: int list) : unit =
  try
    let id, lst = Hashtbl.find trans pset in
    Hashtbl.replace trans pset (id, (c, qset) :: lst)
  with Not_found ->
    Hashtbl.add trans pset (make_det_new_id (), (c, qset) :: [])

(**
    [succs_set_for_char auto pset c] returns the list [qset] such that
    [p -c-> q] is a transition in [auto] for [p] in [pset] and [q] in [qset].
*)
let succs_set_for_char (auto: auto) (pset: int list) (c: char): int list =
  List.fold_left (fun acc' -> fun q -> union_list acc' (Auto.trans auto q c)) [] pset

(**
    Adds all transitions in [trans] for the key [pset].

    [make_det_succ trans sigma auto pset] returns the list [qset] such that
    [p -c-> q] is a transition in [auto] for [p] in [pset], [q] in [qset]
    and [c] a char.
*)
let make_det_succ (trans: trans_table) (sigma: char list) (auto: auto) (pset: int list): int list list =
  if Hashtbl.mem trans pset then []
  else List.fold_left (fun (acc: int list list) (c: char) ->
    match succs_set_for_char auto pset c with
    | []   -> acc
    | qset -> add_trans_in_table trans pset c qset ; qset :: acc
  ) [] sigma

(**
    {b Precondition:} [auto] has no ε-transition.

    [make_det_new_auto trans auto] returns a deterministic automaton [nauto]
    that has the same language as [auto], using [trans].
*)
let make_det_new_auto (trans: trans_table) (auto: auto) : auto =
  
  let nauto = create (Hashtbl.length trans) in
  Hashtbl.iter (fun pset (p', trs) ->
    (* set initial/final *)
    if List.for_all (is_initial auto) pset then set_initial nauto p' ;
    if List.exists  (is_final   auto) pset then set_final   nauto p' ;
    (* add transitions *)
    List.iter (fun (c, qset) ->
      let q' = fst (Hashtbl.find trans qset) in add_trans nauto p' c q'
    ) trs
  ) trans;
  nauto

let make_det (auto: auto) : auto =

  if is_det auto then copy auto else

  let auto  = remove_eps_trans auto in
  let sigma = sigma_of_auto auto    in
  let trans = Hashtbl.create 16     in

  let succs p   = make_det_succ trans sigma auto p in
  let after _ _ = () in

  for p = 0 to size auto - 1 do
    if is_initial auto p then dfs_iter succs after [p] 
  done ;
  make_det_new_auto trans auto

(*****************************************************************************)
(*                                  complete                                 *)
(*****************************************************************************)

let complete (sigma: char list) (auto: auto) : auto =

  let sigma = List.sort_uniq Char.compare sigma in
  let n = size auto in
  let nauto = create (n + 1) in
  
  let bottom = n in
  List.iter (fun c -> add_trans nauto bottom c bottom) sigma ;

  for p = 0 to n - 1 do
    if is_initial auto p then set_initial nauto p ;
    if is_final   auto p then set_final   nauto p ;
    List.iter (fun c ->
      match trans auto p c with
      | [] -> add_trans nauto p c bottom ;
      | qs -> List.iter (add_trans nauto p c) qs
    ) sigma
  done ;
  nauto

(*****************************************************************************)
(*                               semi_normalize                              *)
(*****************************************************************************)

let semi_normalize (auto: auto) : auto = 

  let n = size auto in
  let offset = 1 in
  let nauto = create (n + offset) in

  set_initial nauto 0;
  for p = 0 to n - 1 do
    if is_initial auto p then add_trans_eps nauto 0 (p + offset) ;
    if is_final auto p then set_final nauto (p + offset) ;
    let qs = trans_all_opt auto p in
    List.iter (fun (o,q) -> add_trans_opt nauto (p + offset) o (q + offset)) qs
  done ;
  nauto

(*****************************************************************************)
(*                                  normalize                                *)
(*****************************************************************************)

let normalize (auto: auto) : auto = 

  let n = size auto in
  let offset = 2 in
  let nauto = create (n + offset) in

  set_initial nauto 0;
  set_final nauto 1;
  for p = 0 to n - 1 do
    if is_initial auto p then add_trans_eps nauto 0 (p + offset) ;
    if is_final   auto p then add_trans_eps nauto (p + offset) 1 ;
    let qs = trans_all_opt auto p in
    List.iter (fun (o,q) -> add_trans_opt nauto (p + offset) o (q + offset)) qs
  done ;
  nauto

(*****************************************************************************)
(*                                                                           *)
(*                              BOOLEAN OPERATIONS                           *)
(*                                   (part 1)                                *)
(*****************************************************************************)

(*****************************************************************************)
(*                                    Product                                *)
(*****************************************************************************)

(*
  type trans_table' = (int * int, int * (char option * (int * int)) list) Hashtbl.t
*)

(**
  [product_new_id ()] returns a unique id each call, starting at 0.
*)
let product_new_id =
  let c = ref (-1) in
  fun () -> incr c; !c

(**
  [product_add_trans trans p' o q'] adds [p' -o-> q'] in the table [trans].
*)
let product_add_trans trans (p: int * int) (o: char option) (q: int * int) : unit =
  let (id, lst) =
    try Hashtbl.find trans p
    with Not_found -> (product_new_id (), [])
  in Hashtbl.replace trans p (id, (o, q) :: lst)

(**
  [product_succ auto1 auto2 trans p] returns the list of states [q]
  such that [p1 -o-> q1] or [p2 -o-> q2].
*)
let product_succ auto1 auto2 trans (p: int * int) : (int * int) list =
  let p1, p2 = p in
  if Hashtbl.mem trans p then []
  else
    let trs1 = trans_all_opt auto1 p1 in
    let trs2 = trans_all_opt auto2 p2 in
    List.fold_left (fun acc1 (o1, q1) ->
      List.fold_left (fun acc2 (o2, q2) ->
        let q = q1, q2 in
        if o1 = o2 then (product_add_trans trans p o1 q; q :: acc2) else acc2
      ) acc1 trs2
    ) [] trs1

(**
  [product op auto1 auto2] returns the product automaton [nauto] of [auto1]
  and [auto2], such that [nanuto] accepts [w] iff [auto1] [op] [auto2]
  accepts [w], and:
  - if [auto1] or [auto2] are   DFAs, then so if [nauto] ;
  - if [auto1] or [auto2] are   NFAs, then so is [nauto] ;
  - if [auto1] or [auto2] are ε-NFAs, then so is [nauto].

  {b Complexity.} In [O(|Q1| * |Q2|)].
*)
let product (op: bool -> bool -> bool) (auto1: auto) (auto2: auto) : auto =

  let trans = Hashtbl.create 16 in
  Hashtbl.add trans (0, 0) (product_new_id (), []) ;
  let succs = product_succ auto1 auto2 trans in
  dfs_iter succs (fun _ _ -> ()) (0, 0);

  let nauto = create (Hashtbl.length trans) in
  Hashtbl.iter (fun (p1, p2) (id, trs) ->
    if op (is_initial auto1 p1) (is_initial auto2 p2) then set_initial nauto id ;
    if op (is_final   auto1 p1) (is_final   auto2 p2) then set_final   nauto id ;
    List.iter (fun (o, q) ->
      add_trans_opt nauto id o (fst (Hashtbl.find trans q))  
    ) trs  
  ) trans ;
  nauto

(*****************************************************************************)
(*                                  Complement                               *)
(*****************************************************************************)

(**
  {b Precondition.} [auto] is a DFA.

  [complement sigma auto] returns a DFA [nauto] that recognizes the
  complement of [auto]'s language with alphabet [sigma].

  {b Complexity.} In [O(|Q|)].
*)
let complement_dfa (sigma: char list) (auto: auto) : auto =
  assert (is_det auto) ;
  let nauto = complete sigma auto in
  for p = 0 to size nauto - 1 do
    if      is_initial auto p  then set_initial nauto p ;
    if not (is_final   auto p) then set_final   nauto p ;
  done ;
  nauto

let complement (sigma: char list) (auto: auto) : auto =
  complement_dfa sigma (make_det auto)

(*****************************************************************************)
(*                                 Intersection                              *)
(*****************************************************************************)

let inter (auto1: auto) (auto2: auto) =
  product (&&) auto1 auto2

(*****************************************************************************)
(*                                     Union                                 *)
(*****************************************************************************)

(**
  [union_nfa_add auto n offset nauto] adds [auto] of size [n] in [nauto],
  starting at the state number [offset].
*)
let union_nfa_add (auto: auto) (n: int) (offset: int) (nauto: auto) : auto =
  for p = 0 to n-1 do
    if is_initial auto p then set_initial nauto (p + offset) ;  
    if is_final   auto p then set_final   nauto (p + offset) ;
    let qs = trans_all_opt auto p in
    List.iter (fun (o, q) -> add_trans_opt auto (p + offset) o (q + offset)) qs ;
  done ;
  nauto

(**
  [union_nfa auto1 auto2] returns a NFA [nauto] that accepts [w] iff [auto1] or
  [auto2] accepts [w].

  {b Complexity:} In [O(|Q|)].
*)
let union_nfa (auto1: auto) (auto2: auto) : auto =
  let n1, n2 = size auto1, size auto2 in
  create (n1 + n2)
  |> union_nfa_add auto1 n1 0
  |> union_nfa_add auto2 n2 n1

(**
  {b Precondition:} [auto1] and [auto2] are DFAs.

  [union_dfa auto1 auto2] returns a DFA [nauto] that accepts [w] iff [auto1] or
  [auto2] accepts [w].

  {b Complexity:} In [O(|Q1| * |Q2|)].
*)
let union_dfa (auto1: auto) (auto2: auto) : auto =
  assert (is_det auto1 && is_det auto2) ;
  let nsigma = union_list (sigma_of_auto auto1) (sigma_of_auto auto2) in
  let nauto1 = complete nsigma auto1 in
  let nauto2 = complete nsigma auto2 in
  product (||) nauto1 nauto2

let union (auto1: auto) (auto2: auto) : auto =
  if is_det auto1 && is_det auto2 then union_dfa auto1 auto2
  else union_nfa auto1 auto2
  
(*****************************************************************************)
(*                                  Difference                               *)
(*****************************************************************************)

let diff (sigma: char list) (auto1: auto) (auto2: auto) : auto =
  inter auto1 (complement sigma auto2)

(*****************************************************************************)
(*                                                                           *)
(*                                   ACCEPTANCE                              *)
(*                                                                           *)
(*****************************************************************************)

(*****************************************************************************)
(*                                  Acceptance                               *)
(*****************************************************************************)

let accept_dfa (auto: auto) (s: string) : bool =
  assert (is_det auto);
  let q = ref 0 in
  for i = 0 to String.length s - 1 do
    q := List.hd (trans auto !q s.[i])
  done;
  is_final auto !q

(**
  {b Precondition:} [auto] has no ε-transition.

  [accept_nfa_aux auto s ps i] returns [true] iff [s] is accepted by [auto].
  - [i] is the index of the currently dealt char in [s] ;
  - [ps] is an array of size [n], where [n] is the number of states in [auto].
    [ps.(p) = true] means that at the [i]-th step, [p] is a possible state to be in.

  {b Correctness:} Let [i ∈ [0, |s|-1]] and [p ∈ [0, n-1]].
  [ps.(p)] is [true] iff there exist states [p_0, p_1, ... p_{i-1}] such that
  [p_0 -s.[0]-> p_1 -s.[1]-> ... -s.[i-1]-> p] is a valid path in [auto].

  {b Terminaison:} [|s|-i] is a loop variant.

  {b Complexity:} in [O((n + |ẟ|) * |s|)], where [|ẟ|] is the number of
  transitions in [auto].
*)
let rec accept_nfa_aux (auto: auto) (s: string) (ps: bool array) (i: int) : bool =
  if i = String.length s then
    exists_i (fun p b -> b && is_final auto p) ps
  else
    let found = ref false in
    let n = size auto in
    let qs = Array.make n false in
    for p = 0 to n - 1 do
      if ps.(p) then
        List.iter (fun q -> found := true ; qs.(q) <- true) (trans auto p s.[i])
    done;
    !found && accept_nfa_aux auto s qs (i+1)

let accept_nfa (auto: auto) (s: string) : bool =
  assert (not (has_epsilon auto)) ;
  let n = size auto in
  let ps = Array.make n false in
  for p = 0 to n - 1 do
    if is_initial auto p then ps.(p) <- true;
  done;
  accept_nfa_aux auto s ps 0

let accept (auto: auto) (s: string) : bool =
  if is_det auto then accept_dfa auto s
  else accept_nfa (remove_eps_trans auto) s

(*****************************************************************************)
(*                                    is_empty                               *)
(*****************************************************************************)

let is_empty (auto: auto): bool =
  not (List.exists (is_final auto) (fst (forward auto)))

(*****************************************************************************)
(*                                    is_full                                *)
(*****************************************************************************)

(**
  [is_full_nfa sigma auto] returns [true] iff [auto] is full for the alphabet
  [sigma].
*)
let is_full_nfa (sigma: char list) (auto: auto) : bool =
  (*
    If [auto] if not full, then there exists a word [w]
    of size [< size auto] such that [w] is not recognized by
    [auto].
  *)
  set_power sigma (size auto)
  |> List.map (String.of_seq << List.to_seq)
  |> List.for_all (accept_nfa auto)

(**
  {b Precondition:} [auto] is a DFA.

  [is_full_dfa] returns [true] iff [auto] is full for the alphabet
  [sigma].
*)
let is_full_dfa (sigma: char list) (auto: auto) : bool =
  is_empty (complement sigma auto)

let is_full (sigma: char list) (auto: auto) : bool =
  if is_det auto then is_full_dfa sigma auto else is_full_nfa sigma auto

(*****************************************************************************)
(*                           Inclusion & Equivalence                         *)
(*****************************************************************************)

let included (auto1: auto) (auto2: auto) : bool =
  let sigma = sigma_of_auto auto1 in
  is_empty (diff sigma auto1 auto2)

let equiv (auto1: auto) (auto2: auto) : bool =
  included auto1 auto2 && included auto2 auto1

(*****************************************************************************)
(*                                                                           *)
(*                              RATIONAL EXPRESSIONS                         *)
(*                                                                           *)
(*****************************************************************************)

(*****************************************************************************)
(*                                  thompson                                 *)
(*****************************************************************************)

let thompson (re: Re.re) : auto =
  
  let n = ref (-1) in
  let trans = ref [] in
  let new_state () = incr n; !n in
  let add_trans t = trans := t :: !trans in
  
  let rec concat (q_in: int) (r1, r2: Re.re * Re.re) (q_out: int) =
    let (q_in1, q_out1) = loop r1 in
    let (q_in2, q_out2) = loop r2 in
    add_trans (q_in  , None, q_in1) ;
    add_trans (q_out1, None, q_in2) ;
    add_trans (q_out2, None, q_out)

  and alt (q_in: int) (r1, r2: Re.re * Re.re) (q_out: int) =
    let (q_in1, q_out1) = loop r1 in
    let (q_in2, q_out2) = loop r2 in
    add_trans (q_in  , None, q_in1) ;
    add_trans (q_in  , None, q_in2) ;
    add_trans (q_out1, None, q_out) ;
    add_trans (q_out2, None, q_out)
  
  and star (q_in: int) (r0: Re.re) (q_out: int) =
    let (q_in0, q_out0) = loop r0 in
    add_trans (q_in  , None, q_in0 ) ;
    add_trans (q_in0 , None, q_out0) ;
    add_trans (q_out0, None, q_in0 ) ;
    add_trans (q_out0, None, q_out )
  
  and loop (r: Re.re) : int * int =
    let q_in  = new_state () in
    let q_out = new_state () in
    begin match r with
      | Re.Empty           -> ()
      | Re.Epsilon         -> add_trans (q_in, None, q_out)
      | Re.Char c          -> add_trans (q_in, Some c, q_out)
      | Re.Concat (r1, r2) -> concat q_in (r1, r2) q_out
      | Re.Alt (r1, r2)    -> alt q_in (r1, r2) q_out
      | Re.Star (r0)       -> star q_in r0 q_out
    end ; (q_in, q_out)
  in

  let (q_in, q_out) = loop re in
  let auto = Auto.create (new_state ()) in
  set_initial auto q_in;
  set_final auto q_out;
  List.iter (fun (p, o, q) -> add_trans_opt auto p o q) !trans;
  auto

(*****************************************************************************)
(*                                 berry-sethi                               *)
(*****************************************************************************)

let berry_sethi (r: Re.re) : auto =

  let (n, arr, r') = Re.linearize r in
  let first = Re.first r' and last = Re.last r' in
  let auto = Auto.create (n+1) in

  (* Set initial / final states *)
  set_initial auto 0 ;
  if Re.has_epsilon r' then set_final auto 0 ;
  List.iter (set_final auto << Char.code) last ;

  (* Add transitions *)
  let add (p: int) (a: char) =
    let i = Char.code a in add_trans auto p arr.(i) i
  in
  
  List.iter (add 0) first ;
  for p = 1 to n do
    let follow = Re.follow r' (Char.chr p) in
    List.iter (add p) follow
  done ;
  
  auto