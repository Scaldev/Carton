open Auto

let (<<) = Fun.compose

(*****************************************************************************)
(*                                    Utils                                  *)
(*****************************************************************************)

(**
  [range a b] returns the list [a ; a+1 ; ...; b-1].
  If [b <= a], returns the empty list.
*)
let range (a: int) (b: int) =
  let n = max 0 (b - a) in
  List.init n (fun i -> a + i)

(**
  [exists_i f arr] returns [f 0 arr.(0) || ... || f (n-1) arr.(n-1)],
  where [n = |arr|].
*)
let exists_i (f: int -> 'a -> bool) (arr: 'a array) : bool =
  let i = ref (-1) in
  Array.exists (fun a -> incr i ; f !i a) arr

(**
  {b Precondition:} [xs] and [ys] are sorted using [<] ordering,
  and don't contain any duplicates.

  [union xs ys] returns a sorted list of the elements from
  [xs] and [ys] without any duplicates.

  {b Complexity:} in [O(n)] time, [n] being the length of the result.
*)
let rec union (xs: int list) (ys: int list): int list =
  match xs, ys with
  | [], _ -> ys
  | _, [] -> xs
  | u :: us, v :: vs ->
    if      u = v then u :: union us vs
    else if u < v then u :: union us ys
    else               v :: union xs vs

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
(*                              is_empty & clean                             *)
(*****************************************************************************)

let is_empty (auto: auto): bool =
  not (List.exists (is_final auto) (fst (forward auto)))

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
  let after (p, q) = visited.(p) <- union visited.(p) visited.(q) in
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

  let n = size auto in
  let auto' = create n in
  let closures = eps_closure auto in

  for p = 0 to n-1 do

    if is_initial auto p then set_initial auto' p;
    if is_final   auto p then set_final   auto' p;

    (* if [p -a-> q] in [auto], then add [p -a-> q] in [auto'] *)
    add_trans_no_eps auto' p (trans_all_opt auto p) ;

    (* if [p -ε->* q -a-> r] in [auto], then add [p -a-> r] in [auto'] *)
    List.iter (add_trans_no_eps auto' p << trans_all_opt auto) closures.(p)
  
  done ;
  auto' (* some states might be removed *)

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

(*****************************************************************************)
(*                     Automata and rational expressions                     *)
(*****************************************************************************)

let thompson (re: Re.re) : auto =
  
  let n = ref (-1) in
  let trans = ref [] in
  let new_state () = incr n; !n in
  let add_trans t = trans := t :: !trans in
  
  (*
    [loop r] returns the pair [(q_in, q_out)] of states such that the automaton
    of [r] as [q_in] as its unique initial state and [q_out] as its unique final
    state.
  *)
  let rec loop (r: Re.re) : int * int =
    let q_in = new_state () in
    let q_out = new_state () in
    begin match r with
      | Re.Empty  -> ()
      | Re.Epsilon -> add_trans (q_in, None, q_out)
      | Re.Char c -> add_trans (q_in, Some c, q_out)
      | Re.Concat (r1, r2) -> (
        let (q_in1, q_out1) = loop r1 in
        let (q_in2, q_out2) = loop r2 in
        add_trans (q_in, None, q_in1) ;
        add_trans (q_out1, None, q_in2) ;
        add_trans (q_out2, None, q_out)
      )
      | Re.Alt (r1, r2) -> (
        let (q_in1, q_out1) = loop r1 in
        let (q_in2, q_out2) = loop r2 in
        add_trans (q_in, None, q_in1) ;
        add_trans (q_in, None, q_in2) ;
        add_trans (q_out1, None, q_out) ;
        add_trans (q_out2, None, q_out)
      )
      | Re.Star (r0) -> (
        let (q_in0, q_out0) = loop r0 in
        add_trans (q_in, None, q_in0) ;
        add_trans (q_in0, None, q_out0) ;
        add_trans (q_out0, None, q_in0) ;
        add_trans (q_out0, None, q_out)
      )
    end ; (q_in, q_out)
  in
  let (q_in, q_out) = loop re in
  let auto = Auto.create (new_state ()) in
  set_initial auto q_in;
  set_final auto q_out;
  List.iter (fun (p, o, q) -> add_trans_opt auto p o q) !trans;
  auto

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
    List.iter (fun a -> 
    print_string ("  follow " ^ string_of_int p ^ " -> " ^ (string_of_int (Char.code a)) ^ " (" ^ (String.make 1 arr.(Char.code a)) ^  ")\n") ; add p a) follow
  done ;
  
  auto