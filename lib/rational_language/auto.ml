(**
  Finite automata type, that can represent DFA, NFA or epsilon-NFA.
  Mutable structure.
  States are integers [0, ..., n-1].
  The alphabet [Σ] of that automata is the [char] type.

  Based on https://informatique-mpi.fr/files/code/auto.mli.html .
*)
type auto = {
  trans : int list array array; (* state -> chars -> list of states *)
  initial : bool array;
  final : bool array
}

let eps_code = 256

(*****************************************************************************)
(*                             Automaton creation                            *)
(*****************************************************************************)

let create (n: int) : auto =
  {
    trans   = Array.init n (fun _ -> Array.make 257 []);
    initial = Array.make n false;
    final   = Array.make n false
  }

let eq (a1: auto) (a2: auto) : bool =
     a1.trans   = a2.trans
  && a1.initial = a2.initial
  && a1.final   = a2.final

let copy (a: auto) : auto =
  {
    trans   = Array.map Array.copy a.trans;
    initial = Array.copy a.initial;
    final   = Array.copy a.final
  }

(*****************************************************************************)
(*                            Automaton properties                           *)
(*****************************************************************************)

let size (a: auto) : int =
  Array.length a.trans

let is_initial (a: auto) (q: int) : bool =
  assert (0 <= q && q < size a);
  a.initial.(q)

let is_final (a: auto) (q: int) : bool =
  assert (0 <= q && q < size a);
  a.final.(q)

let has_epsilon (a: auto) : bool =
  Array.exists (fun q -> q.(eps_code) <> []) a.trans

(**
  [has_size_leq_1 xs] returns [true] iff [List.length xs <= 1],
  in [O(1)] time complexity.
*)
let has_size_leq_1 (xs: 'a list) : bool =
  match xs with
  | [] | [ _ ] -> true
  | _          -> false

(**
  [is_init_unique a] returns [true] iff the automaton [a] has exactly one
  initial state.
*)
let is_init_unique (a: auto) : bool =
  let count = ref 0 in
  Array.iter (fun b -> if b then incr count) a.initial;
  !count = 1

let is_det (a: auto) : bool =
  is_init_unique a
  && not (has_epsilon a)
  && Array.for_all (Array.for_all has_size_leq_1) a.trans

let trans (a: auto) (p: int) (c: char) : int list =
  assert (0 <= p && p < size a);
  a.trans.(p).(Char.code c)

let trans_eps (a: auto) (p: int) : int list =
  a.trans.(p).(eps_code)

let trans_all (a: auto) (p: int) : (char * int list) list * int list =
  let char_trans = ref [] in
  Array.iteri (fun i qs ->
    if i <> eps_code && qs <> [] then
      char_trans := (Char.chr i, qs) :: !char_trans
  ) a.trans.(p);
  (!char_trans, trans_eps a p)

let trans_all_opt (a: auto) (p: int) : (char option * int) list =
  let ts = ref [] in
  Array.iteri (fun i qs ->
    if i <> eps_code then
      List.iter (fun q -> ts := (Some (Char.chr i), q) :: !ts) qs
  ) a.trans.(p);
  List.iter (fun q -> ts := (None, q) :: !ts) (trans_eps a p);
  !ts

(*****************************************************************************)
(*                              Automaton edits                              *)
(*****************************************************************************)

(**
  [insert x xs] returns the unique sorted list containing all elements of [xs]
  and [x] without duplicates.

  {b Precondition:} [xs] is a sorted list of integeres without duplicates.
*)
let rec insert (x: int) (xs: int list) : int list =
  match xs with
  | []      -> [x]
  | y :: ys -> if x = y then xs
          else if x < y then x :: xs
          else               y :: insert x ys

(**
  [add_trans_aux a p i q] adds the [p -c-> q] transition to the automaton
  [a], where [c = ε] iff [i = 256], or else [c] is a [Char.chr].

  {b Precondition:} [0 <= i <= 256].
*)
let add_trans_aux (a: auto) (p: int) (i: int) (q: int) : unit =
  assert (0 <= i && i <= 256);
  a.trans.(p).(i) <- insert q a.trans.(p).(i)

let add_trans (a: auto) (p: int) (c: char) (q: int) : unit =
  assert (0 <= p && p < size a && 0 <= q && q < size a);
  add_trans_aux a p (Char.code c) q

let add_trans_eps (a: auto) (p: int) (q: int) : unit =
  assert (0 <= p && p < size a && 0 <= q && q < size a);
  add_trans_aux a p eps_code q

let add_trans_opt (a: auto) (p: int) (o: char option) (q: int) : unit =
  assert (0 <= p && p < size a && 0 <= q && q < size a);
  match o with
  | None   -> add_trans_eps a p q
  | Some c -> add_trans a p c q

let set_initial (a: auto) (q: int) : unit =
  assert (0 <= q && q < size a);
  a.initial.(q) <- true

let unset_initial (a: auto) (q: int) : unit =
  assert (0 <= q && q < size a);
  a.initial.(q) <- false

let set_final (a: auto) (q: int) : unit =
  assert (0 <= q && q < size a);
  a.final.(q) <- true

let unset_final (a: auto) (q: int) : unit =
  assert (0 <= q && q < size a);
  a.final.(q) <- false

(**
  [build_removal_map states] returns a [(int, int)] map, such that
  [Hashtbl.mem smap s] is the state in the cleared automaton.
  If the state has no image, the value is -1.
*)
let build_removal_map (states: int list) : (int, int) Hashtbl.t =
  let smap = Hashtbl.create (List.length states) in
  List.iter (fun s -> Hashtbl.replace smap s (-1)) states;
  smap

(**
  [all_trues_removed smap qs] returns [true] iff [qs.(q) = true] implies
  [Hashtbl.mem smap q] for all [q] such that [0 <= q < |xs|].

  Here, [Hashtbl.mem smap q] implies that the value is -1, i.e. [q] is
  a deleted state.
*)
let all_trues_removed smap (qs: bool array) : bool =
  qs
  |> Array.mapi (fun q b -> (q, b))
  |> Array.for_all (fun (q, b) -> not b || Hashtbl.mem smap q)

(**
  [build_state_mapping smap size] edits [smap] such that for all states
  [q] from [0] to [size-1], [q] is mapped to the corresponding state [nq]
  in the new automaton [nauto].
*)
let build_state_mapping smap (size: int) : unit =
  let nq = ref 0 in
  for q = 0 to size - 1 do
    if not (Hashtbl.mem smap q) then (
      Hashtbl.replace smap q !nq;
      incr nq;
    )
  done

(**
  {b Precondition:} [|arr| = |narr|] and [|narr.(i)| = false] for all [i].

  [copy_flags smap arr narr] updates [narr] such that for all states
  [q] between [0] and [|arr|-1], the corresponding state [nq] in [narr]
  is true iff [arr.(q)] is.
*)
let copy_flags smap (arr: bool array) (narr: bool array) : unit =
  Array.iteri (fun q b -> 
    if b then
      let nq = Hashtbl.find smap q in
      if nq <> -1 then
        narr.(nq) <- b
  ) arr

(**
  [copy_transitions_from_state smap auto nauto p] copies all the transitions
  [p -o-> q] starting from [p] in [auto], to [np -o-> nq] in [nauto], where
  [p] and [q] are the image of [p] and [q] in [smap].
*)
let copy_transitions_from_state smap (auto: auto) (nauto: auto) (p: int) : unit =
  List.iter (fun (o, q) ->
    let np = Hashtbl.find smap p in
    let nq = Hashtbl.find smap q in
    if Hashtbl.find smap q >= 0 then
      add_trans_opt nauto np o nq
  ) (trans_all_opt auto p)

(**
  [copy_transitions smap auto nauto] copies, for all states [p]
  with a corresponding state [np] in [nauto] with [smap], all the
  transitions [p -o-> q] to [np -o-> nq] in [nauto].

  In other words, [nauto] is updated with transitions [np -o-> nq]
  iff [p -o-> q] is in [auto], [p] and [q] are kept and [np] and [nq]
  are the corresponding states of [p] and [q] according to [smap].
*)
let copy_transitions smap (auto: auto) (nauto: auto) : unit =
  for p = 0 to size auto - 1 do
    if Hashtbl.find smap p <> -1 then
      copy_transitions_from_state smap auto nauto p
  done

(**
  [remove_states auto states] returns a new automaton corresponding to
  the automaton [auto] after removing the states in [states].
*)
let remove_states (auto : auto) (states : int list) : auto =

  let smap = build_removal_map states in
  if      all_trues_removed smap auto.initial then create 1
  else if all_trues_removed smap auto.final   then create 1
  else (
    build_state_mapping smap (size auto);
    let n = (Hashtbl.fold (fun _ -> max) smap (-1)) + 1 in
    let nauto = create n in
    copy_flags smap auto.initial nauto.initial;
    copy_flags smap auto.final nauto.final;
    copy_transitions smap auto nauto;
    nauto
  )

(*****************************************************************************)
(*                              Print automaton                              *)
(*****************************************************************************)

(**
  [print_trans fmt p i qs] prints the [p -c-> q] transition, where [c] is the
  char of int [i].
*)
let print_trans fmt (p: int) (i: int) (qs: int list) : unit =
  List.iter (fun q ->
    let c = if i < 256 then String.make 1 (Char.chr i) else "ε" in
    Format.fprintf fmt "%d -%s-> %d@\n" p c q
  ) qs

let print (fmt: Format.formatter) (a: auto) : unit =
  Format.fprintf fmt "size: %d@\n" (size a);
  for i = 0 to size a - 1 do
    if is_initial a i then Format.fprintf fmt "initial %d@\n" i;
    if is_final   a i then Format.fprintf fmt "final %d@\n"   i;
  done;
  for i = 0 to size a - 1 do
    Array.iteri (print_trans fmt i) a.trans.(i)
  done

let print_dot (fmt: Format.formatter) (a: auto) : unit =
  Format.fprintf fmt
    "digraph auto {\n\
    \  fontname=\"Helvetica,Arial,sans-serif\"\n\
    \  node [style=filled,fontname=\"Helvetica,Arial,sans-serif\"]\n\
    \  edge [fontname=\"Helvetica,Arial,sans-serif\"]\n\
    \  rankdir=LR;\n";
  
  (* States *)
  let one_init = is_init_unique a in

  (* If there is exactly one initial state, add arrow to it *)
  if one_init then (
    let q_0 = Option.get (Array.find_index Fun.id a.initial) in
    Format.fprintf fmt "  init [shape=point,style=invis];\n  init -> %d" q_0
  );

  (* Else, initial states are colored blue and final states are double-circled *)
  for i = 0 to size a - 1 do
    let shape = if is_final a i then "doublecircle" else "circle" in
    let color = if not one_init && is_initial a i then "dodgerblue" else "white" in
    Format.fprintf fmt "  %d [shape=%s, fillcolor=%s];\n" i shape color;
  done;

  (* Transitions *)
  let trans i n j =
    let c = Char.chr n in
    let s =
      if (65 <= n && n <= 90) || (97 <= n && n <= 122) then String.make 1 c
      else string_of_int n
    in
    Format.fprintf fmt "  %d -> %d [label = \"%s\"];@\n" i j s
  in
  let trans i c l = List.iter (trans i c) l in
  let trans i = Array.iteri (trans i) in
  Array.iteri trans a.trans;

  Format.fprintf fmt "}@\n"

let print_dot_to_file (file: string) (a: auto) =
  let c = open_out file in
  let fmt = Format.formatter_of_out_channel c in
  Format.fprintf fmt "%a@." print_dot a;
  close_out c
