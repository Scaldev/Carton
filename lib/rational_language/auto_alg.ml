open Auto

let (<<) = Fun.compose

(**
  [array_elements arr]  returns a [(accs, naccs)] pair, such that [accs] is the
  list of accessible states in [auto], and [naccs] the remaining unaccessibles
  states.
*)
let array_elements (arr: bool array) : int list * int list =
  let qs    = List.init (Array.length arr) Fun.id in
  let accs  = List.filter (Array.get arr) qs in
  let naccs = List.filter (not << Array.get arr) qs in
  (accs, naccs)

let forward (auto: auto) : int list * int list =

  let n = size auto in
  let visited = Array.make n false in
  let rec loop (q: int) : unit =
    if not visited.(q) then (
      visited.(q) <- true;
      List.iter (loop << snd) (trans_all_opt auto q)
    ) in
  for q = 0 to n-1 do
    if is_initial auto q then loop q
  done;
  array_elements visited

let is_empty (auto: auto): bool =
  not (List.exists (is_final auto) (fst (forward auto)))

let backward (auto: auto) : int list * int list =

  let n = size auto in
  let prevs = Array.make n [] in
  let set_prev (p: int) (q: int) =
    prevs.(q) <- p :: prevs.(q)
  in
  let finals = ref [] in

  for p = 0 to n - 1 do
    if is_final auto p then finals := p :: !finals;
    let qs = trans_all_opt auto p in
    List.iter (set_prev p << snd) qs
  done;

  let visited = Array.make n false in
  let rec loop (q: int) =
    if not visited.(q) then
      visited.(q) <- true;
      List.iter loop prevs.(q)
  in
  List.iter loop !finals;
  array_elements visited
  
let clean (auto: auto) : auto =
  let (_, naccs) = forward auto in
  let (_, ncoaccs) = backward auto in
  Auto.remove_states auto (naccs @ ncoaccs)

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

let eps_closure (auto: auto) : int list array =

  let n = size auto in
  let visited = Array.make n [] in

  let rec visit (p: int) : unit =
    if List.is_empty visited.(p) then
      visited.(p) <- [ p ];
      let qs = Auto.trans_eps auto p in
      List.iter (fun q ->
        visit q;
        visited.(p) <- union visited.(p) visited.(q)
      ) qs
  in

  for p = 0 to n-1 do visit p done;
  visited

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