open Auto

let (<<) = Fun.compose

let dfs (a: auto) : bool array =
  let n = size a in
  let visited = Array.make n false in
  let rec loop (q: int) =
    if not visited.(q) then (
      visited.(q) <- true;
      List.iter (loop << snd) (trans_all_opt a q)
    ) in
  for q = 0 to n-1 do
    if is_initial a q then loop q
  done;
  visited

let array_elements (arr: bool array) : int list * int list =
  let qs    = List.init (Array.length arr) Fun.id in
  let accs  = List.filter (Array.get arr) qs in
  let naccs = List.filter (not << Array.get arr) qs in
  (accs, naccs)

let forward (auto: auto) : int list * int list =
  array_elements (dfs auto)

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