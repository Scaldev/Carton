open Auto

let dfs (a: Auto.auto) : bool array =
  let n = size a in
  let visited = Array.make n false in
  let rec dfs (q: int) =
    if not visited.(q) then (
      visited.(q) <- true;
      List.iter dfs (List.map snd (trans_all_opt a q))
    )
  in
  for q = 0 to n - 1 do dfs q done;
  visited
