let (<<) = Fun.compose

(*****************************************************************************)
(*                            Letters and factors                            *)
(*****************************************************************************)

let occurrences (w: string) (l: char) : int =
  String.fold_right ((+) << Bool.to_int << ((==) l)) w 0

let is_prefix (w: string) (u: string) : bool =
  let rec aux (i: int) =
    if      i == String.length u then true
    else if i >= String.length w then false
    else if w.[i] == u.[i]       then aux (i + 1)
    else false
  in aux 0

let is_suffix (w: string) (u: string) : bool =
  let lw = String.length w in
  let lu = String.length u in
  let rec aux (i: int) =
    if      i          == lu         then true
    else if i          >= lw         then false
    else if w.[lw-1-i] == u.[lu-1-i] then aux (i + 1)
    else false
  in aux 0

let is_factor (w: string) (u: string) : bool =
  let len_w = String.length w in
  let len_u = String.length u in
  let rec aux (i: int) =
    if i + len_u > len_w then false
    else is_prefix (String.sub w i (len_w - i)) u || aux (i + 1)
  in aux 0

(*****************************************************************************)
(*                                Periodicity                                *)
(*****************************************************************************)

let is_period_of_word (w: string) (p: int) : bool =
  let res = ref true in
  for i = 1 to String.length w - p do
    if w.[i-1] <> w.[i+p-1] then
      res := false
  done;
  !res

let periods_of (w: string) : int list =
  List.init (String.length w) ((+) 1)
  |> List.filter (is_period_of_word w)

let is_primitive (w: string) : bool =
  List.length (periods_of w) == 1

type factorized = string * string * int

let factorize (w: string) (p: int) : factorized =
  let len_w = String.length w in
  let len_u = len_w mod p in
  let len_v = p - len_u in
  let u = String.sub w (len_w - len_u) len_u in
  let v = String.sub w  len_u          len_v in
  let k = len_w / p in
  (u, v, k)

(**
  The following functions implement an algorithm built upon
  Guidas-Odlyzko's thereom.
  @see guibas_odlyzko's specification for more details.
*)

(**
  [repeat s n] returns the concatenation of [n]
  strings [s].
*)
let rec repeat (s: string) (n: int) : string =
  match n with
  | 0 -> ""
  | _ -> s ^ repeat s (n-1)

(**
  [eval_facto (u,v,k)] returns [((uv)^k)u].
*)
let eval_facto ((u,v,k): factorized) : string =
  repeat (u ^ v) k ^ u

(**
  [smallest_period_of w] returns the smallest integer [p]
  such that [is_period_of_word w p] returns [true].
*)
let smallest_period_of (w: string) : int =
  let found = ref false in
  let res = ref 1 in
  while !res <= String.length w && not !found do
    if is_period_of_word w !res then
      found := true
    else
      incr res
  done; !res

(**
  Same as [guibas_odlyzko], but [w] is represented as
  [((uv)^k u)], and [w'] as [((u'v')^k u'].
*)
let rec guibas_odlyzko_aux ((u,v,k): factorized) : factorized =
  if k == 1 then
    let ones = String.make (String.length v - 1) '1' in
    if String.length u == 0 then
      ("", "0" ^ ones, 1)
    else
      let u' = eval_facto (guibas_odlyzko_aux ("", u, 1)) in
      (u', ones ^ "1", 1)
  else
    let (u', v', _) = guibas_odlyzko_aux (u, v, 1) in
    (u', v', k)

let guibas_odlyzko (w: string) : string =
  factorize w (smallest_period_of w)
  |> guibas_odlyzko_aux
  |> eval_facto

(*****************************************************************************)
(*                              Infinite words                               *)
(*****************************************************************************)

module CharSet = Set.Make(Char)

type morphism = (char, string) Hashtbl.t

let morphism_of_list (xs: (char * string) list): morphism =
  Hashtbl.of_seq (List.to_seq xs)

(**
  [max_length_in_image psi] returns
  [max Im(psi)], with [max a b = a] iff [|a| >= |b|].
*)
let max_length_in_image (psi: morphism): int =
  let f = fun acc s -> min acc (String.length s) in
  Seq.fold_left f (-Int.min_int) (Hashtbl.to_seq_values psi)

exception Invalid_word of string

let app (psi: morphism) (w: string) : string =

  String.iter (fun c ->
    if not (Hashtbl.mem psi c) then
      raise (Invalid_word ("psi(" ^ (String.make 1 c) ^ ") is undefined."));
  ) w;
  
  let len = max_length_in_image psi in
  let buf = Buffer.create (String.length w * len) in
  String.iter (Buffer.add_string buf << Hashtbl.find psi) w;
  Buffer.contents buf

let is_endomorphism (psi: morphism): bool =
  let alphabet = CharSet.of_seq (Hashtbl.to_seq_keys psi) in
  let is_word_of_lang = String.for_all (fun c -> CharSet.mem c alphabet) in
  Seq.for_all is_word_of_lang (Hashtbl.to_seq_values psi)

let rec iter_app (psi: morphism) (w: string) (n: int) : string =
  match n with
  | 0 -> w
  | _ -> iter_app psi (app psi w) (n-1)