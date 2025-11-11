open Rational_language.Auto
open Rational_language.Auto_alg

(*****************************************************************************)
(*                                                                           *)
(*                                    UTILS                                  *)
(*                                                                           *)
(*****************************************************************************)

let example_auto_0 () : auto =
  (*
   -> [0] -a-> [|1|]
                 |
                 a
                 v
   -> [2] -a-> [|3|]
  *)
  let auto = create 4 in
  set_initial auto 0;
  set_initial auto 2;
  set_final auto 1;
  set_final auto 3;
  add_trans auto 0 'a' 1;
  add_trans auto 1 'a' 3;
  add_trans auto 2 'a' 3;
  auto

let example_auto_1 () : auto =
  (*
   -> [0] -a-> [|1|]
                 ^
                 a
                 |
   -> [2] <-a- [|3|]
  *)
  let auto = create 4 in
  set_initial auto 0;
  set_initial auto 2;
  set_final auto 1;
  set_final auto 3;
  add_trans auto 0 'a' 1;
  add_trans auto 3 'a' 1;
  add_trans auto 3 'a' 2;
  auto

let example_auto_2 () : auto =
  (*
   -> [0] <-a- [|1|]
  *)
  let auto = create 2 in
  set_initial auto 0;
  set_final auto 1;
  add_trans auto 1 'a' 0;
  auto

let example_auto_3 () : auto =
  (*
   -> [0] <-a-> [1]
       | ^  
       ε  \-ε- 
       v      \
   -> [2] -a-> [|3|]
  *)
  let auto = create 4 in
  set_initial auto 0;
  set_initial auto 2;
  set_final auto 3;
  add_trans auto 0 'a' 1;
  add_trans_eps auto 0 2;
  add_trans auto 1 'a' 0;
  add_trans auto 2 'a' 3;
  add_trans_eps auto 3 0;
  auto

let example_auto_4 () : auto =
  (*
  DFA that recognizes (a|b)*ba

      /a\       /b\
      \ v       \ v  
   -> [0]  -b-> [1] -a-> [|2|]
        ^         ^--b--/  |
         \-------a--------/
  *)
  let auto = create 3 in
  set_initial auto 0;
  set_final auto 2;
  add_trans auto 0 'a' 0;
  add_trans auto 0 'b' 1;
  add_trans auto 1 'a' 2;
  add_trans auto 1 'b' 1;
  add_trans auto 2 'a' 0;
  add_trans auto 2 'b' 1;
  auto

let example_auto_5 () : auto =
  (*
   -> [0] <-a-> [1] -b-> [|2|]
      / ^
      \a/   
  *)
  let auto = create 3 in
  set_initial auto 0;
  set_final auto 2;
  add_trans auto 0 'a' 0;
  add_trans auto 0 'a' 1;
  add_trans auto 1 'a' 0;
  add_trans auto 1 'b' 2;
  auto

let example_auto_6 () : auto =
  (*
  NFA that recognizes (a|b)*ba
      /a,b\      
      \   v      
   ->  [0] --b-> [1] --a-> [|2|]
        ^ ^--b--/   ^--b--/  |
         \--------a---------/
  *)
  let auto = create 3 in
  set_initial auto 0;
  set_final auto 2;
  add_trans auto 0 'a' 0;
  add_trans auto 0 'b' 0;
  add_trans auto 0 'b' 1;
  add_trans auto 1 'a' 2;
  add_trans auto 1 'b' 0;
  add_trans auto 2 'a' 0;
  add_trans auto 2 'b' 1;
  auto

let example_auto_7 () : auto =
  (*
    Automaton (not det) that recognizes a(aa)*b
    -> [0] <-a-> [1] -b-> [|2|] 
  *)
  let auto = create 3 in
  set_initial auto 0 ;
  set_final auto 2 ;
  add_trans auto 0 'a' 1 ;
  add_trans auto 1 'a' 0;
  add_trans auto 1 'b' 2 ;
  auto

let example_auto_8 () : auto =
  (*
    Automaton (det) that recognizes a(aa)*b
   -> [0] <-a-> [1] -b-> [|2|]
        \                /
         \              /
           b          a,b
            \-> [3] <-/
               /   ^
               \a,b/
  *)
  let auto = create 4 in
  set_initial auto 0 ;
  set_final auto 2 ;
  add_trans auto 0 'a' 1 ;
  add_trans auto 0 'b' 3 ;
  add_trans auto 1 'a' 0 ;
  add_trans auto 1 'b' 2 ;
  add_trans auto 2 'a' 3 ;
  add_trans auto 2 'b' 3 ;
  add_trans auto 3 'a' 3 ;
  add_trans auto 3 'b' 3 ;
  auto

let example_auto_9 () : auto =
  (*
    Automaton (not-semi-normalized) that recognizes (aa|b)(a|b)*:

   -> [0] -a-> [|1|]  
                 | 
                 a  
                 v 
   -> [2] -b-> [|3|]
               /   ^ 
               \a,b/ 
  *)
  let auto = create 4 in
  set_initial auto 0;
  set_initial auto 2;
  set_final auto 1;
  set_final auto 3;
  add_trans auto 0 'a' 1;
  add_trans auto 1 'a' 3;
  add_trans auto 2 'b' 3;
  add_trans auto 3 'a' 3;
  add_trans auto 3 'b' 3;
  auto

let example_auto_10 () : auto =
  (*
    Automaton (not-semi-normalized) that recognizes (aa|b)(a|b)*:
  *)
  let auto = create 5 in
  set_initial auto 0;
  set_final auto 2;
  set_final auto 4;
  add_trans_eps auto 0 1;
  add_trans_eps auto 0 3;
  add_trans auto 1 'a' 2;
  add_trans auto 2 'a' 4;
  add_trans auto 3 'b' 4;
  add_trans auto 4 'a' 4;
  add_trans auto 4 'b' 4;
  auto

let example_auto_11 () : auto =
  (*
    Automaton (not normalized) that recognizes b|(a+ε)(bb)*:

   ->  [0]  -b->  [|1|]       
        |           |
        a          a,b
        v           v
   -> [|2|] <-b->  [3] 
                   / ^
                   \a/
  *)
  let auto = create 4 in
  set_initial auto 0;
  set_initial auto 2;
  set_final auto 1;
  set_final auto 2;
  add_trans auto 0 'a' 2;
  add_trans auto 0 'b' 1;
  add_trans auto 1 'a' 3;
  add_trans auto 1 'b' 3;
  add_trans auto 2 'b' 3;
  add_trans auto 3 'a' 3;
  add_trans auto 3 'b' 2;
  auto
  
let example_auto_12 () : auto =
  (*
    Automaton (normalized) that recognizes b|(a+ε)(bb)*: 
  *)
  let auto = create 6 in
  set_initial auto 0;
  set_final auto 1;
  add_trans_eps auto 0 2;
  add_trans_eps auto 0 4;
  add_trans_eps auto 3 1;
  add_trans_eps auto 4 1;
  add_trans auto 2 'a' 4;
  add_trans auto 2 'b' 3;
  add_trans auto 3 'a' 5;
  add_trans auto 3 'b' 5;
  add_trans auto 4 'b' 5;
  add_trans auto 5 'a' 5;
  add_trans auto 5 'b' 4;
  auto

(*****************************************************************************)
(*                                                                           *)
(*                             AUTOMATON STRUCTURE                           *)
(*                                                                           *)
(*****************************************************************************)

(*****************************************************************************)
(*                                  is_clean                                 *)
(*****************************************************************************)

let test_is_clean_0 () =
  let expected = true in
  let obtained = is_clean (example_auto_0 ()) in
  Alcotest.(check bool) "" expected obtained

let test_is_clean_1 () =
  let expected = false in
  let obtained = is_clean (example_auto_1 ()) in
  Alcotest.(check bool) "" expected obtained

let tests_is_clean = "is_clean", [
  test_is_clean_0;
  test_is_clean_1
]

(*****************************************************************************)
(*                                 is_complete                               *)
(*****************************************************************************)

let test_is_complete_0 () =
  let expected = false in
  let obtained = is_complete ['a';'b'] (example_auto_7 ()) in
  Alcotest.(check bool) "" expected obtained

let test_is_complete_1 () =
  let expected = true in
  let obtained = is_complete ['a';'b'] (example_auto_8 ()) in
  Alcotest.(check bool) "" expected obtained

let tests_is_complete = "is_complete", [
  test_is_complete_0;
  test_is_complete_1
]

(*****************************************************************************)
(*                               is_semi_normalized                          *)
(*****************************************************************************)

let test_is_semi_normalized_0 () =
  let expected = false in
  let obtained = is_semi_normalized (example_auto_9 ()) in
  Alcotest.(check bool) "" expected obtained

let test_is_semi_normalized_1 () =
  let expected = true in
  let obtained = is_semi_normalized (example_auto_10 ()) in
  Alcotest.(check bool) "" expected obtained

let tests_is_semi_normalized = "is_semi_normalized", [
  test_is_semi_normalized_0;
  test_is_semi_normalized_1
]

(*****************************************************************************)
(*                               is_normalized                               *)
(*****************************************************************************)

let test_is_normalized_0 () =
  let expected = false in
  let obtained = is_normalized (example_auto_11 ()) in
  Alcotest.(check bool) "" expected obtained

let test_is_normalized_1 () =
  let expected = true in
  let obtained = is_normalized (example_auto_12 ()) in
  Alcotest.(check bool) "" expected obtained

let tests_is_normalized = "is_normalized", [
  test_is_normalized_0;
  test_is_normalized_1
]

(*****************************************************************************)
(*                                                                           *)
(*                                  MODIFYING                                *)
(*                                                                           *)
(*****************************************************************************)

(*****************************************************************************)
(*                                    clean                                  *)
(*****************************************************************************)

let test_clean_0 () =
  let a = clean (example_auto_0 ()) in
  let a' = example_auto_0 () in
  let expected = true in
  let obtained = eq a a' in
  Alcotest.(check bool) "" expected obtained

let test_clean_1 () =
  let a = clean (example_auto_1 ()) in
  let a' = create 2 in
  set_initial a' 0;
  set_final a' 1;
  add_trans a' 0 'a' 1;
  let expected = true in
  let obtained = eq a a' in
  Alcotest.(check bool) "" expected obtained

let tests_clean = "clean", [
  test_clean_0;
  test_clean_1
]

(*****************************************************************************)
(*                                remove_eps_trans                           *)
(*****************************************************************************)

let test_remove_eps_trans_0 () =
  let a = remove_eps_trans (example_auto_3 ()) in
  let a' = create 4 in
  set_initial a' 0;
  set_initial a' 2;
  set_final a' 3;
  add_trans a' 0 'a' 1;
  add_trans a' 1 'a' 0;
  add_trans a' 0 'a' 3;
  add_trans a' 2 'a' 3;
  add_trans a' 3 'a' 1;
  add_trans a' 3 'a' 3;

  (*
   -> [0] <-a-> [1]       -> [0] <-a->  [1]
       | ^                    ^  \       ^
       ε  \-ε-        ==>     a   --a-\  a
       v      \               |        v |
   -> [2] -a-> [|3|]      -> [2]  -a-> [|3|]
  *)
  let expected = true in
  let obtained = eq a a' in
  Alcotest.(check bool) "" expected obtained

let tests_remove_eps_trans = "remove_eps_trans", [
  test_remove_eps_trans_0
]

(*****************************************************************************)
(*                                    make_det                               *)
(*****************************************************************************)

let test_make_det_0 () =
  let a = make_det (example_auto_6 ()) in
  let a' = example_auto_4 () in
  let expected = true in
  let obtained = eq a a' in
  Alcotest.(check bool) "" expected obtained

let tests_make_det = "make_det", [
  test_make_det_0
]

(*****************************************************************************)
(*                                    complete                               *)
(*****************************************************************************)

let test_complete_0 () =
  let auto = complete ['a';'b'] (example_auto_7 ()) in
  let auto' = example_auto_8 () in
  let expected = true in
  let obtained = eq auto auto' in
  Alcotest.(check bool) "" expected obtained

let tests_complete = "complete", [
  test_complete_0
]

(*****************************************************************************)
(*                                 semi_normalize                            *)
(*****************************************************************************)

let test_semi_normalize_0 () =
  let auto = semi_normalize (example_auto_9 ()) in
  let auto' = example_auto_10 () in
  let expected = true in
  let obtained = eq auto auto' in
  Alcotest.(check bool) "" expected obtained

let tests_semi_normalize = "semi_normalize", [
  test_semi_normalize_0
]

(*****************************************************************************)
(*                                   normalize                               *)
(*****************************************************************************)

let test_normalize_0 () =
  let auto = normalize (example_auto_11 ()) in
  let auto' = example_auto_12 () in
  let expected = true in
  let obtained = eq auto auto' in
  Alcotest.(check bool) "" expected obtained

let tests_normalize = "normalize", [
  test_normalize_0
]

(*****************************************************************************)
(*                                                                           *)
(*                              BOOLEAN OPERATIONS                           *)
(*                                                                           *)
(*****************************************************************************)

(*****************************************************************************)
(*                                                                           *)
(*                                   ACCEPTANCE                              *)
(*                                                                           *)
(*****************************************************************************)

(*****************************************************************************)
(*                                     accept                                *)
(*****************************************************************************)

let test_accept_0 () =
  let a = example_auto_5 () in
  let expected = false in
  let obtained = accept a (String.make 10_000 'a') in
  Alcotest.(check bool) "" expected obtained

let test_accept_1 () =
  let a = example_auto_5 () in
  let expected = true in
  let obtained = accept a (String.make 10_000 'a' ^ "b") in
  Alcotest.(check bool) "" expected obtained

let tests_accept = "accept", [
  test_accept_0;
  test_accept_1
]

(*****************************************************************************)
(*                                  is_empty                                 *)
(*****************************************************************************)

let test_is_empty_0 () =
  let a = example_auto_0 () in
  let expected = false in
  let obtained = is_empty a in
  Alcotest.(check bool) "" expected obtained

let test_is_empty_1 () =
  let a = example_auto_2 () in
  let expected = true in
  let obtained = is_empty a in
  Alcotest.(check bool) "" expected obtained

let tests_is_empty = "is_empty", [
  test_is_empty_0;
  test_is_empty_1
]

(*****************************************************************************)
(*                                                                           *)
(*                              RATIONAL EXPRESSIONS                         *)
(*                                                                           *)
(*****************************************************************************)

(*****************************************************************************)
(*                                 thompson                                  *)
(*****************************************************************************)

let test_thompson_0 () =
  (*
   -> [0]      [|1|]
  *)
  let a = thompson Empty in
  let a' = create 2 in
  set_initial a' 0 ;
  set_final a' 1 ;
  let expected = true in
  let obtained = eq a a' in
  Alcotest.(check bool) "" expected obtained

let test_thompson_1 () =
  (*
   -> [0] -ε-> [|1|]
  *)
  let a = thompson Epsilon in
  let a' = create 2 in
  set_initial a' 0 ;
  set_final a' 1 ;
  add_trans_eps a' 0 1 ;
  let expected = true in
  let obtained = eq a a' in
  Alcotest.(check bool) "" expected obtained

let test_thompson_2 () =
  (*
   -> [0] -a-> [|1|]
  *)
  let a = thompson (Char 'a') in
  let a' = create 2 in
  set_initial a' 0 ;
  set_final a' 1 ;
  add_trans a' 0 'a' 1 ;
  let expected = true in
  let obtained = eq a a' in
  Alcotest.(check bool) "" expected obtained

let test_thompson_3 () =
  let a = thompson (Concat (Char 'a', Char 'b')) in
  let a' = create 6 in
  set_initial a' 0 ;
  set_final a' 1 ;
  add_trans_eps a' 0 2 ;
  add_trans a' 2 'a' 3 ;
  add_trans_eps a' 3 4 ;
  add_trans a' 4 'b' 5 ;
  add_trans_eps a' 5 1 ;
  let expected = true in
  let obtained = eq a a' in
  Alcotest.(check bool) "" expected obtained

let test_thompson_4 () =
  (*
          ε-> [2] -a-> [3] -ε  
         /                   \
   -> [0]                     > [|1|]
         \                   /     
          ε-> [4] -b-> [5] -ε
  *)
  let a = thompson (Alt (Char 'a', Char 'b')) in
  let a' = create 6 in
  set_initial a' 0 ;
  set_final a' 1 ;
  add_trans_eps a' 0 2 ;
  add_trans_eps a' 0 4 ;
  add_trans a' 2 'a' 3 ;
  add_trans a' 4 'b' 5 ;
  add_trans_eps a' 3 1 ;
  add_trans_eps a' 5 1 ;
  let expected = true in
  let obtained = eq a a' in
  Alcotest.(check bool) "" expected obtained

let test_thompson_5 () =
  (*
   -> [0] -ε-> [2] -a-> [3] -ε-> [|1|] 
                 ^     ^ 
                  \-ε-/
  *)
  let a = thompson (Star (Char 'a')) in
  let a' = create 4 in
  set_initial a' 0 ;
  set_final a' 1 ;
  add_trans_eps a' 0 2 ;
  add_trans_eps a' 2 3 ;
  add_trans a' 2 'a' 3 ;
  add_trans_eps a' 3 2 ;
  add_trans_eps a' 3 1 ;
  let expected = true in
  let obtained = eq a a' in
  Alcotest.(check bool) "" expected obtained

let tests_thompson = "thompson", [
  test_thompson_0;
  test_thompson_1;
  test_thompson_2;
  test_thompson_3;
  test_thompson_4;
  test_thompson_5
]

(*****************************************************************************)
(*                                berry_sethi                                *)
(*****************************************************************************)

let test_berry_sethi_0 () =
  let a = berry_sethi (Alt (Char 'a', Star (Char 'b'))) in
  let a' = create 3 in
  set_initial a' 0;
  set_final a' 0;
  set_final a' 1;
  set_final a' 2;
  add_trans a' 0 'a' 1;
  add_trans a' 0 'b' 2;
  add_trans a' 2 'b' 2;
  let expected = true in
  let obtained = eq a a' in
  Alcotest.(check bool) "" expected obtained

let test_berry_sethi_1 () =
  let a = berry_sethi
    (Alt (
      Concat (Char 'a', Star (Concat (Char 'a', Char 'b'))),
      Concat (Star (Char 'b'), Char 'a')
    )) 
  in
  let a' = create 6 in
  set_initial a' 0;
  set_final a' 1;
  set_final a' 3;
  set_final a' 5;
  add_trans a' 0 'a' 1;
  add_trans a' 0 'b' 4;
  add_trans a' 0 'a' 5;
  add_trans a' 1 'a' 2;
  add_trans a' 2 'b' 3;
  add_trans a' 3 'a' 2;
  add_trans a' 4 'b' 4;
  add_trans a' 4 'a' 5;
  let expected = true in
  let obtained = eq a a' in
  Alcotest.(check bool) "" expected obtained

let tests_berry_sethi = "berry_sethi", [
  test_berry_sethi_0;
  test_berry_sethi_1
]

(*****************************************************************************)

let tests = [

  (* Structure                 *)
  tests_is_clean;
  tests_is_complete;
  tests_is_semi_normalized;
  tests_is_normalized;

  (* Modifying                 *)
  tests_clean;
  tests_remove_eps_trans;
  tests_make_det;
  tests_complete;
  tests_semi_normalize;
  tests_normalize;

  (* Boolean operations        *)
  (* tests_complement;         *)
  (* tests_union;              *)
  (* tests_inter;              *)
  (* tests_diff;               *)

  (* Acceptance                *)
  tests_accept;
  tests_is_empty;
  (* tests_is_full;            *)
  (* tests_included;           *)
  (* tests_equiv;              *)

  (* Rational expressions      *)
  tests_thompson;
  tests_berry_sethi
]

let format_tests tests =
  List.map (fun (name, fs) ->
    (name, List.map (Alcotest.test_case "" `Quick) fs)
  ) tests

let () = Alcotest.run "Rational language - automata algorithms" (format_tests tests)