open Rational_language.Auto
open Rational_language.Auto_alg

(*****************************************************************************)
(*                              dummy automata                               *)
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
  set_final auto 3;
  add_trans auto 0 'a' 1;
  add_trans_eps auto 0 2;
  add_trans auto 1 'a' 0;
  add_trans auto 2 'a' 3;
  add_trans_eps auto 3 0;
  auto

(*****************************************************************************)
(*                                  forward                                  *)
(*****************************************************************************)

let test_forward_0 () =
  let a = example_auto_0 () in
  let (expected_accs, expected_naccs) = ([0;1;2;3], []) in
  let (obtained_accs, obtained_naccs) = forward a in
  Alcotest.(check (list int)) "" expected_accs obtained_accs;
  Alcotest.(check (list int)) "" expected_naccs obtained_naccs

let test_forward_1 () =
  let a = example_auto_1 () in
  let (expected_accs, expected_naccs) = ([0;1;2], [3]) in
  let (obtained_accs, obtained_naccs) = forward a in
  Alcotest.(check (list int)) "" expected_accs obtained_accs;
  Alcotest.(check (list int)) "" expected_naccs obtained_naccs

let tests_forward = "forward", [
  test_forward_0;
  test_forward_1
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
(*                                  backward                                 *)
(*****************************************************************************)

let test_backward_0 () =
  let a = example_auto_0 () in
  let (expected_accs, expected_naccs) = ([0;1;2;3], []) in
  let (obtained_accs, obtained_naccs) = backward a in
  Alcotest.(check (list int)) "" expected_accs obtained_accs;
  Alcotest.(check (list int)) "" expected_naccs obtained_naccs

let test_backward_1 () =
  let a = example_auto_1 () in
  let (expected_accs, expected_naccs) = ([0;1;3], [2]) in
  let (obtained_accs, obtained_naccs) = backward a in
  Alcotest.(check (list int)) "" expected_accs obtained_accs;
  Alcotest.(check (list int)) "" expected_naccs obtained_naccs

let tests_backward = "backward", [
  test_backward_0;
  test_backward_1
]

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
(*                                eps_closure                                *)
(*****************************************************************************)

let test_eps_closure_0 () =
  let obtained = eps_closure (example_auto_3 ()) in
  let expected = [| [ 0; 2 ] ; [ 1 ] ; [ 2 ] ; [ 0 ; 2 ; 3 ] |] in
  Alcotest.(check (array (list int))) "" expected obtained

let tests_eps_closure = "eps_closure", [
  test_eps_closure_0
]

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
  (*
   -> [0]                       [|1|]
       |                          ^
       ε                          ε
       v                          |
      [2] -a-> [3] -ε-> [4] -b-> [5]
  *)
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
  tests_forward;
  tests_is_empty;
  tests_backward;
  tests_clean;
  tests_eps_closure;
  tests_thompson;
  tests_berry_sethi
]

let format_tests tests =
  List.map (fun (name, fs) ->
    (name, List.map (Alcotest.test_case "" `Quick) fs)
  ) tests

let () = Alcotest.run "Rational language - automata algorithms" (format_tests tests)