open Rational_language.Auto

(*****************************************************************************)
(*                              dummy automaton                              *)
(*****************************************************************************)

let example_auto_0 () : auto =
  (* [|0|]     [1] <- *)
  let auto = create 2 in
  set_initial auto 0;
  set_final auto 1;
  unset_initial auto 0;
  unset_final auto 1;
  set_initial auto 1;
  set_final auto 0;
  auto

let example_auto_1 () : auto =
  (*
      -b-       -b-
      \ /       \ /
   -> [0] -a-> [|1|]
  *)
  let auto = create 2 in
  set_initial auto 0;
  set_final auto 1;
  add_trans auto 0 'a' 1;
  add_trans auto 0 'b' 0;
  add_trans auto 1 'a' 0;
  add_trans auto 1 'b' 1;
  auto

let example_auto_2 () : auto =
  (*
      -a-       -a-
      \ /       \ /
   -> [0] -ε-> [|1|]
  *)
  let auto = create 2 in
  set_initial auto 0;
  set_final auto 1;
  add_trans_opt auto 0 None 1;
  add_trans_opt auto 0 (Some 'a') 0;
  add_trans_opt auto 1 (Some 'a') 1;
  auto

let example_auto_3 () : auto =
  (*
      -a-       -a-
      \ /       \ /
   -> [0] -a-> [|1|]
  *)
  let auto = create 2 in
  set_initial auto 0;
  set_final auto 1;
  add_trans auto 0 'a' 0;
  add_trans auto 0 'a' 1;
  add_trans auto 1 'a' 1;
  auto

let example_auto_4 () : auto =
  (*
      -a-         -a-
      \ /         \ /
   -> [0] -a,b-> [|1|]
  *)
  let auto = create 2 in
  set_initial auto 0;
  set_final auto 1;
  add_trans auto 0 'a' 1;
  add_trans auto 0 'b' 1;
  add_trans auto 0 'b' 1; (* for coverage *)
  add_trans auto 0 'a' 0;
  add_trans auto 1 'a' 1;
  auto

let example_auto_5 () : auto =
  (*
       -a-       
       \ /    
    -> [0] -a,b-> [1]
        |          ^
        ε          b
        v          |
   -> [|2|] -a-> [|3|]
  *)
  let auto = create 4 in
  set_initial auto 0;
  set_initial auto 2;
  set_final auto 2;
  set_final auto 3;
  add_trans auto 0 'a' 0;
  add_trans auto 0 'a' 1;
  add_trans auto 0 'b' 1;
  add_trans_eps auto 0 2;
  add_trans auto 2 'a' 3;
  add_trans auto 3 'a' 1;
  auto

(*****************************************************************************)
(*                                     eq                                    *)
(*****************************************************************************)

let test_eq_0 () =
  let a1 = example_auto_1 () in
  let a2 = copy a1 in
  let expected = true in
  let obtained = eq a1 a2 in
  Alcotest.(check bool) "" expected obtained

let test_eq_1 () =
  let a1 = example_auto_0 () in
  let a2 = create 2 in
  set_initial a2 1;
  set_final a2 0;
  let expected = true in
  let obtained = eq a1 a2 in
  Alcotest.(check bool) "" expected obtained

let tests_eq = "eq", [
  test_eq_0;
  test_eq_1
]

(*****************************************************************************)
(*                                     size                                  *)
(*****************************************************************************)

let test_size_0 () =
  let a1 = example_auto_1 () in
  let obtained = size a1 in
  let expected = 2 in
  Alcotest.(check int) "" expected obtained


let tests_size = "size", [
  test_size_0
]

(*****************************************************************************)
(*                                 is_initial                                *)
(*****************************************************************************)

let test_is_initial_0 () =
  let a1 = example_auto_1 () in
  let obtained = is_initial a1 0 in
  let expected = true in
  Alcotest.(check bool) "" expected obtained

let test_is_initial_1 () =
  let a1 = example_auto_1 () in
  let obtained = is_initial a1 1 in
  let expected = false in
  Alcotest.(check bool) "" expected obtained

let tests_is_initial = "is_initial", [
  test_is_initial_0;
  test_is_initial_1
]

(*****************************************************************************)
(*                                  is_final                                 *)
(*****************************************************************************)

let test_is_final_0 () =
  let a1 = example_auto_1 () in
  let obtained = is_final a1 0 in
  let expected = false in
  Alcotest.(check bool) "" expected obtained

let test_is_final_1 () =
  let a1 = example_auto_1 () in
  let obtained = is_final a1 1 in
  let expected = true in
  Alcotest.(check bool) "" expected obtained

let tests_is_final = "is_final", [
  test_is_final_0;
  test_is_final_1
]

(*****************************************************************************)
(*                                has_epsilon                                *)
(*****************************************************************************)

let test_has_epsilon_0 () =
  let a1 = example_auto_1 () in
  let obtained = has_epsilon a1 in
  let expected = false in
  Alcotest.(check bool) "" expected obtained

let test_has_epsilon_1 () =
  let a2 = example_auto_2 () in
  let obtained = has_epsilon a2 in
  let expected = true in
  Alcotest.(check bool) "" expected obtained

let tests_has_epsilon = "has_epsilon", [
  test_has_epsilon_0;
  test_has_epsilon_1
]

(*****************************************************************************)
(*                                    is_det                                 *)
(*****************************************************************************)

let test_is_det_0 () =
  let a1 = example_auto_1 () in
  let obtained = is_det a1 in
  let expected = true in
  Alcotest.(check bool) "" expected obtained

let test_is_det_1 () =
  let a3 = example_auto_3 () in
  let obtained = is_det a3 in
  let expected = false in
  Alcotest.(check bool) "" expected obtained

let tests_is_det = "is_det", [
  test_is_det_0;
  test_is_det_1
]

(*****************************************************************************)
(*                                    trans                                  *)
(*****************************************************************************)

let test_trans_0 () =
  let a1 = example_auto_1 () in
  let obtained = trans a1 0 'a' in
  let expected = [1] in
  Alcotest.(check (list int)) "" expected obtained

let test_trans_1 () =
  let a3 = example_auto_3 () in
  let obtained = trans a3 0 'a' in
  let expected = [0 ; 1] in
  Alcotest.(check (list int)) "" expected obtained

let tests_trans = "trans", [
  test_trans_0;
  test_trans_1;
]

(*****************************************************************************)
(*                                  trans_eps                                *)
(*****************************************************************************)

let test_trans_eps_0 () =
  let a1 = example_auto_1 () in
  let obtained = trans_eps a1 0 in
  let expected = [] in
  Alcotest.(check (list int)) "" expected obtained

let test_trans_eps_1 () =
  let a2 = example_auto_2 () in
  let obtained = trans_eps a2 0 in
  let expected = [1] in
  Alcotest.(check (list int)) "" expected obtained

let tests_trans_eps = "trans_eps", [
  test_trans_eps_0;
  test_trans_eps_1;
]

(*****************************************************************************)
(*                                  trans_all                                *)
(*****************************************************************************)

let check_cts = Alcotest.(check (list (pair char (list int))))

let test_trans_all_0 () =
  let a = example_auto_1 () in
  let (obtained_cts, obtained_ets) = trans_all a 0 in
  let expected_cts = [('b', [0]) ; ('a'), [1]] in
  let expected_ets = [] in
  check_cts "" expected_cts obtained_cts;
  Alcotest.(check (list int)) "" expected_ets obtained_ets

let test_trans_all_1 () =
  let a = example_auto_2 () in
  let (obtained_cts, obtained_ets) = trans_all a 0 in
  let expected_cts = [('a', [0])] in
  let expected_ets = [1] in
  check_cts "" expected_cts obtained_cts;
  Alcotest.(check (list int)) "" expected_ets obtained_ets

let test_trans_all_2 () =
  let a = example_auto_3 () in
  let (obtained_cts, obtained_ets) = trans_all a 0 in
  let expected_cts = [('a', [0; 1])] in
  let expected_ets = [] in
  check_cts "" expected_cts obtained_cts;
  Alcotest.(check (list int)) "" expected_ets obtained_ets

let tests_trans_all = "trans_all", [
  test_trans_all_0;
  test_trans_all_1;
  test_trans_all_2
]

(*****************************************************************************)
(*                               trans_all_opt                               *)
(*****************************************************************************)

let check_ots = Alcotest.(check (list (pair (option char) int)))

let test_trans_all_opt_0 () =
  let a = example_auto_1 () in
  let obtained = trans_all_opt a 0 in
  let expected = [(Some 'b', 0) ; (Some 'a', 1)] in
  check_ots "" expected obtained

let test_trans_all_opt_1 () =
  let a = example_auto_2 () in
  let obtained = trans_all_opt a 0 in
  let expected = [(None, 1) ; (Some 'a', 0)] in
  check_ots "" expected obtained

let test_trans_all_opt_2 () =
  let a = example_auto_4 () in
  let obtained = trans_all_opt a 0 in
  let expected = [(Some 'b', 1); (Some 'a', 1); (Some 'a', 0)] in
  check_ots "" expected obtained

let tests_trans_all_opt = "trans_all_opt", [
  test_trans_all_opt_0;
  test_trans_all_opt_1;
  test_trans_all_opt_2
]

(*****************************************************************************)
(*                               remove_states                               *)
(*****************************************************************************)

let test_remove_states_0 () =
  let a1 = example_auto_5 () in
  let a1' = remove_states a1 [0; 2] in
  let a2 = create 1 in
  let obtained = eq a1' a2 in
  let expected = true in
  Alcotest.(check bool) "" expected obtained

let test_remove_states_1 () =
  let a1 = example_auto_5 () in
  let a1' = remove_states a1 [2; 3] in
  let a2 = create 1 in
  let obtained = eq a1' a2 in
  let expected = true in
  Alcotest.(check bool) "" expected obtained

let test_remove_states_2 () =
  let a1 = example_auto_5 () in
  let a1' = remove_states a1 [2] in
  let a2 = create 3 in
  set_initial a2 0;
  set_final a2 2;
  add_trans a2 0 'a' 0;
  add_trans a2 0 'a' 1;
  add_trans a2 0 'b' 1;
  add_trans a2 2 'a' 1;
  let obtained = eq a1' a2 in
  let expected = true in
  Alcotest.(check bool) "" expected obtained

let tests_remove_states = "remove_states", [
  test_remove_states_0;
  test_remove_states_1;
  test_remove_states_2
]

(*****************************************************************************)

let tests = [
  tests_eq;
  tests_size;
  tests_is_initial;
  tests_is_final;
  tests_has_epsilon;
  tests_is_det;
  tests_trans;
  tests_trans_eps;
  tests_trans_all;
  tests_trans_all_opt;
  tests_remove_states
]

let format_tests tests =
  List.map (fun (name, fs) ->
    (name, List.map (Alcotest.test_case "" `Quick) fs)
  ) tests

let () = Alcotest.run "Rational language - automatons" (format_tests tests) 