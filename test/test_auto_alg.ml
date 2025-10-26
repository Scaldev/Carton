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
  Automaton that recognizes (a|b)*ba

      /a\      /b\
      \ v      \ v
   -> [0] -b-> [1] -a-> [|2|]
        ^        ^--b--/  |
         \------a--------/
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

(*****************************************************************************)
(*                                accept_dfa                                 *)
(*****************************************************************************)

let test_accept_dfa_0 () =
  let a = example_auto_4 () in
  let expected = false in
  let obtained = accept_dfa a (String.init 10_000 (fun i -> if i mod 2 = 0 then 'a' else 'b')) in
  Alcotest.(check bool) "" expected obtained

let test_accept_dfa_1 () =
  let a = example_auto_4 () in
  let expected = true in
  let obtained = accept_nfa a (String.init 10_000 (fun i -> if i mod 2 = 0 then 'b' else 'a')) in
  Alcotest.(check bool) "" expected obtained

let tests_accept_dfa = "accept_dfa", [
  test_accept_dfa_0;
  test_accept_dfa_1
]


(*****************************************************************************)
(*                                accept_nfa                                 *)
(*****************************************************************************)

let test_accept_nfa_0 () =
  let a = example_auto_5 () in
  let expected = false in
  let obtained = accept_nfa a (String.make 10_000 'a') in
  Alcotest.(check bool) "" expected obtained

let test_accept_nfa_1 () =
  let a = example_auto_5 () in
  let expected = true in
  let obtained = accept_nfa a (String.make 10_000 'a' ^ "b") in
  Alcotest.(check bool) "" expected obtained

let tests_accept_nfa = "accept_nfa", [
  test_accept_nfa_0;
  test_accept_nfa_1
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
  print (Format.std_formatter) (example_auto_3 ()) ;
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
  print (Format.std_formatter) a ;
  print (Format.std_formatter) a' ;
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
  tests_is_empty;
  tests_clean;
  tests_remove_eps_trans;
  tests_accept_dfa;
  tests_accept_nfa;
  tests_thompson;
  tests_berry_sethi
]

let format_tests tests =
  List.map (fun (name, fs) ->
    (name, List.map (Alcotest.test_case "" `Quick) fs)
  ) tests

let () = Alcotest.run "Rational language - automata algorithms" (format_tests tests)