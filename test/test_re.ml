open Rational_language.Re
  
(*****************************************************************************)
(*                                string_of_re                               *)
(*****************************************************************************)

let test_string_of_re_0 () =
  let expected = "ε+∅" in
  let re1 = Alt (Epsilon, Empty) in
  let obtained = string_of_re re1 in
  Alcotest.(check string) "" expected obtained

let test_string_of_re_1 () =
  let expected = "abcd" in
  let re1 = Concat (Concat (Char 'a', Char 'b'), Concat (Char 'c', Char 'd')) in
  let obtained = string_of_re re1 in
  Alcotest.(check string) "" expected obtained
  
let test_string_of_re_2 () =
  let expected = "a+b+c+d" in
  let re1 = Alt (Alt (Char 'a', Char 'b'), Alt (Char 'c', Char 'd')) in
  let obtained = string_of_re re1 in
  Alcotest.(check string) "" expected obtained
  
let test_string_of_re_3 () =
  let expected = "a*(b+c)*" in
  let re1 = Concat (Star (Char 'a'), Star (Alt (Char 'b', Char 'c'))) in
  let obtained = string_of_re re1 in
  Alcotest.(check string) "" expected obtained

let test_string_of_re_4 () =
  let expected = "(a+b+c)d" in
  let re1 = Concat (Alt (Alt (Char 'a', Char 'b'), Char 'c'), Char 'd') in
  let obtained = string_of_re re1 in
  Alcotest.(check string) "" expected obtained

let test_string_of_re_5 () =
  let expected = "abc" in
  let re1 = Concat (Char 'a', Concat (Char 'b', Char 'c')) in
  let obtained = string_of_re re1 in
  Alcotest.(check string) "" expected obtained

let test_string_of_re_6 () =
  let expected = "abc+d" in
  let re1 = Alt (Concat (Concat (Char 'a', Char 'b'), Char 'c'), Char 'd') in
  let obtained = string_of_re re1 in
  Alcotest.(check string) "" expected obtained

let test_string_of_re_7 () =
  let expected = "a+bc" in
  let re1 = Alt (Char 'a', Concat (Char 'b', Char 'c')) in
  let obtained = string_of_re re1 in
  Alcotest.(check string) "" expected obtained

let test_string_of_re_8 () =
  let expected = "ε*+∅*" in
  let re1 = Alt (Star Epsilon, Star Empty) in
  let obtained = string_of_re re1 in
  Alcotest.(check string) "" expected obtained

let tests_string_of_re = "string_of_re", [
  test_string_of_re_0;
  test_string_of_re_1;
  test_string_of_re_2;
  test_string_of_re_3;
  test_string_of_re_4;
  test_string_of_re_5;
  test_string_of_re_6;
  test_string_of_re_7;
  test_string_of_re_8
]

(*****************************************************************************)
(*                                has_epsilon                                *)
(*****************************************************************************)

let test_has_epsilon_0 () =
  let expected = false in
  let re1 = Alt (Empty, Char 'a') in
  let obtained = has_epsilon re1 in
  Alcotest.(check bool) "" expected obtained

let test_has_epsilon_1 () =
  let expected = true in
  let re1 = Concat (Epsilon, Star (Char 'a')) in
  let obtained = has_epsilon re1 in
  Alcotest.(check bool) "" expected obtained

let test_has_epsilon_2 () =
  let expected = true in
  let re1 = Alt (Epsilon, Empty) in
  let obtained = has_epsilon re1 in
  Alcotest.(check bool) "" expected obtained

let tests_has_epsilon = "has_epsilon", [
  test_has_epsilon_0;
  test_has_epsilon_1;
  test_has_epsilon_2
]

(*****************************************************************************)
(*                                   first                                   *)
(*****************************************************************************)

let test_first_0 () =
  let expected = [] in
  let r1 = Alt (Empty, Epsilon) in
  let obtained = first r1 in
  Alcotest.(check (list char)) "" expected obtained

let test_first_1 () =
  let expected = ['a'] in
  let r1 = Char 'a' in
  let obtained = first r1 in
  Alcotest.(check (list char)) "" expected obtained

let test_first_2 () =
  let expected = ['a'] in
  let r1 = Concat (Char 'a', Char 'b') in
  let obtained = first r1 in
  Alcotest.(check (list char)) "" expected obtained

let test_first_3 () =
  let expected = ['a'; 'b'; 'c'] in
  let r1 = Alt (Char 'a', (Alt (Char 'b', Char 'c'))) in
  let obtained = first r1 in
  Alcotest.(check (list char)) "" expected obtained

let test_first_4 () =
  let expected = ['a'; 'b'] in
  let r1 = Concat (Star (Char 'b'), Char 'a') in
  let obtained = first r1 in
  Alcotest.(check (list char)) "" expected obtained

let test_first_5 () =
  let expected = ['a'; 'b'] in
  let r1 = Alt (Char 'a', (Alt (Char 'b', Char 'a'))) in
  let obtained = first r1 in
  Alcotest.(check (list char)) "" expected obtained

let tests_first = "first", [
  test_first_0;
  test_first_1;
  test_first_2;
  test_first_3;
  test_first_4;
  test_first_5
]

(*****************************************************************************)
(*                                   last                                    *)
(*****************************************************************************)

let test_last_0 () =
  let expected = [] in
  let r1 = Alt (Empty, Epsilon) in
  let obtained = last r1 in
  Alcotest.(check (list char)) "" expected obtained

let test_last_1 () =
  let expected = ['a'] in
  let r1 = Char 'a' in
  let obtained = last r1 in
  Alcotest.(check (list char)) "" expected obtained

let test_last_2 () =
  let expected = ['b'] in
  let r1 = Concat (Char 'a', Char 'b') in
  let obtained = last r1 in
  Alcotest.(check (list char)) "" expected obtained

let test_last_3 () =
  let expected = ['a'; 'b'; 'c'] in
  let r1 = Alt (Char 'a', (Alt (Char 'b', Char 'c'))) in
  let obtained = last r1 in
  Alcotest.(check (list char)) "" expected obtained

let test_last_4 () =
  let expected = ['a'; 'b'] in
  let r1 = Concat (Char 'a', Star (Char 'b')) in
  let obtained = last r1 in
  Alcotest.(check (list char)) "" expected obtained

let test_last_5 () =
  let expected = ['a'; 'b'] in
  let r1 = Alt (Char 'a', (Alt (Char 'b', Char 'a'))) in
  let obtained = last r1 in
  Alcotest.(check (list char)) "" expected obtained

let tests_last = "last", [
  test_last_0;
  test_last_1;
  test_last_2;
  test_last_3;
  test_last_4;
  test_last_5
]

(*****************************************************************************)
(*                                  follow                                   *)
(*****************************************************************************)

let test_follow_0 () =
  let expected = [] in
  let r1 = Alt (Empty, Epsilon) in
  let obtained = follow r1 'a' in
  Alcotest.(check (list char)) "" expected obtained

let test_follow_1 () =
  let expected = [] in
  let r1 = Char 'a' in
  let obtained = follow r1 'b' in
  Alcotest.(check (list char)) "" expected obtained

let test_follow_2 () =
  let expected = ['b'] in
  let r1 = Concat (Char 'a', Char 'b') in
  let obtained = follow r1 'a' in
  Alcotest.(check (list char)) "" expected obtained

let test_follow_3 () =
  let expected = ['b'; 'c'] in
  let r1 = Concat (Char 'a', (Alt (Char 'b', Char 'c'))) in
  let obtained = follow r1 'a' in
  Alcotest.(check (list char)) "" expected obtained

let test_follow_4 () =
  let expected = ['b'; 'c'] in
  let r1 = Concat (Char 'a', Concat (Star (Char 'b'), Char 'c')) in
  let obtained = follow r1 'a' in
  Alcotest.(check (list char)) "" expected obtained

let test_follow_5 () =
  let expected = [] in
  let r1 = Alt (Char 'a', (Alt (Char 'b', Char 'a'))) in
  let obtained = follow r1 'a' in
  Alcotest.(check (list char)) "" expected obtained

let test_follow_6 () =
  let expected = ['a'] in
  let r1 = Star (Char 'a') in
  let obtained = follow r1 'a' in
  Alcotest.(check (list char)) "" expected obtained

let tests_follow = "follow", [
  test_follow_0;
  test_follow_1;
  test_follow_2;
  test_follow_3;
  test_follow_4;
  test_follow_5;
  test_follow_6
]

(*****************************************************************************)
(*                                linearize                                  *)
(*****************************************************************************)

let test_linearize_0 () =
  let r1 = Alt (Empty, Epsilon) in
  let (n, _, r1') = linearize r1 in
  Alcotest.(check int) "" 0 n ;
  Alcotest.(check bool) "" true (r1 = r1')

let test_linearize_1 () =
  let r1 = Char 'a' in
  let expected = Char (Char.chr 1) in
  let (n, arr, obtained) = linearize r1 in
  Alcotest.(check int) "" 1 n ;
  Alcotest.(check char) "" 'a' arr.(1) ;
  Alcotest.(check bool) "" true (expected = obtained)

let test_linearize_2 () =
  let r1 = Concat (Alt (Char 'a', Char 'b'), Star (Char 'a')) in
  let expected = Concat (
    Alt (Char (Char.chr 1), Char (Char.chr 2)),
    Star (Char (Char.chr 3))
  ) in
  let (n, arr, obtained) = linearize r1 in
  print_string (string_of_re expected);
  print_string (string_of_re obtained);
  print_newline ();
  Alcotest.(check int) "" 3 n ;
  Alcotest.(check char) "" 'a' arr.(1) ;
  Alcotest.(check char) "" 'b' arr.(2) ;
  Alcotest.(check char) "" 'a' arr.(3) ;
  Alcotest.(check bool) "" true (expected = obtained)

let tests_linearize = "linearize", [
  test_linearize_0;
  test_linearize_1;
  test_linearize_2
]

(*****************************************************************************)

let tests = [
  tests_string_of_re;
  tests_has_epsilon;
  tests_first;
  tests_last;
  tests_follow;
  tests_linearize
]

let format_tests (tss: (string * (Alcotest.return -> Alcotest.return) list) list) =
  List.map (fun ts ->
    (fst ts, List.map (fun t -> Alcotest.test_case "" `Quick t) (snd ts))
  ) tss

let () = Alcotest.run "Rational language - regular expressions" (format_tests tests) 