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

let tests = [
  tests_string_of_re;
  tests_has_epsilon
]

let format_tests (tss: (string * (Alcotest.return -> Alcotest.return) list) list) =
  List.map (fun ts ->
    (fst ts, List.map (fun t -> Alcotest.test_case "" `Quick t) (snd ts))
  ) tss

let () = Alcotest.run "Rational language - regular expressions" (format_tests tests) 