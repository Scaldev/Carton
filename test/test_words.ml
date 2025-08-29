open Rational_language.Words
  
(*****************************************************************************)
(*                                occurrences                                *)
(*****************************************************************************)

let test_occurrences_0 () =
  let expected = 0 in
  let obtained = occurrences "aaaa" 'b' in
  Alcotest.(check int) "" expected obtained

let test_occurrences_1 () =
  let expected = 1 in
  let obtained = occurrences "aaba" 'b' in
  Alcotest.(check int) "" expected obtained
  
let test_occurrences_2 () =
  let expected = 2 in
  let obtained = occurrences "abcabc" 'b' in
  Alcotest.(check int) "" expected obtained

let tests_occurrences = "occurrences", [
  test_occurrences_0;
  test_occurrences_1;
  test_occurrences_2;
]

(*****************************************************************************)
(*                                  is_prefix                                *)
(*****************************************************************************)

let test_is_prefix_0 () =
  let expected = false in
  let obtained = is_prefix "abracadabra" "bra" in
  Alcotest.(check bool) "" expected obtained

let test_is_prefix_1 () =
  let expected = false in
  let obtained = is_prefix "abra" "abraca" in
  Alcotest.(check bool) "" expected obtained
  
let test_is_prefix_2 () =
  let expected = true in
  let obtained = is_prefix "abracadabra" "" in
  Alcotest.(check bool) "" expected obtained

let test_is_prefix_3 () =
  let expected = true in
  let obtained = is_prefix "abracadabra" "abra" in
  Alcotest.(check bool) "" expected obtained

let tests_is_prefix = "is_prefix", [
  test_is_prefix_0;
  test_is_prefix_1;
  test_is_prefix_2;
  test_is_prefix_3;
]

(*****************************************************************************)
(*                                  is_suffix                                *)
(*****************************************************************************)

let test_is_suffix_0 () =
  let expected = false in
  let obtained = is_suffix "abracadabra" "dra" in
  Alcotest.(check bool) "" expected obtained

let test_is_suffix_1 () =
  let expected = false in
  let obtained = is_suffix "abra" "dabra" in
  Alcotest.(check bool) "" expected obtained
  
let test_is_suffix_2 () =
  let expected = true in
  let obtained = is_suffix "abracadabra" "" in
  Alcotest.(check bool) "" expected obtained

let test_is_suffix_3 () =
  let expected = true in
  let obtained = is_suffix "abracadabra" "abra" in
  Alcotest.(check bool) "" expected obtained

let tests_is_suffix = "is_suffix", [
  test_is_suffix_0;
  test_is_suffix_1;
  test_is_suffix_2;
  test_is_suffix_3;
]

(*****************************************************************************)
(*                                  is_factor                                *)
(*****************************************************************************)

let test_is_factor_0 () =
  let expected = false in
  let obtained = is_factor "abracadabra" "blabla" in
  Alcotest.(check bool) "" expected obtained

let test_is_factor_1 () =
  let expected = false in
  let obtained = is_factor "abra" "dabra" in
  Alcotest.(check bool) "" expected obtained
  
let test_is_factor_2 () =
  let expected = true in
  let obtained = is_factor "abracadabra" "" in
  Alcotest.(check bool) "" expected obtained

let test_is_factor_3 () =
  let expected = true in
  let obtained = is_factor "abracadabra" "cada" in
  Alcotest.(check bool) "" expected obtained

let tests_is_factor = "is_factor", [
  test_is_factor_0;
  test_is_factor_1;
  test_is_factor_2;
  test_is_factor_3;
]

(*****************************************************************************)
(*                             is_period_of_word                             *)
(*****************************************************************************)

let test_is_period_of_word_0 () =
  let expected = true in
  let obtained = is_period_of_word "abababab" 4 in
  Alcotest.(check bool) "" expected obtained

let test_is_period_of_word_1 () =
  let expected = false in
  let obtained = is_period_of_word "abababab" 3 in
  Alcotest.(check bool) "" expected obtained
  
let tests_is_period_of_word = "is_period_of_word", [
  test_is_period_of_word_0;
  test_is_period_of_word_1;
]

(*****************************************************************************)
(*                                 periods_of                                *)
(*****************************************************************************)

let test_periods_of_0 () =
  let expected = [6] in
  let obtained = periods_of "abcdef" in
  Alcotest.(check Alcotest.(list int)) "" expected obtained

let test_periods_of_1 () =
  let expected = [2; 4; 6; 8] in
  let obtained = periods_of "abababab" in
  Alcotest.(check Alcotest.(list int)) "" expected obtained
  
let tests_periods_of = "periods_of", [
  test_periods_of_0;
  test_periods_of_1;
]

(*****************************************************************************)
(*                                is_primitive                               *)
(*****************************************************************************)

let test_is_primitive_0 () =
  let expected = true in
  let obtained = is_primitive "abcdef" in
  Alcotest.(check bool) "" expected obtained

let test_is_primitive_1 () =
  let expected = false in
  let obtained = is_primitive "abababab" in
  Alcotest.(check bool) "" expected obtained
  
let tests_is_primitive = "is_primitive", [
  test_is_primitive_0;
  test_is_primitive_1;
]

(*****************************************************************************)
(*                                 factorize                                 *)
(*****************************************************************************)

let test_factorize_0 () =
  let (expected_u, expected_v, expected_k) = ("", "abcd", 1) in
  let (obtained_u, obtained_v, obtained_k) = factorize "abcd" 4 in
  Alcotest.(check string) "" expected_u obtained_u;
  Alcotest.(check string) "" expected_v obtained_v;
  Alcotest.(check int   ) "" expected_k obtained_k

let test_factorize_1 () =
  let (expected_u, expected_v, expected_k) = ("ab", "cd", 3) in
  let (obtained_u, obtained_v, obtained_k) = factorize "abcdabcdabcdab" 4 in
  Alcotest.(check string) "" expected_u obtained_u;
  Alcotest.(check string) "" expected_v obtained_v;
  Alcotest.(check int   ) "" expected_k obtained_k
  
let test_factorize_2 () =
  let (expected_u, expected_v, expected_k) = ("", "ab", 3) in
  let (obtained_u, obtained_v, obtained_k) = factorize "ababab" 2 in
  Alcotest.(check string) "" expected_u obtained_u;
  Alcotest.(check string) "" expected_v obtained_v;
  Alcotest.(check int   ) "" expected_k obtained_k
  
    
let tests_factorize = "factorize", [
  test_factorize_0;
  test_factorize_1;
  test_factorize_2;
]

(*****************************************************************************)
(*                               guibas_odlyzko                              *)
(*****************************************************************************)

let test_guibas_odlyzko_0 () =
  let expected = "00000" in
  let obtained = guibas_odlyzko "aaaaa" in
  Alcotest.(check string) "" expected obtained

let test_guibas_odlyzko_1 () =
  let expected = "01101" in
  let obtained = guibas_odlyzko "abcab" in
  Alcotest.(check string) "" expected obtained

let test_guibas_odlyzko_2 () =
  let expected = "01101101101" in
  let obtained = guibas_odlyzko "abcabcabcab" in
  Alcotest.(check string) "" expected obtained

let test_guibas_odlyzko_3 () =
  let expected = "01101" in
  let obtained = guibas_odlyzko "abbab" in
  Alcotest.(check string) "" expected obtained
  
let tests_guibas_odlyzko = "guibas_odlyzko", [
  test_guibas_odlyzko_0;
  test_guibas_odlyzko_1;
  test_guibas_odlyzko_2;
  test_guibas_odlyzko_3;
]

(*****************************************************************************)
(*                                    app                                    *)
(*****************************************************************************)

let test_app_0 () =
  let psi = Hashtbl.create 1 in
  let w = "0" in
    Alcotest.check_raises ""
    (Invalid_word "psi(0) is undefined.")
    (fun () -> ignore (app psi w))

let test_app_1 () =
  let psi = Hashtbl.of_seq (List.to_seq
    [('0', "a") ; ('1', "bb")]
  ) in
  let expected = "abbaabb" in
  let obtained = app psi "01001" in
  Alcotest.(check string) "" expected obtained

let tests_app = "app", [
  test_app_0;
  test_app_1
]

(*****************************************************************************)
(*                              is_endomorphism                              *)
(*****************************************************************************)


let test_is_endomorphism_0 () =
  let psi = Hashtbl.create 0 in
  let expected = true in
  let obtained = is_endomorphism psi in
  Alcotest.(check bool) "" expected obtained

let test_is_endomorphism_1 () =
  let psi = morphism_of_list [('0',"01") ; ('1',"221") ; ('2',"2")] in
  let expected = true in
  let obtained = is_endomorphism psi in
  Alcotest.(check bool) "" expected obtained
  
let test_is_endomorphism_2 () =
  let psi = morphism_of_list [('0',"01") ; ('1',"231") ; ('2',"2")] in
  let expected = false in
  let obtained = is_endomorphism psi in
  Alcotest.(check bool) "" expected obtained

let tests_is_endomorphism = "is_endomorphism", [
  test_is_endomorphism_0;
  test_is_endomorphism_1;
  test_is_endomorphism_2
]

(*****************************************************************************)
(*                                  iter_app                                 *)
(*****************************************************************************)

let test_iter_app_0 () =
  (* a_0 = 0, a_i = 1 if i is a non-null square, = 2 otherwise *)
  let psi = morphism_of_list [('0',"01") ; ('1',"221") ; ('2',"2")] in
  let expected = "0122122221222222122222222122222222221" in
  let obtained = iter_app psi "0" 6 in
  Alcotest.(check string) "" expected obtained

let test_iter_app_1 () =
  (* characteristic function of squares: a_i = 1 iff i is a square *)
  let psi = morphism_of_list [('0',"01") ; ('1',"221") ; ('2',"2")] in
  let mu = morphism_of_list [('0',"1") ; ('1',"1") ; ('2',"0")] in
  let expected = "1100100001000000100000000100000000001" in
  let obtained = app mu (iter_app psi "0" 6) in
  Alcotest.(check string) "" expected obtained

let test_iter_app_2 () =
  let psi = morphism_of_list [('0', "01") ; ('1', "0")] in
  let expected = "010010100100101001010" in
  let obtained = iter_app psi "0" 6 in
  Alcotest.(check string) "" expected obtained


let tests_iter_app = "iter_app", [
  test_iter_app_0;
  test_iter_app_1;
  test_iter_app_2
]

(*****************************************************************************)

let tests = [

  tests_occurrences;
  tests_is_prefix;
  tests_is_suffix;
  tests_is_factor;

  tests_is_period_of_word;
  tests_periods_of;
  tests_is_primitive;
  tests_factorize;
  tests_guibas_odlyzko;

  tests_app;
  tests_is_endomorphism;
  tests_iter_app
]

let format_tests (tss: (string * (Alcotest.return -> Alcotest.return) list) list) =
  List.map (fun ts ->
    (fst ts, List.map (fun t -> Alcotest.test_case "" `Quick t) (snd ts))
  ) tss

let () = Alcotest.run "Rational language - words" (format_tests tests) 