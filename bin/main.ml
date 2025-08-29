open Rational_language.Auto

let setup_auto () : auto =
  let auto = create 2 in
  set_initial auto 0;
  set_final auto 1;
  add_trans auto 0 'a' 1;
  add_trans auto 0 'b' 0;
  add_trans auto 1 'a' 0;
  add_trans auto 1 'b' 1;
  auto

let () =
  print Format.std_formatter (setup_auto ());
  print_dot_to_file "resources/dot/auto_test.dot" (setup_auto ())