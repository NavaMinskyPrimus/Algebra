open Core

let f x = x + 2

let%expect_test _ =
  print_endline "Hello world";
  [%expect {| Hello world |}]
;;
