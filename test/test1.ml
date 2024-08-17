open Core
open Project

(* Just a very basic file to show how to write tests *)
let f x =
  let y = x + 2 in
  y * 3
;;

(* this is a fancy testing thing. An "expect test" gives you a way of
   printing out data from your program, and capturing it inside that
   same file. *)

module Example : SecondTry.Group with type element = int = struct
  type element = int [@@deriving sexp]

  let generators = [ 1 ]
  let multiply x y = (x + y) % 5
  let equals x y = x = y
  let identity = 0
end

let%expect_test _ =
  print_s [%sexp (SecondTry.orbit (module Example) 2 : Example.element list)];
  [%expect {| (2 4 1 3 0) |}]
;;
