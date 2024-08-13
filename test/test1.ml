open Core

(* Just a very basic file to show how to write tests *)
let f x =
  let y = x + 2 in
  y * 3
;;

(* this is a fancy testing thing. An "expect test" gives you a way of
   printing out data from your program, and capturing it inside that
   same file. *)

let t = Project.FirstTry.CyclicGroup.makeGroup 8
let x2 = Project.FirstTry.CyclicGroup.makeElement t 2
let l = Project.FirstTry.CyclicGroup.orbit t x2

let%expect_test _ =
  Project.FirstTry.CyclicGroup.printElementList l;
  [%expect {| 0, 2, 4, 6 |}]
;;
