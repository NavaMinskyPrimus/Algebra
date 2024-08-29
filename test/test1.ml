open Core
open Project
(* this is a fancy testing thing. An "expect test" gives you a way of
   printing out data from your program, and capturing it inside that
   same file. *)

module Example : GroupTypeHolder.GroupTypes.Group with type element = int =
struct
  type element = int [@@deriving sexp]

  let generators = [ 1 ]
  let multiply x y = (x + y) % 5
  let equals x y = x = y
  let identity = 0
  let known_parents = []
  let is_element_specific = None
end

let l = [ 0, 1; 1, 0 ]
let%expect_test _ = printf "hi"
