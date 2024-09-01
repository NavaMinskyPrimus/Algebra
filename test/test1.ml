open Core
open Project
open GroupTypeHolder
(* this is a fancy testing thing. An "expect test" gives you a way of
   printing out data from your program, and capturing it inside that
   same file. *)

module Example : GroupTypeHolder.GroupTypes.Group with type element = int =
struct
  type element = int [@@deriving sexp]

  let generators = [ 1 ]
  let multiply x y = (x + y) % 10
  let equals x y = x = y
  let identity = 0
  let known_parents = []
  let inverse x = 10 - x
  let is_element_specific = None
  let groupID = GroupId.create()
end

module Example2 : GroupTypeHolder.GroupTypes.Group with type element = int =
struct
  type element = int [@@deriving sexp]

  let generators = [ 1 ]
  let multiply x y = (x + y) % 5
  let equals x y = x = y
  let identity = 0
  let known_parents = []
  let inverse x = 5 - x
  let is_element_specific = None
  let groupID = GroupId.create()
end

let%expect_test _ =
  printf "%b" (GroupPlay.is_subgroup (module Example) (module Example2))
;;
