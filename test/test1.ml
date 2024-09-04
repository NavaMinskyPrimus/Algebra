open Core
open Project
open GroupTypeHolder
open GroupPlay

module type Group = GroupTypes.Group

(* this is a fancy testing thing. An "expect test" gives you a way of
   printing out data from your program, and capturing it inside that
   same file. *)

module CyclicExample :
  GroupTypeHolder.GroupTypes.Group with type element = int = struct
  type element = int [@@deriving sexp]

  let generators = [ 1 ]
  let multiply x y = (x + y) % 20
  let equals x y = x = y
  let identity = 0
  let known_parents = []
  let inverse x = 20 - x
  let is_element_specific = None
  let groupID = GroupId.create ()
end

module DihedralExample : Group with type element = int * int = struct
  type element = int * int [@@deriving sexp]

  let generators = [ 1, 0; 0, 1 ]
  let multiply (a, b) (x, y) = (a + x) % 8, (b + y) % 2
  let equals (a, b) (x, y) = a = x && b = y
  let identity = 0, 0
  let known_parents = []
  let inverse (x, y) = 8 - x, 2 - y
  let is_element_specific = None
  let groupID = GroupId.create ()
end

let%expect_test _ =
  let module Holder =
    (val make_subgroup
           (module DihedralExample : Group with type element = int * int)
           [ 0, 1 ])
  in
  let sexp_of_list =
    [%sexp_of: (int * int) list] (orbit (module Holder) (0, 1))
  in
  print_endline (Sexplib.Sexp.to_string_hum sexp_of_list);
  printf "%b" (is_normal (module DihedralExample) (module Holder))
;;
