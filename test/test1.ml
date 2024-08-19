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

module SubExample : SecondTry.Subgroup with type element = int * int = struct
  type element = int * int [@@deriving sexp]

  let generators = [ 0, 1; 1, 0 ]
  let multiply (a, b) (c, d) = (a + c) % 2, (b + d) % 4
  let equals (a, b) (c, d) = a = c && b = d
  let identity = 0, 0
  let known_parents = []
  let is_element_specific = None
end

module SubExample : SecondTry.Subgroup with type element = int * int = struct
  type element = int * int [@@deriving sexp]

  let generators = [ 0, 1; 1, 0 ]
  let multiply (a, b) (c, d) = (a + c) % 2, (b + d) % 4
  let equals (a, b) (c, d) = a = c && b = d
  let identity = 0, 0
  let known_parents = []
  let is_element_specific = None
end

let l = [ 0, 1; 1, 0 ]

let%expect_test _ =
  printf "%b" (SecondTry.is_element (module SubExample) (2, 2))
;;
