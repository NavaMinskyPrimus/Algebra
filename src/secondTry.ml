open! Core

module type Group = sig
  type element [@@deriving sexp]

  val generators : element list
  val multiply : element -> element -> element
  val equals : element -> element -> bool
  val identity : element
end

module type Subgroup = sig
  include Group

  val knownParents : (module Group with type element = element) list
end

let orbitSize (type e) (module G : Group with type element = e) (x : e) =
  let rec helper at count =
    if G.equals at G.identity
    then count + 1
    else helper (G.multiply x at) (count + 1)
  in
  helper x 0
;;

let orbit (type e) (module G : Group with type element = e) (x : e) =
  let orbitS = orbitSize (module G) x in
  let rec helper at count =
    if count = orbitS then [] else at :: helper (G.multiply at x) (count + 1)
  in
  helper x 0
;;
