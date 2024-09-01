open! Core
open! GroupTypeHolder
open! Type_equal

module type Group = GroupTypes.Group

module type Subgroup = sig
  include Group

  val known_parents : (module Group with type element = element) list
  val is_element_specific : (element -> bool) option
end

(*takes a group and an element and returns the size of that element's orbit in that group. *)
let orbitSize (type e) (module G : Group with type element = e) (x : e) =
  let rec helper at count =
    if G.equals at G.identity
    then count + 1
    else helper (G.multiply x at) (count + 1)
  in
  helper x 0
;;

(*takes a group and an element and returns the element's orbit in that group. *)

let orbit (type e) (module G : Group with type element = e) (x : e) =
  let orbitS = orbitSize (module G) x in
  let rec helper at count =
    if count = orbitS then [] else at :: helper (G.multiply at x) (count + 1)
  in
  helper x 0
;;

(*This takes a group, a list of its elements and another element. it checks if that element is in the list*)
let element_list_contains
  (type e)
  (module G : Group with type element = e)
  l
  (x : e)
  =
  match List.find ~f:(fun y -> G.equals y x) l with
  | Some _e -> true
  | None -> false
;;

(*escape scope question for Aba*)

(*checks if second list is contained in first list*)
let all_contained (type e) (module H : Group with type element = e) list1 list2 =
  let rec helper l1 l2 counter =
    if counter = List.length l2
    then true
    else if element_list_contains (module H) l1 (List.nth_exn l2 counter)
    then helper l1 l2 (counter + 1)
    else false
  in
  helper list1 list2 0
;;

(*should be redone as sets, so repeats don't happen*)
(*takes a group and returns all it's elements, though the list is quite repetative*)
let long_walk (type e) (module H : Group with type element = e) =
  let generators = H.generators in
  let rec make_next_level l gCounter lCounter =
    if gCounter = List.length generators
    then
      if lCounter = List.length l then [] else make_next_level l 0 (lCounter + 1)
    else if lCounter = List.length l
    then []
    else (
      let generator = List.nth_exn generators gCounter in
      let thisL = List.nth_exn l lCounter in
      H.multiply thisL generator :: make_next_level l (gCounter + 1) lCounter)
  in
  let rec walking allUnder newLevel =
    let next_level = make_next_level newLevel 0 0 in
    if all_contained (module H) allUnder next_level
    then generators
    else next_level @ walking (allUnder @ next_level) next_level
  in
  walking generators generators
;;

(*checks if an element of a group is also contained in a subgroup*)
let is_element (type e) (module H : Group with type element = e) (x : e) =
  match H.is_element_specific with
  | Some f -> f x
  | None ->
    let elements_of_H = long_walk (module H) in
    element_list_contains (module H) elements_of_H x
;;

(*constructs a subgroup from a list of generators and it's parent group*)
let make_subgroup (type e) (module G : Group with type element = e) l =
  let module C = struct
    type element = G.element [@@deriving sexp]

    let generators = l
    let multiply x y = G.multiply x y
    let equals x y = G.equals x y
    let identity = G.identity
    let inverse x = G.inverse x
    let known_parents = [ (module G : Group with type element = e) ]
    let is_element_specific = None
    let groupID = GroupId.create()
  end
  in
  (module C : Group with type element = G.element)
;;

let add_parent
  (type e)
  (module G : Group with type element = e)
  (module H : Group with type element = e)
  =
  let module C = struct
    type element = H.element [@@deriving sexp]

    let generators = H.generators
    let multiply x y = H.multiply x y
    let equals x y = H.equals x y
    let identity = H.identity
    let inverse x = H.inverse x
    let groupID = GroupId.create()

    let known_parents =
      (module G : Group with type element = e) :: H.known_parents
    ;;

    let is_element_specific = H.is_element_specific
  end
  in
  (module C : Group with type element = G.element)
;;

(*takes a group and returns whether it is cyclic*)
let is_cyclic (module G : Group) = List.length G.generators = 1

(* Need help!!*)
let group_equals
  (module H : Group)
  (module G : Group)
  =
  GroupId.equal H.groupID G.groupID
;;

(*I would love this to be able to take two groups with different element
  types and compare the element types to rule out subgrouping, but I don't know how*)
(*Checks if H is a subgroup of G*)
let is_subgroup
  (type e)
  (module G : Group with type element = e)
  (module H : Group with type element = e)
  =
  let rec next_level (l : (module Group with type element = e) list) counter =
    if List.length l = counter
    then []
    else
      let module Holder = (val List.nth_exn l counter) in
      Holder.known_parents @ next_level l (counter + 1)
  in
  let rec list_tree (l : (module Group with type element = e) list) =
    if List.length l = 0
    then []
    else next_level l 0 @ list_tree (next_level l 0)
  in
  let superGroups =
    (module H : Group with type element = e) :: list_tree [ (module H) ]
  in
  let rec is_contained (l : (module Group with type element = e) list) n =
    if n = List.length l
    then false
    else if group_equals (module G) (module (val (List.nth_exn l n)))
    then true
    else is_contained l (n + 1)
  in
  is_contained superGroups 0
;;

let direct_product
  (type e1 e2)
  (module H : Group with type element = e1)
  (module G : Group with type element = e2)
  =
  let rec cross_by_one_left l x counter =
    if List.length l = counter
    then []
    else (List.nth_exn l counter, x) :: cross_by_one_left l x (counter + 1)
  in
  let rec cross_by_one_right l x counter =
    if List.length l = counter
    then []
    else (x, List.nth_exn l counter) :: cross_by_one_right l x (counter + 1)
  in
  let module C = struct
    type element = H.element * G.element [@@deriving sexp]

    let generators =
      cross_by_one_left H.generators G.identity 0
      @ cross_by_one_right G.generators H.identity 0
    ;;

    let multiply (a, b) (x, y) = H.multiply a x, G.multiply b y
    let equals (a, b) (x, y) = H.equals a x && G.equals b y
    let identity = H.identity, G.identity
    let inverse (a, b) = H.inverse a, G.inverse b
    let groupID = GroupId.create()
    let known_parents = []
    let is_element_specific = None
  end
  in
  (module C : Group with type element = e1 * e2)
;;

(* this is incomplete; I do not have an inverse function yet. *)
let semidirect_product
  (type e1 e2)
  (module N : Group with type element = e1)
  (module G : Group with type element = e2)
  (f : e2 -> e1 -> e1)
  =
  let rec cross_by_one_left l x counter =
    if List.length l = counter
    then []
    else (List.nth_exn l counter, x) :: cross_by_one_left l x (counter + 1)
  in
  let rec cross_by_one_right l x counter =
    if List.length l = counter
    then []
    else (x, List.nth_exn l counter) :: cross_by_one_right l x (counter + 1)
  in
  let module C = struct
    type element = N.element * G.element [@@deriving sexp]

    let generators =
      cross_by_one_left N.generators G.identity 0
      @ cross_by_one_right G.generators N.identity 0
    ;;

    let multiply (a, x) (b, y) = N.multiply a (f x b), G.multiply x y
    let equals (a, x) (b, y) = N.equals a b && G.equals x y
    let identity = N.identity, G.identity
    let inverse x = x
    let known_parents = []
    let is_element_specific = None
    let groupID = GroupId.create()
  end
  in
  (module C : Group with type element = e1 * e2)
;;

(*Checks if N is normal in G*)
let is_normal
  (type e)
  (module N : Group with type element = e)
  (module G : Group with type element = e)
  =
  (* checks if l's conjugates by g are in N *)
  let rec conjugate_contained (l : e list) (g : e) counter =
    if List.length l = counter
    then true
    else (
      let x =
        G.multiply (G.multiply g (List.nth_exn l counter)) (G.inverse g)
      in
      if is_element (module N) x
      then conjugate_contained l g (counter + 1)
      else false)
  in
  (*checks if the first list's conjugates by the second are in N*)
  let rec conjugates_contained (l1 : e list) (l2 : e list) counter =
    if counter = List.length l2
    then true
    else if conjugate_contained l1 (List.nth_exn l2 counter) 0
    then conjugates_contained l1 l2 (counter + 1)
    else false
  in
  if not (is_subgroup (module N) (module G))
  then false
  else conjugates_contained N.generators G.generators 0
;;
