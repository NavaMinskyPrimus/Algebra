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

  val known_parents : (module Group with type element = element) list
  val is_element_specific : (element -> bool) option
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

let is_subgroup
  (type e)
  (module G : Group with type element = e)
  (module H : Subgroup with type element = e)
  =
  true
;;

let list_contains (type e) (module G : Group with type element = e) l (x : e) =
  match List.find l ~f:(fun y -> G.equals y x) with
  | Some _e -> true
  | None -> false
;;

let long_walk (type e) (module H : Subgroup with type element = e) =
  let generators = H.generators in
  let contains l (x : e) =
    match List.find l ~f:(fun y -> H.equals y x) with
    | Some _e -> true
    | None -> false
  in
  let rec allContained l1 l2 counter =
    if counter = List.length l2
    then true
    else if contains l1 (Helpers.getNth l2 counter)
    then allContained l1 l2 (counter + 1)
    else false
  in
  let rec make_next_level l gCounter lCounter =
    if gCounter = List.length generators
    then
      if lCounter = List.length l then [] else make_next_level l 0 (lCounter + 1)
    else if lCounter = List.length l
    then []
    else (
      let generator = Helpers.getNth generators gCounter in
      let thisL = Helpers.getNth l lCounter in
      H.multiply thisL generator :: make_next_level l (gCounter + 1) lCounter)
  in
  let rec walking allUnder newLevel =
    let next_level = make_next_level newLevel 0 0 in
    if allContained allUnder next_level 0
    then generators
    else next_level @ walking (allUnder @ next_level) next_level
  in
  walking generators generators
;;

let is_element (type e) (module H : Subgroup with type element = e) (x : e) =
  match H.is_element_specific with
  | Some f -> f x
  | None ->
    let elements_of_H = long_walk (module H) in
    list_contains (module H) elements_of_H x
;;

(*let is_normal
  (type e)
  (module G : Group with type element = e)
  (module H : Subgroup with type element = e)
  =
  let rec helper counter = if true then 5 else 6 in
  if is_subgroup (module G) (module H)
  then 1
  else raise (Failure "H not a subgroup of G")
  ;;*)
