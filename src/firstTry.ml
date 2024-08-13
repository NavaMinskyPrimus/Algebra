open Core

module type Group = sig
  type t
  type element
  type subgroup

  val makeGroup : int -> t
  val makeElement : t -> int -> element
  val makeSubgroup : t -> element list -> subgroup
  val orbitSize : t -> element -> int
  val orbit : t -> element -> element list
  val takeQuotient : t -> subgroup -> t
  val multiply : element -> element -> element
  val printElementList : element list -> unit
end

module CyclicGroup : Group = struct
  type t = { size : int }

  type element =
    { group : t
    ; exponent : int
    }

  type subgroup =
    { _group : t
    ; generator : element
    }

  let makeGroup size = { size }
  let makeElement group a = { group; exponent = a % group.size }

  let rec elementToNumberList l =
    match l with
    | [] -> []
    | [ e ] -> [ e.exponent ]
    | first :: rest -> first.exponent :: elementToNumberList rest
  ;;

  let makeSubgroup g l =
    { _group = g
    ; generator = makeElement g (Helpers.gcdList (elementToNumberList l))
    }
  ;;

  let rec printElementList l =
    match l with
    | [] -> ()
    | [ a ] -> printf "%d" a.exponent
    | first :: rest ->
      printf "%d, " first.exponent;
      printElementList rest
  ;;

  let orbitSize t e = t.size / Helpers.gcd t.size e.exponent

  let orbit t a =
    let rec orbith t a r =
      if r = orbitSize t a
      then []
      else
        { group = t; exponent = a.exponent * r % t.size } :: orbith t a (r + 1)
    in
    orbith t a 0
  ;;

  let takeQuotient g n = { size = g.size / orbitSize g n.generator }

  let multiply a b =
    { group = a.group; exponent = a.exponent + (b.exponent % a.group.size) }
  ;;
end

module DihedralGroup = struct
  type t = { points : int }

  type element =
    { group : t
    ; r : int
    ; s : int
    }

  let makeGroup x = { points = x }
  let makeElement x a b = { group = x; r = a % x.points; s = b % 2 }

  let multiply t a b =
    { group = t; r = a.r + (b.r % t.points); s = a.s + (b.s % 2) }
  ;;

  let orbitSize t e =
    if e.s = 0
    then (
      let g = CyclicGroup.makeGroup t.points in
      CyclicGroup.orbitSize g (CyclicGroup.makeElement g e.r))
    else 2
  ;;

  let orbit t e =
    let rec orbith t a n =
      if a.s = 0
      then
        if n = orbitSize t a
        then []
        else { group = t; r = a.r * n % t.points; s = 0 } :: orbith t a (n + 1)
      else [ { group = t; r = 0; s = 0 }; a ]
    in
    orbith t e 0
  ;;
end
