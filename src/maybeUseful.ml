(*let cyclicGroup size =
  let module C = struct
    type t = int
    type element = int

    let generators = [ 1 ]
    let id = 0
    let orbitSize e = size / Helpers.gcd size e

    let orbit e =
      let rec orbith a r =
        if r = orbitSize a then [] else (a * r % size) :: orbith a (r + 1)
      in
      orbith e 0
    ;;

    let multiply x y = (x + y) % size
  end
  in
  (module C : Group with type element = int and type t = int)
;;

module DihedralElement = struct
  type t =
    { r : int
    ; s : int
    }
end

let dihedralGroup size =
  let module C = struct
    type t = int

    type element = DihedralElement.t =
      { r : int
      ; s : int
      }

    let generators : element list = [ { r = 1; s = 0 }; { r = 0; s = 1 } ]
    let id : element = { r = 0; s = 0 }

    let orbitSize e =
      if e.s = 0
      then
        let module Cyclic =
          (val cyclicGroup size
            : Group with type element = int and type t = int)
        in
        Cyclic.orbitSize e.r
      else 2
    ;;

    let orbit e =
      let rec orbith a c =
        if c = orbitSize a
        then []
        else { s = 0; r = a.r * c % size } :: orbith a (c + 1)
      in
      orbith e 0
    ;;

    let multiply a b = { r = a.r + (b.r % size); s = a.s + (b.s % 2) }
  end
  in
  (module C : Group with type t = int and type element = DihedralElement.t)
;;*)
