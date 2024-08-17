(*open! Core

module Cyclic_group = struct
  type t =
    { order : int
    ; parent : t list
    }

  type element =
    { exponent : int
    ; group : t
    }
end

module Dihedral_group = struct
  type t = { points : int }
end

module Symmetric_group = struct
  type t = { points : int }
end

module Alternating_group = struct
  type t = { points : int }
end

val Generated_subgroup t (t.element list) = 
struct
  type element = int
  type t =
    { generators : element list
    ; multiplication : element -> element -> element
    }
end

module Base_group = struct
  type t =
    | Cyclic of Cyclic_group.t
    | Dihedral of Dihedral_group.t
    | Symmetric of Symmetric_group.t
    | Alternating of Alternating_group.t
end

module Group = struct
  type t =
    | Base_group of Base_group.t
    | Cross_product of t * t
    | Quotient of
        { group : t
        ; subgroup : t
        }
end
*)
