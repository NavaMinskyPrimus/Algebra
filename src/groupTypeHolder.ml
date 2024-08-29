open! Core

module rec GroupTypes : sig
  module type Group = sig
    type element [@@deriving sexp]

    val generators : element list
    val multiply : element -> element -> element
    val equals : element -> element -> bool
    val identity : element

    val known_parents
      : (module GroupTypes.Group with type element = element) list

    val is_element_specific : (element -> bool) option
  end
end =
  GroupTypes
