open! Core
(*is there a way to remove type element = element? Would be useful not to need it
  everywhere, but i'm not sure if that breaks compatibility*)
(*I would also like to hide known parents, if that's possible; I don't want users
  to be able to chose parents recklessly, i want to have rules around it (same element type)*)
module GroupId = Unique_id.Int()

module rec GroupTypes : sig
  module type Group = sig
    type element [@@deriving sexp]

    val generators : element list
    val multiply : element -> element -> element
    val equals : element -> element -> bool
    val identity : element
    val inverse : element -> element
    val groupID : GroupId.t

    val known_parents
      : (module GroupTypes.Group with type element = element) list

    val is_element_specific : (element -> bool) option
  end
end = GroupTypes



