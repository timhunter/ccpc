(* Define a disjoint type for components of a string yield,
 * either a Component or Epsilon *)
type component = Component of int*int | Epsilon
type t = string * string list * (component list list)
val get_cat : t -> string
val get_children : t -> string list
val get_rewrite : t -> component list list
