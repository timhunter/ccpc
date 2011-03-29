(* Define a disjoint type for components of a string yield,
 * either a Component or Epsilon *)
type component = Component of int*int | Epsilon

type t = string * string list * (component list list)
let make_rule (cat, children, rewrite) = (cat, children, rewrite)
let get_cat (cat, children, rewrite) = cat
let get_children (cat, children, rewrite) = children
let get_rewrite (cat, children, rewrite) = rewrite
