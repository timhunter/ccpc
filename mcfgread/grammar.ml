type component = Component of int*int | Epsilon
type rule = (string * string list * (component list list))
type t = rule list
