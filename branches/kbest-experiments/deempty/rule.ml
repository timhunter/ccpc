(* Define a disjoint type for components of a string yield,
 * either a Component or Epsilon *)
type component = Component of int*int | Epsilon
type t = string * string list * (component list list)

(* a rule is a terminal rule if it is either an empty rule or a terminating string *)
let is_terminal (_,ch,_) =
  ((List.length ch)=1 && (String.length (List.nth ch 0) = 0 || (List.nth ch 0).[0]='\"')) || (List.length ch)=0
