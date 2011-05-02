type item 
type input= Prefix of (string list) | Sentence of (string list)
type prim
type backpointer =  item option * item option

val deduce: int -> Rule.r list -> input -> item list
val is_goal: input -> item -> bool 
val get_axioms: Rule.r list -> input -> item list 
val create_item: string -> (Util.range_item * Util.range_item) list -> backpointer option -> item 
val get_nonterm: item -> string
val get_ranges: item -> (Util.range_item * Util.range_item) list 
val get_backpointer: item -> backpointer option
val to_string: item -> string
