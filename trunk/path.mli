type 'a history

val singleton : 'a -> 'a history
val root : 'a history -> 'a
val last : 'a history -> 'a list * 'a * 'a list
val extend : 'a history -> Util.weight -> ('a list * 'a * 'a list) -> 'a history
val to_list : 'a history -> 'a list
val try_ending_cycle : 'a history -> ('a history * 'a history) option
val weight_product : 'a history -> Util.weight
val latex_history_tikz : ('a -> string) -> 'a history -> string
val show_history : ('a -> string) -> ('a history) -> string
val show_history_full : ('a -> string) -> ('a history) -> string
val rotate : 'a -> 'a history -> 'a history
val canonicalise : ('a -> 'a -> int) -> 'a history -> 'a history

(* These functions are for working with paths and cycles through derivation trees, in 
 * contrast to the stuff in cycles.ml which concerns paths and cycles in grammars. 
 * (Perhaps it would be good to unify the two using a hypergraph type at some point.) *)
val get_cycles : 'a Derivation.derivation_tree -> ('a history * 'a history) list
val get_path : (string -> bool) -> string Derivation.derivation_tree -> string history
