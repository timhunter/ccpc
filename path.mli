type 'a choice = 'a list * 'a * 'a list
type 'a history

val singleton : 'a -> 'a history
val root : 'a history -> 'a
val last : 'a history -> 'a list * 'a * 'a list
val extend : 'a history -> Util.weight -> 'a choice -> 'a history
val to_list : 'a history -> 'a list
val try_ending_cycle : 'a history -> ('a history * 'a history) option
val weight_product : 'a history -> Util.weight
val latex_history_tikz : ('a -> string) -> 'a history -> string
val show_history : ('a -> string) -> ('a history) -> string
val rotate : 'a -> 'a history -> 'a history
val canonicalise : ('a -> 'a -> int) -> 'a history -> 'a history
