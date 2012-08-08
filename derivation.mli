type 'a derivation_tree

val get_derivations : Chart.chart -> Chart.item -> (Chart.item derivation_tree) list
val get_root_item : 'a derivation_tree -> 'a
val get_children : 'a derivation_tree -> 'a derivation_tree list
val get_weight : 'a derivation_tree -> Util.weight
val get_rule : 'a derivation_tree -> Rule.r
val get_n_best : int -> Chart.chart -> Chart.item -> (Chart.item derivation_tree) list

val print_tree : ('a -> string) -> 'a derivation_tree -> string
val print_tree_compact : 'a derivation_tree -> string
