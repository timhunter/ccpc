type derivation_tree

val get_derivations : Chart.chart -> Chart.item -> derivation_tree list
val get_root_item : derivation_tree -> Chart.item
val get_children : derivation_tree -> derivation_tree list
val get_weight : derivation_tree -> Util.weight
val get_rule : derivation_tree -> Rule.r
val get_n_best : int -> Chart.chart -> Chart.item -> derivation_tree list

val print_tree : derivation_tree -> string
val print_tree_compact : derivation_tree -> string
