type derivation_tree

val get_derivations : Chart.chart -> Chart.item -> derivation_tree list
val get_root_item : derivation_tree -> Chart.item
val get_children : derivation_tree -> derivation_tree list
val get_weight : derivation_tree -> Util.weight
val get_rule : derivation_tree -> Rule.r
val get_nth_best_derivation : int -> Chart.chart -> (Chart.item * int) list -> Chart.item -> derivation_tree option

val print_tree : derivation_tree -> string
