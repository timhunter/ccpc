type derivation_tree

val get_derivations : Chart.chart -> Chart.item -> derivation_tree list
val get_root_item : derivation_tree -> Chart.item
val get_children : derivation_tree -> derivation_tree list
val get_weight : derivation_tree -> Util.weight
