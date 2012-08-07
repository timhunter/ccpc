type derivation_tree

(* Represents a hypergraph as a function which maps a vertex/item to a list 
 * of its tails (i.e. edges that lead to it). Besides a list of items 
 * (i.e. source vertices), each edge also has a rule and a weight. *)
type hgraph = Chart.item -> (Chart.item list * Rule.r * Util.weight) list

val get_derivations : Chart.chart -> Chart.item -> derivation_tree list
val get_root_item : derivation_tree -> Chart.item
val get_children : derivation_tree -> derivation_tree list
val get_weight : derivation_tree -> Util.weight
val get_rule : derivation_tree -> Rule.r
val get_n_best : int -> hgraph -> Chart.item -> derivation_tree list

val print_tree : derivation_tree -> string
val print_tree_compact : derivation_tree -> string
