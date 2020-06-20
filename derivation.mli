
(** Derivation trees for multiple context-free grammars. *)

(** A type for derivation trees, parametrized by the type of element that appears at each node 
    (internal nodes and leaf nodes). This type parameter is typically either [string], in which 
    case its values represent nonterminal symbols, or [Chart.item]; recall that a [Chart.item] is 
    effectively just a nonterminal symbol paired with a list of ranges. *)
type 'a derivation_tree = Leaf of 'a * Rule.r * Util.weight | NonLeaf of 'a * ('a derivation_tree) list * Rule.r * Util.weight
(* type 'a derivation_tree *)

val get_root_item : 'a derivation_tree -> 'a
val get_children : 'a derivation_tree -> 'a derivation_tree list
val get_weight : 'a derivation_tree -> Util.weight

(** The rule that licenses the "last"/"root" step of the derivation tree. Notice that while the [get_weight] function 
    returns the weight of the entire derivation, the weight introduced by this last step can be retrieved from the 
    result of [get_rule]. *)
val get_rule : 'a derivation_tree -> Rule.r

(** [make_derivation_tree root children r] constructs a new derivation of [root] by combining the derivation trees 
    [children] according to the rule [r]. *)
val make_derivation_tree : 'a -> ('a derivation_tree) list -> Rule.r -> 'a derivation_tree

(** Returns the string derived by a derivation. Fails if the provided derivation derives an MCFG nonterminal of rank greater than one. *)
val derived_string : 'a derivation_tree -> string

(** Retrieves all derivations of the given item from the chart. *)
val get_derivations : Chart.chart -> Chart.item -> (Chart.item derivation_tree) list

(** Compares two derivations by weight. If their weights are equal, we "back of" to consider other factors 
    that will distinguish the two derivations, so [compare_derivations d1 d2] will return [0] only if [d1] and 
    [d2] are identical. The result when [d1] and [d2] are distinct but have equal weights is arbitrary but 
    consistent (i.e. the function does define a total ordering on derivations, which respects the ordering by weight). *)
val compare_derivations: ('a -> 'a -> int) -> ('a derivation_tree) -> ('a derivation_tree) -> int

(** {2 n-best lists via random sampling} *)

(** [generate num_samples n grammar_file] randomly samples [num_samples] derivations from the grammar read from [grammar_file], 
    and returns the best [n] by weight, sorted in decreasing order by weight. [num_samples] defaults to 300, so for values of [n] 
    that are not significantly smaller than this, a larger value of [num_samples] should be passed by the caller. *)
val generate : ?num_samples:int -> int -> string -> (string derivation_tree) list

(** {2 Exact n-best lists} *)

(** [get_n_best_from_chart n chart item] will produce the [n] best derivations (by weight) of [item] constructible from [chart] 
    (or all such derivations, if there are less than [n]). 
    The resulting list is ordered by decreasing weight. *)
val get_n_best_from_chart : int -> Chart.chart -> Chart.item -> (Chart.item derivation_tree) list

(** [get_n_best_from_grammar n rules nt] will produce the [n] best derivations (by weight) of the nonterminal [nt] constructible from the 
    grammar rules [rules] (or all such derivations, if there are less than [n]). 
    The resulting list is ordered by decreasing weight. *)
val get_n_best_from_grammar : int -> Rule.r list -> string -> (string derivation_tree) list  (* both occurrences of 'string' here represent nonterminals *)

(** {2 Displaying derivation trees} *)

val print_tree : ('a -> string) -> 'a derivation_tree -> string
val print_tree_sexp : ('a -> string) -> 'a derivation_tree -> string
val print_tree_compact : 'a derivation_tree -> string

(******************************************************************************************)
(** Functions by Angelica *)

(** Prints a derivation tree in a way that can be compiled by LaTeX. Relies on [forest] package. *)
val latex_tree : string derivation_tree -> string

(** Produces the derived "tuple" of strings (actually a string list) of the tree's root item. 
    If the produced list is length 1, then it represents a triple (left-of-head, head, right-of-head) 
        that has been concatenated into a singleton.
    If the produced list is length 3, then it represents a triple.
    If the produced list is length >3, then the first three elements represent a triple, and all following elements            
        represents triples that have been concatenated into singletons (aka moving elements). *)
val get_tuple : 'a derivation_tree -> string list
