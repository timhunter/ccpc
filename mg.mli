
(** Functions and types specific to Minimalist Grammars. *)

type derived_tree = Leaf of string | NonLeaf of (string option) * derived_tree * derived_tree
type derived_tree2 = L of string | NL of string * ((int * int) list list) * derived_tree2 * derived_tree2 

(** Returns the surface tree derived by a derivation. Fails if the provided derivation derives an MCFG nonterminal of rank greater than one. *)
val derived_tree : string Derivation.derivation_tree -> derived_tree
val derived_tree2 : string -> string Derivation.derivation_tree -> derived_tree2

(** Prints a derived tree in a way that can be compiled by LaTeX. Relies on [forest] package. *)
(* val latex_derived_tree : derived_tree -> string *)
val latex_derived_tree2 : derived_tree2 -> string

(** Prints a derivation tree, with some MG-specific tweaks, in a way that can be compiled by LaTeX. Relies on [forest] package. *)
val latex_derivation_tree : string -> string -> string Derivation.derivation_tree -> string

(** Produces the features of the tree's root item as a list of string lists. 
  The first element is ["::"] or [":"], to indicate whether the node is lexical or non-lexical. 
  The second element is the list of root item features.
  If there are any additional elements, they are the features of items that will be Moved. *)
val get_features : (string, string) Hashtbl.t -> string -> string list list

