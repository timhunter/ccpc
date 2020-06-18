
(** Functions and types specific to Minimalist Grammars. *)

type derived_tree = Leaf of string | NonLeaf of (string option) * derived_tree * derived_tree

(** Prints a derived tree in a way that can be compiled by LaTeX. Relies on [forest] package. *)
val latex_derived_tree : derived_tree -> string

