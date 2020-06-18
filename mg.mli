
(** Functions and types specific to Minimalist Grammars. *)

type derived_tree = Leaf of string | NonLeaf of (string option) * derived_tree * derived_tree

(** Returns the surface tree derived by a derivation. Fails if the provided derivation derives an MCFG nonterminal of rank greater than one. *)
val derived_tree : string Derivation.derivation_tree -> derived_tree

(** Prints a derived tree in a way that can be compiled by LaTeX. Relies on [forest] package. *)
val latex_derived_tree : derived_tree -> string

(** Prints a derivation tree, with some MG-specific tweaks, in a way that can be compiled by LaTeX. Relies on [forest] package. *)
val latex_derivation_tree : string -> string -> string Derivation.derivation_tree -> string

