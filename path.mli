
(** Paths through weighted hypergraphs (for example, weighted grammars). *)

(** A type for representing a path through a weighted hypergraph that has elements of type ['a] 
    as its vertices. Along with each step from vertex [x] to vertex [y], we record the hyperedge 
    that "licensed" this step; this hyperedge has a weight, and identifies which other vertices [y'] 
    it also leads to from [x].

    A path can represent a cycle by beginning and ending at the same node (or a path can properly 
    contain a cycle). Notice that there are multiple distinct representations of "the same cycle" 
    that differ only in where they begin and end; see the [rotate] and [canonicalise] functions. *)
type 'a history

(** A singleton path *)
val singleton : 'a -> 'a history

(** The starting point of the path. There is always such a node (i.e. there is no "empty path"). *)
val root : 'a history -> 'a

(** Returns a triple [(ls,x,rs)] where [x] is the last node on the path. 
    If the path is singleton, [ls] and [rs] are empty; otherwise the (ordered) list of tails of 
    the edge that led us to [x] is [(ls @ \[x\] @ rs)]. *)
val last : 'a history -> 'a list * 'a * 'a list

(** [extend h wt (ls,x,rs)] constructs a new history by "appending" to the end of [h] a step 
    to the vertex [x], where the edge licensing this last step has weight [wt] and has tails 
    [(ls @ \[x\] @ rs)] (i.e. [ls] and [rs] record the "co-tails" of [x] that we didn't go to). *)
val extend : 'a history -> Util.weight -> ('a list * 'a * 'a list) -> 'a history

(** The list of all vertices on this path. *)
val to_list : 'a history -> 'a list

(** Tries to split a path into two sub-paths, [h1] and [h2], such that [h2] is a loop. A loop is a 
    non-singleton path that begins and ends with the same vertex. If this is possible, returns 
    [Some (h1,h2)]; otherwise, returns [None]. If multiple such splits are possible, returns the 
    split that makes [h1] as long as possible (i.e. uses the earliest possible split). *)
val try_ending_cycle : 'a history -> ('a history * 'a history) option

(** Product of all the weights of hyperedges used on this path. *)
val weight_product : 'a history -> Util.weight

(** In [rotate x h], the path [h] must contains the vertex [x] and must be a loop, i.e. must begin and end with the same vertex. 
    The result is a path that represents the same loop as [h] does, but with [x] as the first and last vertex. *)
val rotate : 'a -> 'a history -> 'a history

(** Rotates a path into a canonical form, based on a given comparison function for vertices. *)
val canonicalise : ('a -> 'a -> int) -> 'a history -> 'a history

(** {2 Functions for displaying paths} *)

val latex_history_tikz : ('a -> string) -> 'a history -> string
val show_history : ('a -> string) -> ('a history) -> string
val show_history_full : ('a -> string) -> ('a history) -> string

(** {2 Functions for working with paths through derivation trees.} *)

(* These functions are for working with paths and cycles through derivation trees, in 
 * contrast to the stuff in cycles.ml which concerns paths and cycles in grammars. 
 * (Perhaps it would be good to unify the two using a hypergraph type at some point.) *)
val get_cycles : 'a Derivation.derivation_tree -> ('a history * 'a history) list
val get_path : (string -> bool) -> string Derivation.derivation_tree -> string history

