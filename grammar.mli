
(** Functions manipulating (weighted or unweighted) multiple context-free grammars. 

    There is no distiguished grammar type: a grammar is simply represented by a list 
    of rules ([Rule.r list]) or sometimes by a list of rules paired with a string; 
    in the latter case, the string is the start-symbol of the grammar.
*)

(** Constructs an intersection grammar from a chart, using the algorithm from Dan Albro's dissertation (appendix C). 
    The result is a pair [(rules,str)] where [rules] is the list of rules comprising the intersection grammar, 
    and [str] is the intersection grammar's start symbol. 
    The second argument (type [string]) is the start symbol of the original grammar, and the third argument 
    is the FSA with which the original grammar was intersected/parsed to construct the chart. *)
val intersection_grammar : Chart.chart -> string -> Fsa.fsa -> (Rule.r list * string)

(** Maps a nonterminal from an intersection grammar back to the corresponding nonterminal in the "pre-intersection" grammar. *)
val desituate : string -> string

(** Maps a rule from an intersection grammar back to the corresponding rule in the "pre-intersection" grammar. *)
val desituate_rule : Rule.r -> Rule.r

(** Reads a (weighted or unweighted) MCFG from a file with the specified name. *)
val get_input_grammar : string -> (Rule.r list * string)

(** Reads from the specified dict file and returns a mapping from guillaumin-generated 
    preterminals (e.g. ["t123"]) to feature sequences (e.g. [":: =N D -f"]). *)
val get_guillaumin_dict : string -> (string, string) Hashtbl.t

val drawgraph : Chart.chart -> Chart.item list -> string list -> string -> unit
