
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

(** Reads a (weighted or unweighted) MCFG from a file with the specified name. *)
val get_input_grammar : string -> (Rule.r list * string)

(** Re-expresses all weights in a grammar so that whenever two rules have the same 
    left-hand side, their weights have the same denominator. *)
val ensure_common_denominators : Rule.r list -> Rule.r list

(** Reads from the specified dict file and returns a mapping from guillaumin-generated 
    preterminals (e.g. ["t123"]) to feature sequences (e.g. [":: =N D -f"]). *)
val get_guillaumin_dict : string -> (string, string) Hashtbl.t

val drawgraph : Chart.chart -> Chart.item list -> string list -> string -> unit

(* Angelica (5/7/20): added [List.rev] *)
val get_nonterminals : Rule.r list -> string -> string list

(******************************************************************************************)

(* Functions by Angelica *)

(* Creates the fertility matrix of a grammar. *)
val fertility_matrix : (Rule.r list * string) -> Matrix.matrix

(** Checks whether the given grammar is consistent. *)
val is_consistent : (Rule.r list * string) -> bool