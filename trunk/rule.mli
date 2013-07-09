
(** Multiple context-free grammar rules. *)

(** A rule. *)
type r 

(** A "recipe" for combining some list of tuples of strings, to form a new list of tuples of strings. *)
type tuplerecipe 

(** A right hand side of a rule. We restrict ourselves to rules that either (a) introduce a single string 
    and no nonterminals (i.e. [PublicTerminating]), or (b) introduce a non-empty list of nonterminals and a 
    recipe for combining their yields (i.e. [PublicNonTerminating]). *)
type expansion = PublicTerminating of string | PublicNonTerminating of (string Nelist.t * tuplerecipe)

(** A piece of a [tuplerecipe]. *)
type component

val to_string : r -> string
val rule_arity : r -> int
val get_nonterm : r -> string
val get_weight : r -> Util.weight
val get_expansion : r -> expansion
val map_nonterms : (string -> string) -> r -> r
val apply :  tuplerecipe ->
           'a list list ->
           ('a ->
            'a ->
            'a) ->
           'a list

(** {2 Creating rules} *)

val create_terminating : string * string * Util.weight -> r
val create_nonterminating : string * (string list) * (component list list) * Util.weight -> r
val create_rule : string * (string list) * tuplerecipe * Util.weight -> r
val create_tuplerecipe : component list -> tuplerecipe
val create_component : int -> int -> component
val add_to_recipe : component list -> tuplerecipe -> tuplerecipe

(** {2 Interpreting MCFG rules as MG rules} *)

type marked_mg_rule = LeftAdjunction | RightAdjunction
val get_marked_mg_rule : (string, string) Hashtbl.t -> r -> marked_mg_rule option

