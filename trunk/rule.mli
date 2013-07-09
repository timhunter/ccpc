
(** Multiple context-free grammar rules. *)

(** A rule. *)
type r 

(** A "recipe" for combining some list of tuples of strings, to form a new list of tuples of strings. *)
type tuplerecipe 

(** A right hand side of a rule. We restrict ourselves to rules that either (a) introduce a single string 
    and no nonterminals (i.e. [PublicTerminating]), or (b) introduce a non-empty list of nonterminals and a 
    recipe for combining their yields (i.e. [PublicNonTerminating]). *)
type expansion = PublicTerminating of string | PublicNonTerminating of (string Nelist.t * tuplerecipe)

(** A string representation of the rule, in the format of a line of a (W)MCFG file. *)
val to_string : r -> string

(** The number of nonterminals introduced by this rule (zero in the case of a terminating rule). *)
val rule_arity : r -> int

(** The nonterminal that is expanded by this rule. *)
val get_nonterm : r -> string

(** The weight of this rule. *)
val get_weight : r -> Util.weight

(** The right hand side of this rule; see description of [expansion] type above. *)
val get_expansion : r -> expansion

(** [map_nonterms f r] produces a new rule which is like [r], except that each nonterminal [nt] mentioned 
    in [r] is replaced by [f nt]. *)
val map_nonterms : (string -> string) -> r -> r

(** [apply recipe tuple_list concat] produces the result of applying [recipe] to the list-of-tuples [tuple_list], 
    where [concat] is a function for concatenating the elements of the tuples. (Typically, ['a] is [string], and 
    [concat] performs simple string concatenation. *)
val apply :  tuplerecipe ->
           'a list list ->
           ('a ->
            'a ->
            'a) ->
           'a list

(** {2 Creating rules} *)

(** [create_terminating (nt, str, wt)] creates a rule that expands the nonterminal [nt] into the string [str], with weight [wt]. *)
val create_terminating : string * string * Util.weight -> r

(** [create_nonterminating (nt, children, recipe, wt)] creates a rule that expands the nonterminal [nt], introducing 
    the nonterminals [children] whose yields are to be combined as specified by [recipe], with weight [wt]. *)
val create_nonterminating : string * (string list) * tuplerecipe * Util.weight -> r

(** The following type and functions are exposed as part of the public interface to this module only because they are 
    used by the lex/yacc parser that reads (W)MCFG files. *)

type component
val create_tuplerecipe : component list -> tuplerecipe
val create_component : int -> int -> component
val add_to_recipe : component list -> tuplerecipe -> tuplerecipe

(** {2 Interpreting MCFG rules as MG rules} *)

(** Certain MCFG rules correspond to special "marked" MG rules; MG rules that aren't the normal 
    merge and move rules. These rules need to be indicated in a "derivation string" in order for 
    it to identify a derivation, because they break the result of Hale and Stabler which says that 
    the string of lexical items (in derivation-tree order) is enough. *)
type marked_mg_rule = LeftAdjunction | RightAdjunction
    
(** Given an MG-MCFG dictionary and an MCFG rule, gives back the corresponding marked MG rule if there is one.
    (NB: May not give the right results if you pass it a situated grammar rule! If you need to check a 
    situated rule, use [Grammar.desituate_rule] first.) *)
val get_marked_mg_rule : (string, string) Hashtbl.t -> r -> marked_mg_rule option

