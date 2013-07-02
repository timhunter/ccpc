
(** CKY-style charts representing the parsing deductions licensed by an MCFG. *)

(*******************************************************************************************)
(** {2 Types} *)

type chart

(** An item is simply a "proposition" in the parsing-as-deduction sense. It does not 
    "know about" its antecedents or the rule that deduced it; this information is stored 
    in the chart. *)
type item

(** A valid deductive step: a list of existing items and a rule that can deduce a new 
    item from those items. *)
type route = (item list) * Rule.r

(** If we've just discovered that item [i] can be derived by route [r], a value of type 
    [item_route_status] is an answer to the question "What does this discovery add to what 
    we already knew?"
    The value [NewItem] means this is the first derivation of [i] we have discovered; 
    [OldItemNewRoute] means we already have some derivations of [i] but not ones via [r]; and
    [OldItemOldRoute] means we have already found derivations of [i] via [r]. *)
type item_route_status = NewItem | OldItemOldRoute | OldItemNewRoute

(*******************************************************************************************)
(** {2 Basic chart functions} *)

val create : int -> chart
val add : chart -> item -> route -> unit
val length : chart -> int

(** See description of [item_route_status] type. *)
val get_status : chart -> item -> route -> item_route_status

(*******************************************************************************************)
(** {2 Functions on items} *)

(** The result of [create_item s ranges] corresponds to the proposition that the nonterminal [s] is covered by 
    [ranges]. Note that since we are dealing with MCFGs, an item covers a list of ranges (or "spans"), not a single 
    range as in the case of a normal CFG. *)
val create_item: string -> Fsa.range list -> item

(** The nonterminal "covered" by this item. *)
val get_nonterm: item -> string

(** The ranges "covered" by this item. *)
val get_ranges: item -> Fsa.range list

(** All the routes that "lead to" this item. *)
val get_routes : item -> chart -> route list

val debug_str : item -> string
val debug_str_long : item -> chart -> string

val compare_items : item -> item -> int

(** [goal_item nt fsa] is the item corresponding to the proposition that the nonterminal [nt] covers the start-to-end 
    span of [fsa]. *)
val goal_item : string -> Fsa.fsa -> item

(*******************************************************************************************)
(** {2 Slow functions only for "high-level" use (i.e. not inside loops)} *)

val iter_items : chart -> (item -> unit) -> unit
val map_items : chart -> (item -> 'a) -> 'a list
