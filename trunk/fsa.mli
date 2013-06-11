type fsa
type state
type range = Range of fsa * ((state * state) option)
exception RangesNotAdjacentException

val make_fsa_prefix : string -> fsa
val make_fsa_infix : string -> fsa
val make_fsa_exact : string -> fsa
val make_fsa_from_file : string -> fsa
val is_exact : fsa -> bool

val string_of : state -> string

val concat_ranges : range -> range -> range
val ranges_equal : range -> range -> bool
val get_consumed_span : range -> (state * state) option
val goal_span : fsa -> (state * state) option
val start_state : fsa -> state
val end_state : fsa -> state
val axiom_spans : fsa -> string -> ((state * state) option) list

val weight_of_arc : fsa -> (state * state) -> string -> Util.weight

val description : fsa -> string

(* Just an arbitrary identifier. Not promising anything other than that it's unique for each 
 * state of a single machine. Included because the graph-drawing code needs an int for each state. *)
val index_of : state -> int
