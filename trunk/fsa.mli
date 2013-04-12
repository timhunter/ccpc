type fsa
type range = Range of fsa * ((int * int) option)
exception RangesNotAdjacentException

val make_fsa_prefix : string list -> fsa
val make_fsa_infix : string list -> fsa
val make_fsa_exact : string list -> fsa
val is_exact : fsa -> bool

val concat_ranges : range -> range -> range
val get_consumed_span : range -> (int * int) option
val goal_span : fsa -> (int * int) option
val end_state : fsa -> int
val axiom_spans : fsa -> string -> ((int * int) option) list

val description : fsa -> string
