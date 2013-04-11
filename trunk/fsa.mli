type fsa = Prefix of (string list) | Infix of (string list) | Sentence of (string list)
type range = Range of fsa * ((int * int) option)
exception RangesNotAdjacentException

val concat_ranges : range -> range -> range
val get_consumed_span : range -> (int * int) option
val goal_span : fsa -> (int * int) option
val end_state : fsa -> int
val find_arcs : fsa -> string -> (int * int) list
