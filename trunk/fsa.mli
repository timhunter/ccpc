type fsa = Prefix of int | Infix of int | Sentence of int
type range = Range of fsa * ((int * int) option)
exception RangesNotAdjacentException

val concat_ranges : range -> range -> range
val get_consumed_span : range -> (int * int) option
val goal_span : fsa -> (int * int) option
