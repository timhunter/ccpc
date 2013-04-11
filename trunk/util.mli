type fsa = Prefix of int | Infix of int | Sentence of int
type range = Range of fsa * ((int * int) option)
exception RangesNotAdjacentException
exception EmptyListException

val reverse_tr : 'a list -> 'a list
val map_tr : ('a -> 'b) -> 'a list -> 'b list
val concatmap_tr: ('a -> 'b list) -> 'a list -> 'b list
val take : int -> 'a list -> 'a list
val take_while : ('a -> bool) -> 'a list -> 'a list
val optlistmap : ('a -> 'b option) -> 'a list -> 'b list
val require_no_nones : ('a option) list -> ('a list) option
val concat_ranges : range -> range -> range
val get_consumed_span : range -> (int * int) option
val goal_span : fsa -> (int * int) option
val range : int -> int -> int list
val (^^) : string -> string -> string
val find_in_list : 'a -> 'a list -> int list
val split : char -> string -> string list
val uniques : 'a list -> 'a list
val all_lists : 'a list -> int -> 'a list list

val show_list : ('a -> string) -> 'a list -> string

type weight
val no_weight : weight
val make_weight : Num.num -> Num.num -> weight
val show_weight : weight -> string
val show_weight_float : weight -> string
val mult_weights : weight -> weight -> weight
val compare_weights : weight -> weight -> int
(* These last two functions are used only by generate.ml. Might be nice 
 * to get rid of them when/if that becomes obsolete. *)
val weight_numerator : weight -> Num.num option
val weight_denominator : weight -> Num.num option
