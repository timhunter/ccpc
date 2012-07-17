type range = Pair of int * int | VarRange of int * int   (* VarRange n means (i,i) for any i, 0 <= i < n *)
exception RangesNotAdjacentException
exception EmptyListException

val set_debug_mode : bool -> unit
val debug : ('a, unit, string, unit) format4 -> 'a
val debug_fast : string Lazy.t -> unit

val reverse_tr : 'a list -> 'a list
val map_tr : ('a -> 'b) -> 'a list -> 'b list
val concatmap_tr: ('a -> 'b list) -> 'a list -> 'b list
val take : int -> 'a list -> 'a list
val optlistmap : ('a -> 'b option) -> 'a list -> 'b list
val concat_ranges : range -> range -> range
val range : int -> int -> int list
val (^^) : string -> string -> string
val find_in_list : 'a -> 'a list -> int list
val split : char -> string -> string list
val uniques : 'a list -> 'a list
val all_lists : 'a list -> int -> 'a list list

type weight
val no_weight : weight
val make_weight : Num.num -> Num.num -> weight
val show_weight : weight -> string
val mult_weights : weight -> weight -> weight
val compare_weights : weight -> weight -> int
(* These last two functions are used only by generate.ml. Might be nice 
 * to get rid of them when/if that becomes obsolete. *)
val weight_numerator : weight -> Num.num option
val weight_denominator : weight -> Num.num option
