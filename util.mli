type range_item = RangeVal of int | EpsVar
exception RangesNotAdjacentException
exception EmptyListException

val reverse_tr : 'a list -> 'a list
val map_tr : ('a -> 'b) -> 'a list -> 'b list
val concatmap_tr: ('a -> 'b list) -> 'a list -> 'b list
val optlistmap : ('a -> 'b option) -> 'a list -> 'b list
val concat_ranges : (range_item * range_item) -> (range_item * range_item) -> (range_item * range_item)
val range : int -> int -> int list
val (^^) : string -> string -> string
val find_in_list : 'a -> 'a list -> int list
val split : char -> string -> string list
val uniques : 'a list -> 'a list
val all_lists : 'a list -> int -> 'a list list

