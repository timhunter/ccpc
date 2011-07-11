type item
type chart 
type route = (item list) * Rational.rat option

val get_nonterm: item -> string
val create_item: string -> (Util.range_item * Util.range_item) list -> item
val get_ranges: item -> (Util.range_item * Util.range_item) list
val get_routes : item -> chart -> route list
val to_string: item -> string list -> string 
val debug_str : item -> string

val create : int -> bool -> chart 
val add : chart -> item -> route -> unit
val mem : chart -> item -> bool
val mem_route : chart -> item -> route -> bool
val length : chart -> int

val goal_items : chart -> string -> int -> item list
val item_list : chart -> item list
