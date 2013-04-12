type item
type chart 
type route = (item list) * Rule.r * Util.weight
type item_route_status = NewItem | OldItemOldRoute | OldItemNewRoute

val get_nonterm: item -> string
val create_item: string -> Fsa.range list -> item
val get_ranges: item -> Fsa.range list
val get_routes : item -> chart -> route list
val debug_str : item -> string
val debug_str_long : item -> chart -> string

val compare_items : item -> item -> int

val create : int -> chart 
val add : chart -> item -> route -> unit
val get_status : chart -> item -> route -> item_route_status
val length : chart -> int

val goal_item : string -> Fsa.fsa -> item

val goal_items : chart -> string -> Fsa.fsa -> item list
val iter_items : chart -> (item -> unit) -> unit
val map_items : chart -> (item -> 'a) -> 'a list
