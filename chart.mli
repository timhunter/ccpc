type item
type chart 
type route = (item list) * Rule.r * (Num.num * Num.num) option
type item_route_status = NewItem | OldItemOldRoute | OldItemNewRoute

val get_nonterm: item -> string
val create_item: string -> Util.range list -> item
val get_ranges: item -> Util.range list
val get_routes : item -> chart -> route list
val to_string: item -> string list -> string 
val debug_str : item -> string
val debug_str_long : item -> chart -> string

val create : int -> chart 
val add : chart -> item -> route -> unit
val get_status : chart -> item -> route -> item_route_status
val length : chart -> int

val goal_item : string -> int -> item

val goal_items : chart -> string -> int -> item list
val iter_items : chart -> (item -> unit) -> unit
val map_items : chart -> (item -> 'a) -> 'a list
