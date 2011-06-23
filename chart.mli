type backpointer = item ref option * item ref option
and item = ParseItem of string * ((Util.range_item * Util.range_item) list) * backpointer option * Rational.rat (*range_item defined in Util*) 

val get_nonterm: item -> string
val create_item: string -> (Util.range_item * Util.range_item) list -> backpointer option -> Rational.rat -> item 
val get_ranges: item -> (Util.range_item * Util.range_item) list 
val get_backpointer: item -> backpointer option
val to_string: item -> string list -> string 
val debug_str : item -> string
type chart 

val create : int -> chart 
val add : chart -> item -> unit
val mem : chart -> item -> bool
val length : chart -> int
val iter : (item -> string -> unit) -> chart -> unit
val find : chart -> item -> string
val fold : (item -> string -> 'a -> 'a) -> chart -> 'a -> 'a
