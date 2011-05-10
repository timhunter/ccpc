type backpointer = item option * item option
and item = ParseItem of string * ((Util.range_item * Util.range_item) list) * backpointer option * Rational.rat (*range_item defined in Util*) 

val get_nonterm: item -> string
val create_item: string -> (Util.range_item * Util.range_item) list -> backpointer option -> Rational.rat -> item 
val get_ranges: item -> (Util.range_item * Util.range_item) list 
val get_backpointer: item -> backpointer option
val to_string: item -> string 
type t

val create : int -> t
val add : t -> item -> unit
val mem : t -> item -> bool
val length : t -> int
val iter : (item -> string -> unit) -> t -> unit
val find : t -> item -> string
val fold : (item -> string -> 'a -> 'a) -> t -> 'a -> 'a
