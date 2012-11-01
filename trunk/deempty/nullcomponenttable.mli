type t
val create : unit -> t
val add : t -> string -> int -> unit
val mem : t -> string -> int -> bool
val lookup : t -> string -> int list
val remove : t -> string -> int -> unit
val clear : t -> unit
val get_items : t -> (string * int list) list
    
