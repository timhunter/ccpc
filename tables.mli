type 'a map (* maps a string to a list of 'a *)

val build_rule_map : Rule.r list -> int -> Rule.r map
val build_item_map : Chart.item list -> Chart.item map
val add : 'a map -> string -> 'a list -> unit
val mem : 'a map -> string -> bool
val find : 'a map -> string -> 'a list
