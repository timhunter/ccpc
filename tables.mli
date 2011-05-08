type map_value = RuleVal of Rule.r list | ItemVal of Chart.item list
type map

val build_rule_map: Rule.r list -> int -> map
val build_item_map: Chart.item list -> map
val add: map -> string -> map_value -> unit
val mem: map -> string -> bool
val find: map -> string -> map_value 
