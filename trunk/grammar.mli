val intersection_grammar : Chart.chart -> Chart.item list -> string -> Fsa.fsa -> (Rule.r list * string)
val desituate : string -> string
val desituate_rule : Rule.r -> Rule.r
val get_input_grammar : string -> (Rule.r list * string)
val drawgraph : Chart.chart -> Chart.item list -> string list -> string -> unit
