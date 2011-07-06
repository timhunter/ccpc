val get_input_grammar : string -> Rule.r list
val parse : Rule.r list -> string list -> Chart.item list
val timed_parse : Rule.r list -> string list -> (float * Chart.item list)
val run_parser : string list -> string -> string list
val print_tree : Chart.item -> string list -> string 
