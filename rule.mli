 

		type component = Component of int * int | Epsilon
		type tuplerecipe 
		type expansion = PublicTerminating of string | PublicNonTerminating of (string Nelist.t * tuplerecipe)
		type r 

		type marked_mg_rule = LeftAdjunction | RightAdjunction
		val get_marked_mg_rule : (string, string) Hashtbl.t -> r -> marked_mg_rule option

		val to_string : r -> string
		val create_terminating : string * string * (Rational.rat option) -> r
		val create_nonterminating : string * (string list) * (component list list) * (Rational.rat option) -> r
		val create_rule : string * (string list) * tuplerecipe * (Rational.rat option) -> r
		val create_tuplerecipe : component list -> tuplerecipe
		val add_to_recipe : component list -> tuplerecipe -> tuplerecipe
		val rule_arity : r -> int
		val max_arity: r list -> int
		val nonterm_degree : r -> int
		val get_nonterm : r -> string
		val get_rhs : r -> string list
		val get_weight : r -> Rational.rat option
                val get_recipe : r -> tuplerecipe
                val get_indices : component -> int * int 
		val get_expansion : r -> expansion
		val map_nonterms : (string -> string) -> r -> r
		val apply :  tuplerecipe ->
           'a list list ->
           ('a ->
            'a ->
            'a) ->
           'a list
                

