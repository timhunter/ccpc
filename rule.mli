 

		type component = Component of int * int | Epsilon
		type tuplerecipe 
		type expansion = PublicTerminating of string | PublicNonTerminating of (string Nelist.t * tuplerecipe)
		type r 
		val to_string : r -> string
		val create_terminating : string * string * (Rational.rat option) -> r
		val create_nonterminating : string * (string list) * (component list list) * (Rational.rat option) -> r
		val create_rule : string * (string list) * tuplerecipe * (Rational.rat option) -> r
		val create_tuplerecipe : component list -> tuplerecipe
		val add_to_recipe : component list -> tuplerecipe -> tuplerecipe
		val rule_arity : r -> int
		val max_arity: r list -> int
		val rule_type : r -> string 
		val nonterm_degree : r -> int
		val get_nonterm : r -> string
		val get_weight : r -> Rational.rat option
		val get_expansion : r -> expansion
		val rule_recipe : r -> string
		val apply :  tuplerecipe ->
           (Util.range_item * Util.range_item) list list ->
           (Util.range_item * Util.range_item ->
            Util.range_item * Util.range_item ->
            Util.range_item * Util.range_item) ->
           (Util.range_item * Util.range_item) list

