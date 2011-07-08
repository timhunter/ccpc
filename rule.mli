type 'a expansion = PublicTerminating of string | PublicNonTerminating of (string Nelist.t * 'a)
 

		type component = Component of int * int | Epsilon
		type tuplerecipe 
		type r 
		val to_string : r -> string
		val create_terminating : string * string * Rational.rat -> r
		val create_nonterminating : string * (string list) * (component list list) *Rational.rat -> r
		val create_rule : string * (string list) * tuplerecipe * Rational.rat -> r
		val create_tuplerecipe : component list -> tuplerecipe
		val add_to_recipe : component list -> tuplerecipe -> tuplerecipe
		val rule_arity : r -> int
		val max_arity: r list -> int
		val nonterm_degree : r -> int
		val get_nonterm : r -> string
    val get_weight : r -> int * int
		val get_expansion : r -> tuplerecipe expansion
		val apply :  tuplerecipe ->
           (Util.range_item * Util.range_item) list list ->
           (Util.range_item * Util.range_item ->
            Util.range_item * Util.range_item ->
            Util.range_item * Util.range_item) ->
           (Util.range_item * Util.range_item) list

		(*val eval_str : tuplerecipe expansion -> string list list -> string list*)

