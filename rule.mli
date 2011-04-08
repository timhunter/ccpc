type 'a expansion = PublicTerminating of string | PublicNonTerminating of (string Nelist.t * 'a)
 

		type component = Component of int * int | Epsilon
		type tuplerecipe 
		type r 
		val print_rule : r -> unit
		val create_terminating : string * string -> r
		val create_nonterminating : string * (string list) * (component list list) -> r
		val create_rule : string * (string list) * tuplerecipe -> r
		val create_tuplerecipe : component list -> tuplerecipe
		val add_to_recipe : component list -> tuplerecipe -> tuplerecipe
		val rule_arity : r -> int
		val nonterm_degree : r -> int
		val get_nonterm : r -> string
		val get_expansion : r -> tuplerecipe expansion
		val apply : tuplerecipe -> 'a list list -> ('a -> 'a -> 'a) -> 'a list
		val eval_str : tuplerecipe expansion -> string list list -> string list

