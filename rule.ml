open Util

(****
 This type is meant to be understood as part of the RULE interface.
 (For some reason that I don't understand, I can't put it inside the RULE signature, 
 and so I'm forced to leave the type variable unbound; ideally this type declaration 
 would just refer to tuplerecipe in the sig.)
 PublicTerminating and PublicNonTerminating are used outside this file to pattern-match 
 on the result of get_expansion. In different implementations of the RULE interface, the 
 r type (and maybe the tuplerecipe type) declared inside the struct will differ, but 
 this type here should stay the same.
 ****)
type 'a expansion = PublicTerminating of string | PublicNonTerminating of (string NEList.t * 'a)

module type RULE =
	sig
		type tuplerecipe
		type r
		val print_rule : r -> unit
		val create_terminating : string * string -> r
		val create_nonterminating : string * (string list) * ((int * int) list list) -> r
		val create_rule : string * (string list) * tuplerecipe -> r
		val rule_arity : r -> int
		val nonterm_degree : r -> int
		val get_nonterm : r -> string
		val get_expansion : r -> tuplerecipe expansion
		val apply : tuplerecipe -> 'a list list -> ('a -> 'a -> 'a) -> 'a list
		val eval_str : tuplerecipe expansion -> string list list -> string list
	end ;;

module Rule : RULE =
	struct

		type stringrecipe = (int * int) NEList.t
		type tuplerecipe = stringrecipe NEList.t
		type r = Terminating of (string * string) | NonTerminating of (string * string NEList.t * tuplerecipe)

		(**********************************************************)

		let getstr yields concat (i,j) =
			let relevant_yield = List.nth yields i in
			List.nth relevant_yield j

		let makestr list_of_pairs yields concat =
			let pieces_to_concatenate = NEList.map (getstr yields concat) list_of_pairs in
			NEList.fold concat pieces_to_concatenate

		let apply srecipes yields concat =
			NEList.to_list (NEList.map (fun sf -> makestr sf yields concat) srecipes)

		let eval_str exp yields =
			match exp with
			| PublicTerminating s -> [s]
			| PublicNonTerminating (_, srecipes) -> NEList.to_list (NEList.map (fun strfunc -> makestr strfunc yields (^)) srecipes)

		(**********************************************************)

		(* Input to these creation functions should be aggressively verified *)

		let create_terminating (nonterm, term) = Terminating (nonterm, term)

		(* TODO: Still need to make sure the ``rights'' and the ``recipes'' are compatible *)
		let create_nonterminating (nonterm, rights, recipes) =
			let checked_rights =
				try NEList.from_list rights
				with EmptyListException -> failwith("Nonterminating rule must have nonterminals to expand to") in
			let checked_recipes =
				try NEList.from_list (List.map NEList.from_list recipes)
				with EmptyListException -> failwith("Nonterminating rule must have a function for composing yields") in
			NonTerminating (nonterm, checked_rights, checked_recipes)

		let create_rule (nonterm, rights, srecipes) = create_nonterminating (nonterm, rights, List.map NEList.to_list (NEList.to_list srecipes))

		(**********************************************************)

		(* How many nonterminals does this rule put together? *)
		let rule_arity rule =
			match rule with
			| Terminating _ -> 0
			| NonTerminating (left, rights, _) -> NEList.length rights

		(* How many components does the tuple produced by this rule have? *)
		let nonterm_degree rule =
			match rule with
			| Terminating _ -> 1
			| NonTerminating (left, rights, recipes) -> NEList.length recipes

		let get_nonterm rule =
			match rule with
			| Terminating (nt, _) -> nt
			| NonTerminating (nt, _, _) -> nt

		let get_expansion rule =
			match rule with
			| Terminating (_, str) -> PublicTerminating str
			| NonTerminating (_, rights, recipes) -> PublicNonTerminating (rights, recipes)

		(**********************************************************)

		let print_rule rule =
			let left = get_nonterm rule in
			let rhs_output = 
				match (get_expansion rule) with
				| PublicTerminating s -> s
				| PublicNonTerminating (rights, _) -> List.fold_left (^^) "" (NEList.to_list rights)
			in
			Printf.printf "%s  --->  %s\n" left rhs_output

	end ;;

