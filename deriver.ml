open Util
open Rule

module MCFG_Deriver =
	struct

		type ntyield = (string list) list

		type prim = Rule.r
		type item = DerivItem of string * ntyield
		type input = ()
		type chart = int    

		(* Don't understand why, but I can't pass as input a () value that originates outside *)
		(* Seems to be something special about the () type *)
		let null_input = ()

		let get_nonterm = function (DerivItem (nonterm,_)) -> nonterm
		let get_ntyield = function (DerivItem (_,ntyield)) -> ntyield

		(* let is_goal _ (DerivItem (sym,ntyield)) = (sym = "S") *)
		let is_goal _ item = (get_nonterm item = "S")

		let get_axioms grammar _ =
			let get_axiom rule =
				match Rule.get_expansion rule with
				| PublicTerminating str -> Some (DerivItem (Rule.get_nonterm rule, [[str]]))
				| PublicNonTerminating _ -> None
			in
			optlistmap get_axiom grammar

		let max_arity rules = List.fold_left max 0 (List.map Rule.rule_arity rules)

		let rule_arity rule = Rule.rule_arity rule

		let build_nary rules items =
			let build_nary' items rule =
				match Rule.get_expansion rule with
				| PublicTerminating str -> None
				| PublicNonTerminating (nts, f) ->
					let left = Rule.get_nonterm rule in
					let yields = List.map get_ntyield items in
					if ((List.map get_nonterm items) = (Nelist.to_list nts)) then (Some (DerivItem(left, Rule.apply f yields (@)))) else None
			in
			optlistmap (build_nary' items) rules
		
		let add_to_chart chart item = 0 

	end;;


