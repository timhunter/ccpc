open Util
open Rational

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
type 'a expansion = PublicTerminating of string | PublicNonTerminating of (string Nelist.t * 'a)


    
		type component = Component of int * int | Epsilon
		type stringrecipe = component Nelist.t
		type tuplerecipe = stringrecipe Nelist.t
		type r = Terminating of (string * string * Rational.rat) | NonTerminating of (string * string Nelist.t * tuplerecipe * Rational.rat) 

		(**********************************************************)

		let getstr yields pair =
			match pair with 
				| Component (i,j) ->
							let relevant_yield = List.nth yields i in
						 (List.nth relevant_yield j)
			  | Epsilon -> (EpsVar, EpsVar) 

		let makestr list_of_pairs yields concat =
			let pieces_to_concatenate = Nelist.map (getstr yields) list_of_pairs in
		  (Nelist.fold concat pieces_to_concatenate)

		let apply srecipes yields concat =
			let res = Nelist.to_list (Nelist.map (fun sf -> makestr sf yields concat) srecipes) in 
		(*	let r = List.fold_left (fun acc x -> match x with 
																						 None -> acc
																						 | Some a -> (a::acc)) [] res in *)
			res

		(*let eval_str exp yields =
			match exp with
			| PublicTerminating s -> [s]
			| PublicNonTerminating (_, srecipes) -> Nelist.to_list (Nelist.map (fun strfunc -> makestr strfunc yields (^)) srecipes) *)

    let create_tuplerecipe lst =
			Nelist.from_list [(Nelist.from_list lst)]

		let add_to_recipe lst recipe =
			let component = Nelist.from_list lst in 
			Nelist.from_list (component::(Nelist.to_list recipe))
		
		(**********************************************************)

		(* Input to these creation functions should be aggressively verified *)

		let create_terminating (nonterm, term, weight) = Terminating (nonterm, term, weight)

		(* TODO: Still need to make sure the ``rights'' and the ``recipes'' are compatible *)
		let create_nonterminating (nonterm, rights, recipes, weight) =
			let checked_rights =
				try Nelist.from_list rights
				with EmptyListException -> failwith("Nonterminating rule must have nonterminals to expand to") in
			let checked_recipes =
				try Nelist.from_list (List.map Nelist.from_list recipes)
				with EmptyListException -> failwith("Nonterminating rule must have a function for composing yields") in
			NonTerminating (nonterm, checked_rights, checked_recipes, weight)

		let create_rule (nonterm, rights, srecipes, weight) = create_nonterminating (nonterm, rights, List.map Nelist.to_list (Nelist.to_list srecipes), weight)

		(**********************************************************)

		(* How many nonterminals does this rule put together? *)
		let rule_arity rule =
			match rule with
			| Terminating _ -> 0
			| NonTerminating (left, rights, _, _) -> Nelist.length rights
	  
		let max_arity rules = List.fold_left max 0 (map_tr rule_arity rules)

		(* How many components does the tuple produced by this rule have? *)
		let nonterm_degree rule =
			match rule with
			| Terminating _ -> 1
			| NonTerminating (left, rights, recipes, weight) -> Nelist.length recipes

		let get_nonterm rule =
			match rule with
			| Terminating (nt, _, _) -> nt
			| NonTerminating (nt, _, _, _) -> nt

		let get_expansion rule =
			match rule with
			| Terminating (_, str, weight) -> PublicTerminating str
			| NonTerminating (_, rights, recipes, weight) -> PublicNonTerminating (rights, recipes)

		(**********************************************************)

		let component_to_string prev tup = 
		  let tup = match tup with
				| Component (a,b) -> (a,b)
				| Epsilon -> (333, 333) in 
			String.concat "" [(string_of_int (fst tup)); ","; (string_of_int(snd tup)); ";"; prev]
		
		let stringrecipe_to_string lst =
			String.concat "" ["["; (List.fold_left component_to_string "" (List.rev (Nelist.to_list lst))); "]"]
	
		let print_rule rule =
			let left = get_nonterm rule in
			let rhs_output = 
				match (get_expansion rule) with
				| PublicTerminating s -> s
				| PublicNonTerminating (rights, _) -> List.fold_left (^^) "" (Nelist.to_list rights)
			in
			let recipe =
				match (get_expansion rule) with
					| PublicTerminating s -> []
					| PublicNonTerminating (_,recs) -> List.map stringrecipe_to_string (Nelist.to_list recs) in
			let recipe = String.concat "" recipe in 
			Printf.printf "%s  --->  %s %s\n" left rhs_output recipe


