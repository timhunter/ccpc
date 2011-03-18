open Util
open Rule

module Key =
  struct
    type t = string
    let compare = String.compare
  end;;

module Grammar_Map = Map.Make(Key);;

module Item_Map = Map.Make(Key);;

module MCFG_ParserGen =
	struct

		type prim = Rule.r
		type item = ParseItem of string * ((int * int) list)
		type input = Prefix of (string list) | Sentence of (string list)

    let create_item str ranges = ParseItem(str, ranges)

		let is_goal input item =
			match input with
			| Sentence strings -> item = ParseItem ("S", [(0, List.length strings)])
			| Prefix strings -> failwith("Help! Start symbol isn't going to be S!")

		let get_nonterm = function ParseItem(nt, _) -> nt

		let get_ranges = function ParseItem(_, rs) -> rs

		let to_string item =
			let ParseItem (nt, ranges) = item in
			let range_strings = map_tr (fun (p,q) -> Printf.sprintf "(%d,%d)" p q) ranges in
			Printf.sprintf "[%s, %s]" nt (List.fold_left (^) "" range_strings)

		let get_axioms_parse grammar symbols =
		        let indices = range 0 (List.length symbols) in
			let make_axiom nt term i = if (List.nth symbols i) = term then Some (create_item nt [i,i+1]) else None in
			let get_axiom symbols rule =
				match Rule.get_expansion rule with
				| PublicTerminating str -> optlistmap (make_axiom (Rule.get_nonterm rule) str) indices
				| PublicNonTerminating _ -> []
			in
			concatmap_tr (get_axiom symbols) grammar 
			
		let get_axioms_intersect grammar prefix =
			let len = List.length prefix in
			let situated_axiom nt index = create_item nt [(index,index+1)] in
			let unsituated_axiom nt = create_item nt [(len,len)] in
			let get_axiom rule =
				let nt = Rule.get_nonterm rule in
				match Rule.get_expansion rule with
				| PublicTerminating str -> (unsituated_axiom nt) :: (map_tr (situated_axiom nt) (find_in_list str prefix))
				| PublicNonTerminating _ -> []
			in
			concatmap_tr get_axiom grammar

		let get_axioms grammar input =
			match input with
			| Prefix strings -> get_axioms_intersect grammar strings
			| Sentence strings -> get_axioms_parse grammar strings

		let rule_arity rule = Rule.rule_arity rule

		let max_arity rules = List.fold_left max 0 (map_tr Rule.rule_arity rules)

		(*Builds a map with leftmost symbol as the key, based upon the provided grammar*)
		let build_map grammar =
		  let load_map map rule =
		    match Rule.get_expansion rule with 
		      | PublicTerminating str -> map
		      | PublicNonTerminating (rights, recipes) -> 
		         	let key = NEList.nth rights 0 in
			        if Grammar_Map.mem key map then 
			          let prev_value = Grammar_Map.find key map in
			          Grammar_Map.add key (rule::prev_value) map
			        else Grammar_Map.add key [rule] map in
		  List.fold_left load_map Grammar_Map.empty grammar 

(*
   let build_item_map items =
      let load_map map item =
        let key = get_nonterm item in
        if Item_Map.mem key map then
          let prev_value = Item_Map.find key map in
          Item_Map.add key (item::prev_value) map 
        else Item_Map.add key [item] map in
      List.fold_left load_map Item_Map.empty items 
*)
    let filter_items map rule =
      let rec filter' lst acc =
        match lst with 
          | [] -> acc 
          | h::t -> 
              try
                  let res = Item_Map.find h map in
                  filter' t (res::acc)
              with _ ->
                filter' t acc in
      let rights = match Rule.get_expansion rule with 
                     | PublicNonTerminating (rights, recipes) -> NEList.to_list rights
                     | _ -> print_string "This should never happen..."; [] in
     (*The concat might be slowing things down. Find a different way?*)
     uniques (List.concat (filter' rights [])) 
     
                

		let build_nary rules items =
		   (*let item_map = build_item_map items in*)
       let combine' items rule =
			  	match Rule.get_expansion rule with
				    | PublicTerminating _ -> None
				    | PublicNonTerminating (nts, f) ->
					      let left = Rule.get_nonterm rule in
				      (*  let possible_items = filter_items item_map rule in *)
                let item_nonterms = map_tr get_nonterm items in
                let item_ranges = map_tr get_ranges items in
                if item_nonterms = NEList.to_list nts then
                  try
                    Some (create_item left (Rule.apply f item_ranges concat_ranges))
                  with
                    RangesNotAdjacentException -> None
                else
                    None in
		      optlistmap (combine' items) rules

		(* Filter rules based on current items using the map*)
		let filter_rules gram_map items = 
		  let rec filter' lst acc =
		      match lst with 
			| [] -> acc
			| h::t -> 
			  (*if Grammar_Map.mem (get_nonterm h) gram_map then *)
        try
			    filter' t ((Grammar_Map.find (get_nonterm h) gram_map)::acc)
			  with _ ->
			    filter' t acc in 
		  uniques (List.flatten (filter' items []))
		  
		let rec consequences max_depth prims recent_items old_items gram_map =
		        let all_existing_items = recent_items @ old_items in
			if max_depth = 0 then
				all_existing_items
			else
			       match recent_items with
					  [] -> old_items
					| (i::is) ->
					        let possible_rules = filter_rules gram_map recent_items in
									
					        let rules_of_arity n = List.filter (fun rule -> rule_arity rule = n) possible_rules in
						     
                  let new_items n = concatmap_tr (build_nary (rules_of_arity n))  (all_lists all_existing_items n)  in
   						   
                  let all_new_items = concatmap_tr new_items (range 1 ((max_arity prims)+1)) in
		 				     
                  let useful_new_items = List.filter (fun x -> not (List.mem x all_existing_items)) all_new_items in
					      	
                  consequences (max_depth-1) prims (is @ useful_new_items) (i :: old_items) gram_map
		 
		  let deduce max_depth prims input =
		  let gram_map = build_map prims in 
		  let axioms = get_axioms prims input in
	  	  consequences max_depth prims axioms [] gram_map
		
       	end


