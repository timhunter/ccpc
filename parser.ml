open Util
open Rule



		type prim = Rule.r
		type backpointer = item option * item option
		and item = ParseItem of string * ((range_item * range_item) list) * backpointer option (*range_item defined in Util*) 
		type input = Prefix of (string list) | Sentence of (string list)
    type tables = {sRule_map: (string, Rule.r list) Hashtbl.t ; lRule_map: (string, Rule.r list) Hashtbl.t ; rRule_map: (string, Rule.r list) Hashtbl.t ; item_map: (string, item list) Hashtbl.t}

    let create_item str ranges bp = ParseItem(str, ranges, bp)
		let get_ranges = function ParseItem(_, rs,_) -> rs
   
		let get_nonterm = function ParseItem(nt, _,_) -> nt
		let get_backpointer = function ParseItem(_,_,bp) -> bp

		let is_goal input item =
			match input with
			| Sentence strings -> (get_nonterm item = "S") && (get_ranges item = [(RangeVal 0, RangeVal (List.length strings))])
			| Prefix strings -> failwith("Help! Start symbol isn't going to be S!")



		let to_string item =
			let ParseItem (nt, ranges, bp) = item in
			let range_strings = map_tr (fun (p,q) -> match (p,q) with 
																									| (EpsVar, EpsVar) -> Printf.sprintf "(Epsilon, Epsilon)"
																									| (RangeVal a, RangeVal b) -> Printf.sprintf "(%d,%d)" a b  
																									| _ -> failwith "Should never mix RangeVal and EpsVar!") ranges in 
			  Printf.sprintf "'{%s, %s}'" nt (List.fold_left (^) "" range_strings) 


		let get_axioms_parse grammar symbols =
		        let indices = range 0 (List.length symbols) in
			let make_axiom nt term i = if (List.nth symbols i) = term then Some (create_item nt [RangeVal i, RangeVal (i+1)] None) else None in
			let get_axiom symbols rule =
				match Rule.get_expansion rule with
				| PublicTerminating str -> optlistmap (make_axiom (Rule.get_nonterm rule) str) indices
				| PublicNonTerminating _ -> []
			in
			concatmap_tr (get_axiom symbols) grammar 
			
		let get_axioms_intersect grammar prefix =
			let len = List.length prefix in
			let situated_axiom nt index = create_item nt [(RangeVal index, RangeVal (index+1))] None in
			let unsituated_axiom nt = create_item nt [(RangeVal len, RangeVal len)] None in
			let get_axiom rule =
				let nt = Rule.get_nonterm rule in
				match Rule.get_expansion rule with
				| PublicTerminating str -> (unsituated_axiom nt) :: (map_tr (situated_axiom nt) (find_in_list str prefix))
				| PublicNonTerminating _ -> []
			in
			concatmap_tr get_axiom grammar

		let get_axioms grammar input =
		 	let from_symbols =	match input with
				| Prefix strings -> get_axioms_intersect grammar strings
				| Sentence strings -> get_axioms_parse grammar strings in 
			let rec get_empties gram acc =
				match gram with 
					| [] -> acc
					| h::t -> (match Rule.get_expansion h with 
									  	|	PublicTerminating str -> if str = " " then (get_empties t ((create_item (Rule.get_nonterm h) [EpsVar, EpsVar] None)::acc))
																											 else get_empties t acc
											| PublicNonTerminating _ -> get_empties t acc) in 
			(get_empties grammar []) @ from_symbols 
	
		(*Builds a map with the symbol in the daughter position as the key, based upon the provided grammar*)
		let build_map grammar daughter =
		  let load_map map rule =
		    if (rule_arity rule)-1 >= daughter then  
        match Rule.get_expansion rule with 
		      | PublicTerminating str -> () 
		      | PublicNonTerminating (rights, recipes) -> 
		         	let key = Nelist.nth rights daughter in
			        if Hashtbl.mem map key then 
			          let prev_value = Hashtbl.find map key in
			          Hashtbl.add map key (rule::prev_value) 
			        else Hashtbl.add map key [rule]  
        else () in
			let tbl = Hashtbl.create 100 in 
		  List.iter (load_map tbl) grammar;
			tbl

(*builds a map out the provided items, using the nonterminal as the key*)
   let build_item_map items =
      let load_map map item =
        let key = get_nonterm item in
        if Hashtbl.mem map key then
          let prev_value = Hashtbl.find map key in
          Hashtbl.add map key (item::prev_value) 
        else Hashtbl.add map key [item] in
			let tbl = Hashtbl.create 100 in 
      List.iter (load_map tbl) items;
			tbl

(*Add an item to the given item map*)
    let add_item item_map item =
      let key = get_nonterm item in 
      if Hashtbl.mem item_map key then 
        let prev_value = Hashtbl.find item_map key in
        Hashtbl.add item_map key (item::prev_value) 
      else Hashtbl.add item_map key [item] 
     
    let build_items rules trigger items = 
     let build' rules item_list = 
       let combine' items rule =
			 	 (*Printf.printf "\nRULE: ";
				 print_rule rule;*)
         match Rule.get_expansion rule with
          | PublicTerminating _ -> None
          | PublicNonTerminating (nts, f) -> 
            let left = Rule.get_nonterm rule in 
            let item_nonterms = map_tr get_nonterm items in 
            let item_ranges = map_tr get_ranges items in 
            if item_nonterms = Nelist.to_list nts then
                try
                    match items with 
										 [h] -> (*(if (Rule.get_nonterm rule) = "t157" then Printf.printf "\n%s" (to_string h)); *)Some (create_item left (Rule.apply f item_ranges concat_ranges) (Some (Some h, None)) )
										 | [h;t] -> (*(if (Rule.get_nonterm rule) = "t157" then (Printf.printf "\n%s" (to_string h); Printf.printf " ---- %s" (to_string t)));*) Some (create_item left (Rule.apply f item_ranges concat_ranges) (Some (Some h, Some t))) 
										 | _ -> failwith "List can only have one or two items"
			         with
                    RangesNotAdjacentException -> (*(if (Rule.get_nonterm rule) = "t157" then Printf.printf "OH no");*) None
                else
                    None in
		    optlistmap (combine' item_list) rules in 
      let trigger_item = concatmap_tr (fun item -> (build' rules [trigger;item])) items in 
      let item_trigger =  concatmap_tr (fun item -> (build' rules [item;trigger])) items in
      let trigger_only = (build' rules) [trigger] in 
      trigger_item @ item_trigger @ trigger_only 


		(* Filter rules based on current items using the map*)
		let filter_rules rule_map trigger = 
		  try
		    Hashtbl.find rule_map (get_nonterm trigger)
			with _ -> []
		
    (*produce a chart which contains only relevant items based on the given rules*)
    (*left_rules are rules where the trigger is the leftmost nonterminal, right_rules are the opposite*)
    let filter_chart item_map left_rules right_rules =
			let rec get_items nonterms acc =   (*Get items out of the map which have the nonterminal as the key*)
        match nonterms with 
          | [] -> acc
          | h::t -> if (Hashtbl.mem item_map h) then
                      get_items t ((Hashtbl.find item_map h)::acc)
                    else 
                      get_items t acc in 
      let get_nts daughter rule =        (*For a given rule, get the nonterminal corresponding to the daughter, either right or left*)
        match Rule.get_expansion rule with
          | PublicNonTerminating (nts, recipes) -> List.nth (Nelist.to_list nts) daughter
          | _ -> failwith "Error filtering chart" in
      let left_nts = List.map (get_nts 1) left_rules in   (*For right_rules, collect all the left daughters, opposite for left_rules*)
      let right_nts = List.map (get_nts 0) right_rules in 
      let left_items = get_items left_nts [] in
      let right_items = get_items right_nts [] in 
      List.flatten ( left_items @ right_items)
	
    let rec consequences max_depth prims chart q tables =
      if (Queue.is_empty q)
      then chart
      else
        let trigger = Queue.pop q in
          let left_rules = filter_rules tables.lRule_map trigger in 

          let right_rules = filter_rules tables.rRule_map trigger in

					let single_rules = filter_rules tables.sRule_map trigger in 
          
          let possible_rules = single_rules @ left_rules @ right_rules in
          let possible_items = filter_chart tables.item_map left_rules right_rules in 
          let all_new_items = build_items possible_rules trigger possible_items in 
         
          let useful_new_items = List.filter (fun x -> not (Setlike.mem chart x)) all_new_items in  
         
          for i=0 to (List.length useful_new_items)-1 do
						let item = List.nth useful_new_items i in 
            Queue.add item q;
						Setlike.add chart item
          done;
          List.iter (fun item -> add_item tables.item_map item) useful_new_items; 
          consequences (max_depth -1) prims chart q tables
       
		let build_arity_map rules =
		 let arr = Array.make 3 [] in 
		 let rec build' lst =
		   match lst with
			 	| [] -> arr
				| h::t -> match Rule.rule_arity h with
										| 0 -> Array.set arr 0 (h::(Array.get arr 0)); build' t
										| 1 -> Array.set arr 1 (h::(Array.get arr 1)); build' t
										| 2 -> Array.set arr 2 (h::(Array.get arr 2)); build' t 
										| _ -> build' t in 
		 build' rules




	  let deduce max_depth prims input =
			let arity_map = build_arity_map prims in 
			let left_map = build_map (Array.get arity_map 2) 0 in 
			let right_map = build_map (Array.get arity_map 2) 1 in
			let single_map = build_map (Array.get arity_map 1) 0 in
	    let axioms_list = get_axioms prims input in 	
			let axioms =
				let tbl = Setlike.create 100 in 
				let rec add lst  =
					match lst with 
						| [] -> tbl
						| h::t -> Setlike.add tbl h ; add t in 
				add axioms_list in
			let item_map = build_item_map axioms_list in 
			let tables = {sRule_map = single_map; lRule_map = left_map; rRule_map = right_map; item_map = item_map} in 
			let queue = Queue.create () in 
			for i=0 to (List.length axioms_list)-1 do
				Queue.add (List.nth axioms_list i) queue 
			done;
			let chart = consequences max_depth prims axioms queue tables in
			let chart_as_list = Hashtbl.fold (fun a b lst -> (a::lst)) chart [] in
			chart_as_list


