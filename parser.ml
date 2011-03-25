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
    type tables = {lRule_map: Rule.r list Grammar_Map.t ; rRule_map: Rule.r list Grammar_Map.t ; item_map: item list Item_Map.t}

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

		(*Builds a map with the symbol in the daughter position as the key, based upon the provided grammar*)
		let build_map grammar daughter =
		  let load_map map rule =
		    if (rule_arity rule)-1 >= daughter then  
        match Rule.get_expansion rule with 
		      | PublicTerminating str -> map
		      | PublicNonTerminating (rights, recipes) -> 
		         	let key = NEList.nth rights daughter in
			        if Grammar_Map.mem key map then 
			          let prev_value = Grammar_Map.find key map in
			          Grammar_Map.add key (rule::prev_value) map
			        else Grammar_Map.add key [rule] map 
        else map in 
		  List.fold_left load_map Grammar_Map.empty grammar 

(*builds a map out the provided items, using the nonterminal as the key*)
   let build_item_map items =
      let load_map map item =
        let key = get_nonterm item in
        if Item_Map.mem key map then
          let prev_value = Item_Map.find key map in
          Item_Map.add key (item::prev_value) map 
        else Item_Map.add key [item] map in
      List.fold_left load_map Item_Map.empty items 

(*Add an item to the given item map*)
    let add_item item_map item =
      let key = get_nonterm item in 
      if Item_Map.mem key item_map then 
        let prev_value = Item_Map.find key item_map in
        Item_Map.add key (item::prev_value) item_map
      else Item_Map.add key [item] item_map 
     
                
(*TODO: Currently using build_items instead of this. Might want to remove it and change the interface*)
		let build_nary rules items =
       (*let item_map = build_item_map items in*)
       let combine' items rule =
			  	match Rule.get_expansion rule with
				    | PublicTerminating _ -> None
				    | PublicNonTerminating (nts, f) ->
					      let left = Rule.get_nonterm rule in
				      (*  let possible_items = filter_items item_map rule in *)
                let item_nonterms = map_tr get_nonterm items in
                print_string "\n nonterms\n";
                for i=0 to (List.length item_nonterms)-1 do 
                  Printf.printf " %s" (List.nth item_nonterms i);
                done;

                let item_ranges = map_tr get_ranges items in
                if item_nonterms = NEList.to_list nts then
                  try
                    Some (create_item left (Rule.apply f item_ranges concat_ranges))
                  with
                    RangesNotAdjacentException -> None
                else
                    None in
		      optlistmap (combine' items) rules


    let build_items rules trigger items = 
     let build' rules item_list = 
       let combine' items rule =
         match Rule.get_expansion rule with
          | PublicTerminating _ -> None
          | PublicNonTerminating (nts, f) -> 
            let left = Rule.get_nonterm rule in 
            (*let possible_items = filter_items item_map rule in*)
            let item_nonterms = map_tr get_nonterm items in 
            let item_ranges = map_tr get_ranges items in 
            if item_nonterms = NEList.to_list nts then
                try
                    Some (create_item left (Rule.apply f item_ranges concat_ranges))
                  with
                    RangesNotAdjacentException -> None
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
		    Grammar_Map.find (get_nonterm trigger) rule_map
			with _ -> []
		
    (*produce a chart which contains only relevant items based on the given rules*)
    (*left_rules are rules where the trigger is the leftmost nonterminal, right_rules are the opposite*)
    let filter_chart item_map left_rules right_rules =
      let arity_two_rules = List.filter (fun r -> (rule_arity r) = 2) in   (*Only want to be dealing with rules of arity 2*)
      let left_rules = arity_two_rules left_rules in 
      let right_rules = arity_two_rules right_rules in 
      let rec get_items nonterms acc =   (*Get items out of the map which have the nonterminal as the key*)
        match nonterms with 
          | [] -> acc
          | h::t -> if (Item_Map.mem h item_map) then
                      get_items t ((Item_Map.find h item_map)::acc)
                    else 
                      get_items t acc in 
      let get_nts daughter rule =        (*For a given rule, get the nonterminal corresponding to the daughter, either right or left*)
        match Rule.get_expansion rule with
          | PublicNonTerminating (nts, recipes) -> List.nth (NEList.to_list nts) daughter
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
          
          let possible_rules = left_rules @ right_rules in
          let possible_items = filter_chart tables.item_map left_rules right_rules in 
          let all_new_items = build_items possible_rules trigger possible_items in 
         
          let useful_new_items = List.filter (fun x -> not (List.mem x chart)) all_new_items in  
         
          for i=0 to (List.length useful_new_items)-1 do
            Queue.add (List.nth useful_new_items i) q
          done;
          let item_map = List.fold_left (fun im item -> add_item im item) tables.item_map useful_new_items in
          let tables = {lRule_map = tables.lRule_map; rRule_map = tables.rRule_map; item_map = item_map} in 
          consequences (max_depth -1) prims (chart@useful_new_items) q tables
       
	  let deduce max_depth prims input =
	  let left_map = build_map prims 0 in 
    let right_map = build_map prims 1 in
	  let axioms = get_axioms prims input in
    let item_map = build_item_map axioms in 
    let tables = {lRule_map = left_map; rRule_map = right_map; item_map = item_map} in 
    let queue = Queue.create () in 
    for i=0 to (List.length axioms)-1 do
      Queue.add (List.nth axioms i) queue 
    done;
	 	  consequences max_depth prims axioms queue tables 
       	end


