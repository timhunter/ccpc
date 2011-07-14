open Util
open Rule
open Tables
open Chart
open Rational

    type prim = Rule.r
    type input = Prefix of (string list) | Sentence of (string list)
    type tables = {sRule_map: Rule.r Tables.map ; lRule_map: Rule.r Tables.map ; rRule_map: Rule.r Tables.map ; item_map: Chart.item Tables.map}

    
    let is_goal start_symbol length item =
      (get_nonterm item = start_symbol) && (get_ranges item = [(RangeVal 0, RangeVal length)])

    (* return type is (item, Rational.rat option) *)
    let get_axioms_parse grammar symbols =
      let indices = range 0 (List.length symbols) in
      let make_axiom nt term r i = if (List.nth symbols i) = term then Some (create_item nt [RangeVal i, RangeVal (i+1)], r, Rule.get_weight r) else None in
      let get_axiom symbols rule =
        match Rule.get_expansion rule with
        | PublicTerminating str -> optlistmap (make_axiom (Rule.get_nonterm rule) str rule) indices
        | PublicNonTerminating _ -> []
      in
      concatmap_tr (get_axiom symbols) grammar 
      
    (* return type is (item, Rational.rat option) *)
    let get_axioms_intersect grammar prefix =
      let len = List.length prefix in
      let situated_axiom nt rule index = (create_item nt [(RangeVal index, RangeVal (index+1))], rule, Rule.get_weight rule) in
      let unsituated_axiom nt rule = (create_item nt [(RangeVal len, RangeVal len)], rule, Rule.get_weight rule) in
      let get_axiom rule =
        let nt = Rule.get_nonterm rule in
        match Rule.get_expansion rule with
        | PublicTerminating str -> (unsituated_axiom nt rule) :: (map_tr (situated_axiom nt rule) (find_in_list str prefix))
        | PublicNonTerminating _ -> []
      in
      concatmap_tr get_axiom grammar

    let get_axioms grammar input =
      let from_symbols =  match input with
        | Prefix strings -> get_axioms_intersect grammar strings
        | Sentence strings -> get_axioms_parse grammar strings in 
      (* get_axioms_parse only gives us items that cover non-empty chunks of the input; get_empties creates 
         items that can go anywhere in the input (i.e. EpsVar) from each terminating rule that produces the empty string *)
      let rec get_empties gram acc =
        match gram with 
          | [] -> acc
          | h::t -> (match Rule.get_expansion h with 
                      | PublicTerminating str -> if str = " " then (get_empties t ((create_item (Rule.get_nonterm h) [EpsVar, EpsVar], h, Rule.get_weight h)::acc))
                                                       else get_empties t acc
                      | PublicNonTerminating _ -> get_empties t acc) in 
      (get_empties grammar []) @ from_symbols 
  
  

(*Add an item to the given item map*)
    let add_item item_map item =
      let key = get_nonterm item in 
      Tables.add item_map key item

    let build_items rules trigger items = 
     let build' rules item_list = 
       let combine' items rule =
         match Rule.get_expansion rule with
          | PublicTerminating _ -> None
          | PublicNonTerminating (nts, f) -> 
            let left = Rule.get_nonterm rule in 
            let item_nonterms = map_tr get_nonterm items in 
            let item_ranges = map_tr get_ranges items in 
            if item_nonterms = Nelist.to_list nts then
                try
                    match items with 
                     | [h] ->   Some (create_item left (Rule.apply f item_ranges concat_ranges), ([h], rule, Rule.get_weight rule))
                     | [h;t] -> Some (create_item left (Rule.apply f item_ranges concat_ranges), ([h;t], rule, Rule.get_weight rule))
                     | _ -> failwith "List can only have one or two items"
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
         Tables.find rule_map (get_nonterm trigger)
      with _ -> []
    
    (*produce a chart which contains only relevant items based on the given rules*)
    (*left_rules are rules where the trigger is the leftmost nonterminal, right_rules are the opposite*)
    let filter_chart item_map left_rules right_rules =
      let rec get_items nonterms acc =   (*Get items out of the map which have the nonterminal as the key*)
        match nonterms with 
          | [] -> acc
          | h::t -> if (Tables.mem item_map h) then
                      let itm = Tables.find item_map h in
                      get_items t (itm::acc)
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
      List.flatten (left_items @ right_items)
  
    let rec consequences max_depth prims chart q tables =
      <:DEBUG< "===== Size of queue is %d\n" (Queue.length q) >> ;
      (* Queue.iter (fun item -> <:DEBUG< "%s\n" (Chart.debug_str item) >>) q ; *)
      if (Queue.is_empty q)
      then chart
      else
        let trigger = Queue.pop q in
          <:DEBUG< "      Trigger item: %s\n" (Chart.debug_str trigger) >> ;
          let left_rules = filter_rules tables.lRule_map trigger in 

          let right_rules = filter_rules tables.rRule_map trigger in

          let single_rules = filter_rules tables.sRule_map trigger in 
          
          let possible_rules = single_rules @ left_rules @ right_rules in
          let possible_items = filter_chart tables.item_map left_rules right_rules in    (* Limits the chart to other items the trigger might combine with *)
          let all_new_items = ( build_items possible_rules trigger possible_items : ((item * route) list) ) in 
         
          <:DEBUG< "%d new items\n" (List.length all_new_items) >> ;
          let process (item,route) =
            if not (Chart.mem chart item) then (
              <:DEBUG< "  %s \tnew item (hence new route)\n" (Chart.debug_str item) >> ;
              add_item tables.item_map item ;
              Queue.add item q ;
              Chart.add chart item route
            ) else (
              if not (Chart.mem_route chart item route) then (
                <:DEBUG< "  %s \told item, new route\n" (Chart.debug_str item) >> ;
                Chart.add chart item route
              ) else (
                <:DEBUG< "  %s \told item, old route\n" (Chart.debug_str item) >> ;
                ()
              )
            )
          in
          List.iter process all_new_items ;
          consequences (max_depth -1) prims chart q tables
       
    (* Produces a length-three array of rule lists; nullary, unary and binary rules *)
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
      let left_map = build_rule_map (Array.get arity_map 2) 0 in 
      let right_map = build_rule_map (Array.get arity_map 2) 1 in
      let single_map = build_rule_map (Array.get arity_map 1) 0 in
      let axioms_list : ((item * Rule.r * Rational.rat option) list) = get_axioms prims input in   
      let axioms =
        let tbl = Chart.create 100 in 
        let rec add lst  =
          match lst with 
            | [] -> tbl
            | (item,rule,weight)::t -> Chart.add tbl item ([],rule,weight) ; add t in 
        add axioms_list in
      let item_map = build_item_map (map_tr (fun (x,_,_) -> x) axioms_list) in 
      let tables = {sRule_map = single_map; lRule_map = left_map; rRule_map = right_map; item_map = item_map} in 
      let queue = Queue.create () in 
      List.iter (fun (item,_,_) -> Queue.add item queue) axioms_list ;
      let chart = consequences max_depth prims axioms queue tables in
      <:DEBUG< "Chart contains %d propositions\n" (Chart.length chart) >> ;
      chart

