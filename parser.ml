open Util
open Rule
open Chart
open Fsa

module Tables : sig
    type 'a map
    val build_rule_map : Rule.r list -> int -> Rule.r map
    val add : 'a map -> string -> 'a -> unit
    val find : 'a map -> string -> 'a list
end = struct

    type 'a map = Map of (string, 'a) Hashtbl.t

    let add map key value =
      let (Map tbl) = map in
      Hashtbl.add tbl key value

    (*Builds a map with the symbol in the daughter position as the key, based upon the provided grammar. *)
    (* daughter is an index into the right-hand side of a rule. *)
    (* The returned object maps a nonterminal NT to a list of rules which have NT in the daughter'th position in their right-hand side. *)
    let build_rule_map grammar daughter =
      let load_map map rule =
        if (Rule.rule_arity rule)-1 >= daughter then  
        match Rule.get_expansion rule with 
          | PublicTerminating str -> () 
          | PublicNonTerminating (rights, recipes) -> 
              let key = Nelist.nth rights daughter in
              add map key rule
        else () in
      let map = Map (Hashtbl.create ((List.length grammar)/2)) in  (* was 100 *)
      List.iter (load_map map) grammar;
      map

    let find map nt =
      match map with
        Map tbl -> Hashtbl.find_all tbl nt

end

(******************************************************************************)

    type tables = {sRule_map: Rule.r Tables.map ; lRule_map: Rule.r Tables.map ; rRule_map: Rule.r Tables.map}

    (* return type is Chart.item * Rule.r * Rational.rat option *)
    let get_axioms grammar fsa =
      let get_axiom rule =
        match Rule.get_expansion rule with
        | PublicTerminating str ->
          map_tr (let nt = Rule.get_nonterm rule in
                  fun span -> (create_item nt [Range(fsa,span)], rule))
                 (axiom_spans fsa str)
        | PublicNonTerminating _ -> []
      in
      concatmap_tr get_axiom grammar

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
                     | [h] ->   Some (create_item left (Rule.apply f item_ranges concat_ranges), ([h], rule))
                     | [h;t] -> Some (create_item left (Rule.apply f item_ranges concat_ranges), ([h;t], rule))
                     | _ -> failwith "List can only have one or two items"
               with
                    RangesNotAdjacentException -> None
                else
                    None in
        optlistmap (combine' item_list) rules in 
      let trigger_item = concatmap_tr (fun item -> (build' rules [trigger;item])) items in 
      let item_trigger =  concatmap_tr (fun item -> (build' rules [item;trigger])) items in
      let trigger_only = (build' rules) [trigger] in 
      append_tr (append_tr trigger_item item_trigger) trigger_only 

    (*produce a chart which contains only relevant items based on the given rules*)
    (*left_rules are rules where the trigger is the leftmost nonterminal, right_rules are the opposite*)
    let filter_chart chart left_rules right_rules =
      let get_items nonterms = concatmap_tr (fun nt -> Chart.get_items chart nt) nonterms in
      let get_nts daughter rule =        (*For a given rule, get the nonterminal corresponding to the daughter, either right or left*)
        match Rule.get_expansion rule with
          | PublicNonTerminating (nts, recipes) -> List.nth (Nelist.to_list nts) daughter
          | _ -> failwith "Error filtering chart" in
      let left_nts = List.map (get_nts 1) left_rules in   (*For right_rules, collect all the left daughters, opposite for left_rules*)
      let right_nts = List.map (get_nts 0) right_rules in 
      get_items (append_tr left_nts right_nts)

    let rec consequences chart q tables =
      if (Queue.is_empty q)
      then chart
      else
        let trigger = Queue.pop q in
        let trigger_nt = get_nonterm trigger in
          let left_rules = Tables.find tables.lRule_map trigger_nt in
          let right_rules = Tables.find tables.rRule_map trigger_nt in
          let single_rules = Tables.find tables.sRule_map trigger_nt in
          let possible_rules = append_tr (append_tr single_rules left_rules) right_rules in
          let possible_items = filter_chart chart left_rules right_rules in    (* other items the trigger might combine with *)
          let all_new_items = ( build_items possible_rules trigger possible_items : ((item * route) list) ) in 
          let process (item,route) =
            match (Chart.get_status chart item route) with
            | NewItem -> Queue.add item q ;
                         Chart.add ~is_new_item:(Some true) chart item route
            | OldItemNewRoute -> Chart.add ~is_new_item:(Some false) chart item route
            | OldItemOldRoute -> ()
          in
          List.iter process all_new_items ;
          consequences chart q tables

    let deduce rules input =
      let pick_out_rules (acc_unary_rules, acc_binary_rules) r =
        match (Rule.rule_arity r) with
            | 0 -> (acc_unary_rules, acc_binary_rules)
            | 1 -> (r::acc_unary_rules, acc_binary_rules)
            | 2 -> (acc_unary_rules, r::acc_binary_rules)
            | _ -> (Printf.eprintf "WARNING: Ignoring rule with arity greater than two:\n         %s\n%!" (to_string r); 
                    (acc_unary_rules, acc_binary_rules))
      in
      let (unary_rules, binary_rules) = List.fold_left pick_out_rules ([],[]) rules in
      let left_map = Tables.build_rule_map binary_rules 0 in 
      let right_map = Tables.build_rule_map binary_rules 1 in
      let single_map = Tables.build_rule_map unary_rules 0 in
      let axioms_list : ((item * Rule.r) list) = get_axioms rules input in   
      let initial_chart =
        let c = Chart.create 500 in
        List.iter (fun (item,rule) -> Chart.add c item ([],rule)) axioms_list ;
        c
      in
      let tables = {sRule_map = single_map; lRule_map = left_map; rRule_map = right_map} in 
      let queue = Queue.create () in 
      List.iter (fun (item,_) -> Queue.add item queue) axioms_list ;
      let chart = consequences initial_chart queue tables in
      chart

