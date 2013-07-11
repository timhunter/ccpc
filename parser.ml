open Util
open Rule
open Chart
open Fsa

module Tables : sig
    type 'a map
    val build_rule_map : Rule.r list -> int -> Rule.r map
    val build_item_map : Chart.item list -> Chart.item map
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
      let map = Map (Hashtbl.create 100) in 
      List.iter (load_map map) grammar;
      map

    (*builds a map out the provided items, using the nonterminal as the key*)
    let build_item_map items =
      let load_map map item =
        let key = Chart.get_nonterm item in
        add map key item
      in
      let map = Map (Hashtbl.create 100) in 
      List.iter (load_map map) items;
      map

    let find map nt =
      match map with
        Map tbl -> Hashtbl.find_all tbl nt

end

(******************************************************************************)

    type tables = {sRule_map: Rule.r Tables.map ; lRule_map: Rule.r Tables.map ; rRule_map: Rule.r Tables.map ; item_map: Chart.item Tables.map}

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
      trigger_item @ item_trigger @ trigger_only 

    (*produce a chart which contains only relevant items based on the given rules*)
    (*left_rules are rules where the trigger is the leftmost nonterminal, right_rules are the opposite*)
    let filter_chart item_map left_rules right_rules =
      let get_items nonterms = concatmap_tr (fun nt -> Tables.find item_map nt) nonterms in
      let get_nts daughter rule =        (*For a given rule, get the nonterminal corresponding to the daughter, either right or left*)
        match Rule.get_expansion rule with
          | PublicNonTerminating (nts, recipes) -> List.nth (Nelist.to_list nts) daughter
          | _ -> failwith "Error filtering chart" in
      let left_nts = List.map (get_nts 1) left_rules in   (*For right_rules, collect all the left daughters, opposite for left_rules*)
      let right_nts = List.map (get_nts 0) right_rules in 
      get_items (left_nts @ right_nts)

    let rec consequences chart q tables =
      if (Queue.is_empty q)
      then chart
      else
        let trigger = Queue.pop q in
        let trigger_nt = get_nonterm trigger in
          let left_rules = Tables.find tables.lRule_map trigger_nt in
          let right_rules = Tables.find tables.rRule_map trigger_nt in
          let single_rules = Tables.find tables.sRule_map trigger_nt in
          let possible_rules = single_rules @ left_rules @ right_rules in
          let possible_items = filter_chart tables.item_map left_rules right_rules in    (* Limits the chart to other items the trigger might combine with *)
          let all_new_items = ( build_items possible_rules trigger possible_items : ((item * route) list) ) in 
          let process (item,route) =
            match (Chart.get_status chart item route) with
            | NewItem -> Tables.add tables.item_map (get_nonterm item) item ;
                         Queue.add item q ;
                         Chart.add chart item route
            | OldItemNewRoute -> Chart.add chart item route
            | OldItemOldRoute -> ()
          in
          List.iter process all_new_items ;
          consequences chart q tables

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

    let deduce rules input =
      let arity_map = build_arity_map rules in 
      let left_map = Tables.build_rule_map (Array.get arity_map 2) 0 in 
      let right_map = Tables.build_rule_map (Array.get arity_map 2) 1 in
      let single_map = Tables.build_rule_map (Array.get arity_map 1) 0 in
      let axioms_list : ((item * Rule.r) list) = get_axioms rules input in   
      let axioms =
        let tbl = Chart.create 100 in 
        List.iter (fun (item,rule) -> Chart.add tbl item ([],rule)) axioms_list ;
        tbl
      in
      let item_map = Tables.build_item_map (map_tr (fun (x,_) -> x) axioms_list) in 
      let tables = {sRule_map = single_map; lRule_map = left_map; rRule_map = right_map; item_map = item_map} in 
      let queue = Queue.create () in 
      List.iter (fun (item,_) -> Queue.add item queue) axioms_list ;
      let chart = consequences axioms queue tables in
      chart

