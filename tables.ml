open Parser
open Rule

type map_value = RuleVal of Rule.r list | ItemVal of Chart.item list
type map = Map of (string, map_value) Hashtbl.t 
  

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
          if Hashtbl.mem map key then 
            let prev_value = 
              match Hashtbl.find map key with
                RuleVal rv -> rv
                | _ -> failwith "Should not encounter ItemVal here" in
            Hashtbl.add map key (RuleVal (rule::prev_value))
          else Hashtbl.add map key (RuleVal [rule])
    else () in
  let tbl = Hashtbl.create 100 in 
  List.iter (load_map tbl) grammar;
  Map tbl

(*builds a map out the provided items, using the nonterminal as the key*)
let build_item_map items =
  let load_map map item =
    let key = Chart.get_nonterm item in
    if Hashtbl.mem map key then
      let prev_value = 
        match Hashtbl.find map key with 
          ItemVal iv -> iv 
          | _ -> failwith "Should not have encountered RuleVal here" in
      Hashtbl.add map key (ItemVal (item::prev_value))
    else Hashtbl.add map key (ItemVal [item]) in
  let tbl = Hashtbl.create 100 in 
  List.iter (load_map tbl) items;
  Map tbl

let add map value key =
  match map with
    Map tbl -> Hashtbl.add tbl value key

let mem map value =
  match map with
    Map tbl -> Hashtbl.mem tbl value

let find map nt =
  match map with
    Map tbl -> Hashtbl.find tbl nt


