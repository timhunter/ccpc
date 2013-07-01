open Rule

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


