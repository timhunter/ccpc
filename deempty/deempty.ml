open Rule
type grammar = Rule.t list

let epsilon = [""],[[]]
(* find all categories that only have empty productions *)
let identify_empty (g : grammar) : string list = 
  (* build a hashtable as follows: true if all rules of a token are empty, 
   * otherwise false *)
  let rec build_table (tbl : (string,bool) Hashtbl.t) (g : grammar) =
    match g with
	(tok,rw,sy)::t -> 
	  (if (compare (rw,sy) epsilon)=0
	   then 
	      (try let _ = Hashtbl.find tbl tok in build_table tbl t
	       with Not_found -> Hashtbl.add tbl tok true; build_table tbl t)
	   else ((Hashtbl.add tbl tok false); build_table tbl t))
      | _ -> () in
  let tbl = Hashtbl.create 101 in
  build_table tbl g;
  Hashtbl.fold (fun k v acc -> if v=true then k::acc else acc) tbl []
    
(* build a data structure that keeps null component information *)
let nullcomponenttable = Nullcomponenttable.create ()
let add_nullcomponent tok comp = Nullcomponenttable.add nullcomponenttable tok comp 
(* lookup all members of a rewrite and return a (category index, component index) list *)
let lookup_nullcomponents (tok_lst : string list) : (int * int) list =
  let _,ret_lst = (List.fold_left 
		     (fun (i,acc) tok -> 
		       (i+1, (List.fold_left (fun lst cat -> (i,cat)::lst) [] (Nullcomponenttable.lookup nullcomponenttable tok))::acc)) 
		     (0,[]) tok_lst) in
  List.flatten ret_lst

(* find out if a rule needs to be rewritten - are any nullable (cat,comp) pairs in the string yield? *)
let rule_needs_update ((tok, rw, sy) : Rule.t) : bool =
  let lst = List.flatten sy in
  List.fold_left (fun acc (i,j) -> if List.mem (Component(i,j)) lst then true else acc) false (lookup_nullcomponents rw)
    
(* find out if a grammar contains rules that need to be updated *)
let grammar_needs_update g = 
  List.fold_left (fun acc r -> (rule_needs_update r) || acc) false g
    
(* add all empty components of a rule to the table - check that no other productions exist where that component is not null *)
let add_nullable_components ((tok, rw, sy) : Rule.t) (g : grammar) : unit =
  if not (List.mem [Epsilon] sy) (* if there are no empty categories in the string yield fxn *)
  then ()
  else 
    let emptycomps = List.map (fun c -> c=[Epsilon]) sy in
    let emptycomps = 
      (List.fold_left 
	 (fun acc (t,_,s) -> 
	   if t=tok 
	   then (List.map2 (fun current acc -> current=[Epsilon] && acc) s acc) 
	   else acc) 
	 emptycomps g) in
    let _ = List.fold_left (fun i c -> (if c then add_nullcomponent tok i); (i+1)) 0 emptycomps in ()
												
(* rewrite a rule, replacing nullable components with Epsilon *)
let modify_rule ((tok, rw, sy) : Rule.t) (g : grammar): Rule.t =
  let nullcomponents = List.map (fun (i,j) -> (Component(i,j))) (lookup_nullcomponents rw) in
  if nullcomponents=[]
  then (tok,rw,sy) (* there is no change *)
  else
    let new_sy = 
      (List.map (fun l -> (List.map (fun c -> if (c != Epsilon) && not (List.mem c nullcomponents) then c else Epsilon) l)) sy) in 
    (tok, rw, new_sy)
      
let modify_grammar (g : grammar) : grammar =
  (* add all initially null categories to the nullcomponents table*)
  List.iter (fun tok -> add_nullcomponent tok 0) (identify_empty g); 
  add_nullcomponent "E" 0;
  (* modify rules until no further modification is necessary *)
  let rec mod_grammar g =
    if grammar_needs_update g
    then
      let mod_g = List.map (fun r -> modify_rule r g) g in
      List.iter (fun r -> add_nullable_components r mod_g) mod_g;
      mod_grammar  mod_g
    else g in
  mod_grammar g

(* remove all epsilons from string yield
 * then remove all rules with an empty string yield *)
let clean_grammar (g : grammar) : grammar =
  let remove_epsilon = List.map (List.filter (fun c -> not (c = Epsilon))) in
  let remove_emptylst = (List.filter (fun lst -> not (lst=[]))) in
  let clean_sy sy = remove_emptylst (remove_epsilon sy) in
  let remove_empty_rules = List.filter (fun (cat,children,sy) -> not (sy=[])) in
  let g = List.map (fun (cat,children,sy) -> (cat,children,(clean_sy sy))) g in
  remove_empty_rules g

(* remove children that are unreferenced in the string yield 
 * also rereference the string yield to account for the absense of removed children *)
let remove_children (g : grammar) : grammar = 
  let remove_children_prime ((tok,children,sy) : Rule.t) : Rule.t =
    let referenced = 
      List.fold_left 
        (fun acc comp ->
          match comp with
            Component(i,_) -> if (List.mem i acc) then acc else i::acc 
          | Epsilon -> acc)
          [] (List.flatten sy) in
    let referenced = List.sort compare referenced in
    let rec remove i daughters ret =
      match daughters with
        [] -> ret
      | h::t -> if List.mem i referenced then remove (i+1) t (h::ret) else remove (i+1) t ret in
    let rereference ((tok,children,sy) : Rule.t) : Rule.t =
      (* a mapping of i -> i* ---- from old (i,_) to new (i*,_) *)
      let (_,mapping) = List.fold_left (fun (i,acc) x -> ((i+1),((x,i)::acc))) (0,[]) referenced in
      let map_lst = List.map (fun (Component(i,j)) -> Component((List.assoc i mapping),j)) in
      (tok,children,(List.map (fun lst -> map_lst lst) sy)) in
    let r = (tok, (List.rev (remove 0 children [])), sy) in
    rereference r in
  List.map remove_children_prime g

let deempty (g : Rule.t list) : Rule.t list = 
  Componentmap.initialize g;
  let g = remove_children (clean_grammar (modify_grammar g)) in
  Componentmap.compute_mappings (Nullcomponenttable.get_items nullcomponenttable);
  Componentmap.remap_components g

let print_nullcomponenttable unit =
  let items = Nullcomponenttable.get_items nullcomponenttable in
  List.iter (fun (s,il) -> print_string (s^": "); List.iter (fun i -> print_string ((string_of_int i)^" ")) il; print_string "\n") items
