(* Zhong Chen, Sep 20, 2011,
a radom tree generator based on WMCFG format (currently ignoring string_yield info) 
Tree type and drawing were borrowed from LIN4424 course material by John Hale
added function get_rhs in rule.ml and updated rule.mli
*)

(*
generate a random Korean tree and its corresponding weight:
let (random_korean_tree, weight) = generate "grammars/wmcfg/korean.wmcfg";;

or a Chinese tree
let (random_chinese_tree, weight) = generate "grammars/wmcfg/chinese.wmcfg";;

and draw it in a .dot file:
write_tree random_korean_tree "random_korean_tree";;
*)

open Num

   (* trees are a typical kind of value for parsers to return *)
type 'a tree = Leaf of 'a | NonLeaf of ('a * 'a tree list * Rule.r)   (* list should never be empty in the NonLeaf case *)

   (* AT&T dot I/O *)

   (* adapted from Maxime Amblard: dot needs each node to have a unique integer identifier *)
   (*   dot_of_tree  takes a starting node number i and a tree into a pair consisting of   *)
   (*   a dot-input string and the highest node number used in making that script          *)
   let rec dot_of_tree i = function
       Leaf label -> ("n"^(string_of_int i)^" [label = \""^label^"\"];\n" , i)
     | NonLeaf (label,children,_) -> 
	 let parent = "n"^(string_of_int i)^"[label = \""^label^"\"];\n" in
	 let (subtrees,maximum) = List.fold_left
	     (fun (prevt,oldi) t ->
	       let (subtreei1,highest) = dot_of_tree (oldi+1) t in
	       let branch = "n"^(string_of_int i)^"-> n"^(string_of_int (oldi+1))^";\n" in
	       (prevt^branch^subtreei1,highest)
	     )
	     ("",i)
	     children in
	 parent^subtrees,maximum


   let write_tree t fname =
     let oc = open_out (fname^".dot") in
     let str = "digraph "^fname^" {\n node [shape = plaintext]; \n edge [arrowhead = none]; \n"^
       (Pervasives.fst (dot_of_tree 0 t))^"}" in
       begin
	 output_string oc str;
	 close_out oc
       end


(* this function only generates a random tree
  let rec generate_all g items = 
    let nonterm = List.map Rule.get_nonterm g in
	if List.mem (List.hd items) nonterm then
	  let productions = List.filter (fun x -> Rule.get_nonterm x = (List.hd items)) g in
	  let rule_selected = Rule.get_rhs (List.nth productions (Random.int (List.length productions))) in
	    if List.length rule_selected = 1 then Node ((List.hd items),[generate_all g rule_selected])
	    else Node ((List.hd items),[(generate_all g [List.hd rule_selected]);(generate_all g (List.tl rule_selected))])
	else Node ((List.hd items),[])
*)


   (*sorting a list of rules with same lfs symbol in a decending order on its weight*)
   let sort_rules rules = 
     let rulecompare r1 r2 =
       match ((Rule.get_weight r1),(Rule.get_weight r2)) with
	 | (Some (num1,denom1),Some (num2,denom2)) -> -(compare num1 num2)
	 | (None,_) -> 0 
	 | (_,None) -> 0 
     in
       List.sort rulecompare rules

let n_of num denom = div_num (num_of_int num) (num_of_int denom)

   (* getting the weight in int. for a rule*)
   let some_to_int weight = 
     match weight with
	 | Some (num,denom) -> (num,denom)
	 | None -> failwith "There's no weight here"
 
  (* weighted random sampling*)
  let weighted_random rules =
    if (List.length rules) = 1 then (List.hd rules) else
      let sorted_rules = sort_rules rules in
      let random = Random.int (snd (some_to_int (Rule.get_weight (List.hd sorted_rules)))+1) in
        let rec select rnd r =
	  if List.length r = 1 then (List.hd r) else 
	    let diff = rnd - fst (some_to_int (Rule.get_weight (List.hd r))) in
	      if diff < 0 then (List.hd r)	  
	      else select diff (List.tl r) 
	in
	  select random sorted_rules



(* generate a random tree and its weight*)
let rec generate_all g items w = 
    let nonterm = List.map Rule.get_nonterm g in
	if List.mem (List.hd items) nonterm then
	  let productions = List.filter (fun x -> Rule.get_nonterm x = (List.hd items)) g in
	  let rule_selected = weighted_random productions in
	  let rule_selected_rhs = Rule.get_rhs rule_selected in
	  let current_w = 
	    match (Rule.get_weight rule_selected) with
	      | Some (num,denom) -> mult_num (div_num (num_of_int num) (num_of_int denom)) w
	      | None -> w in
	    if List.length rule_selected_rhs = 1 then 
	      let child = generate_all g rule_selected_rhs current_w in
	      (NonLeaf (List.hd items, [fst child], rule_selected), snd child)
	    else 
	      let left_child = generate_all g [List.hd rule_selected_rhs] current_w in
	      let right_child = generate_all g (List.tl rule_selected_rhs) (snd left_child) in
	      (NonLeaf (List.hd items, [fst left_child; fst right_child], rule_selected), snd right_child)
	else (Leaf (List.hd items), w)

   (*sorting a list of trees in a decending order on their weights*)
   let sort_trees treelist = 
     let treecompare t1 t2 = -(Num.compare_num (snd t1) (snd t2)) in
       List.sort treecompare treelist

(* remove the duplicate trees in a tree list*)
let remove_duplicate list = 
  let rec fstlist l = 
     match l with
       |[] -> []
       |h::t -> (fst h)::fstlist t in
  let add_elem elem l =
    if List.mem (fst elem) (fstlist l) then l 
    else elem :: l
  in
    List.fold_right add_elem list []



let generate grammar_file = 
  let (g,start_symbol) = Grammar.get_input_grammar grammar_file in
  let items = [start_symbol] in
  let rec add_n g items w n =
    if n < 1 then []
    else (generate_all g items w)::(add_n g items w (n-1)) 
  in
  let ntrees = add_n g items (n_of 1 1) 300 in
    sort_trees (remove_duplicate ntrees)
