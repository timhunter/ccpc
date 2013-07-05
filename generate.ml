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

   let rec get_sentence t = 
     let rec get' t = 
       match t with 
           Leaf label -> [[label]]
         | NonLeaf (label,children,r) -> 
           let yields = List.map (fun child -> (get' child)) children in
           match (Rule.get_expansion r) with 
               Rule.PublicTerminating _ -> List.flatten (yields)
             | Rule.PublicNonTerminating _ ->
               let recipe = Rule.get_recipe r in
               (Rule.apply recipe (yields) List.append) in 
     List.filter (fun item -> (String.contains item ' ') == false) (List.flatten (get' t))

   let write_tree t fname =
     let oc = open_out (fname^".dot") in
     let str = "digraph "^fname^" {\n node [shape = plaintext]; \n edge [arrowhead = none]; \n"^
       (Pervasives.fst (dot_of_tree 0 t))^"}" in
       begin
	 output_string oc str;
	 close_out oc
       end


(* generate a random tree and its weight*)
let rec generate_all g items w = 
    let nonterm = List.map Rule.get_nonterm g in
	if List.mem (List.hd items) nonterm then
	  let productions = List.filter (fun x -> Rule.get_nonterm x = (List.hd items)) g in
	  let rule_selected =
	    try
            Util.weighted_random (Util.map_tr (fun r -> (r, Rule.get_weight r)) productions)
        with
            Failure str -> Printf.eprintf "generate_all: Call to weighted_random failed for expansions of nonterminal %s\n" (List.hd items) ;
            failwith str
      in
	  let rule_selected_rhs = Rule.get_rhs rule_selected in
	  let current_w = 
	    let rule_weight = Rule.get_weight rule_selected in
	    Util.mult_weights rule_weight w in
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
     let treecompare t1 t2 = -(Util.compare_weights (snd t1) (snd t2)) in
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
  let ntrees = add_n g items Util.weight_one 300 in (* magic number: 300 samples *)
    sort_trees (remove_duplicate ntrees)

