(* Zhong Chen, Sep 20, 2011,
a radom tree generator based on WMCFG format (currently ignoring string_yield info) 
Tree type and drawing were borrowed from LIN4424 course material by John Hale
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
let rec generate_all g nonterm w = 
    let productions = List.filter (fun x -> Rule.get_nonterm x = nonterm) g in
    assert (productions <> []) ;
    let rule_selected =
        try
            Util.weighted_random (Util.map_tr (fun r -> (r, Rule.get_weight r)) productions)
        with
            Failure str -> Printf.eprintf "generate_all: Call to weighted_random failed for expansions of nonterminal %s\n" nonterm ;
            failwith str
    in
    let current_w = Util.mult_weights (Rule.get_weight rule_selected) w in
    match Rule.get_expansion rule_selected with
    | Rule.PublicTerminating str -> (NonLeaf (nonterm, [Leaf str], rule_selected), current_w)
    | Rule.PublicNonTerminating (nts, recipe) -> (
        match (Nelist.to_list nts) with
        | [x] ->
            let child = generate_all g x current_w in
            (NonLeaf (nonterm, [fst child], rule_selected), snd child)
        | [x;y] ->
            let left_child = generate_all g x current_w in
            let right_child = generate_all g y (snd left_child) in
            (NonLeaf (nonterm, [fst left_child; fst right_child], rule_selected), snd right_child)
        | _ -> failwith "generate_all: right-hand-side has neither one nor two nonterminals"
    )

   (*sorting a list of trees in a decending order on their weights*)
   let sort_trees treelist = 
     let treecompare t1 t2 = -(Util.compare_weights (snd t1) (snd t2)) in
       List.sort treecompare treelist

let generate grammar_file = 
  let (g,start_symbol) = Grammar.get_input_grammar grammar_file in
  let rec add_n g n =
    if n < 1 then []
    else (generate_all g start_symbol Util.weight_one)::(add_n g (n-1))
  in
  let ntrees = add_n g 300 in (* magic number: 300 samples *)
  let eq (t1,w1) (t2,w2) = (t1 = t2) && (Util.compare_weights w1 w2 = 0) in
  sort_trees (Util.uniques ~eq:eq ntrees)

