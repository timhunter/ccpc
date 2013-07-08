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
let rec generate_all g nonterm = 
    let productions = List.filter (fun x -> Rule.get_nonterm x = nonterm) g in
    assert (productions <> []) ;
    let rule_selected =
        try
            Util.weighted_random (Util.map_tr (fun r -> (r, Rule.get_weight r)) productions)
        with
            Failure str -> Printf.eprintf "generate_all: Call to weighted_random failed for expansions of nonterminal %s\n" nonterm ;
            failwith str
    in
    match Rule.get_expansion rule_selected with
    | Rule.PublicTerminating str -> Derivation.make_derivation_tree nonterm [] rule_selected
    | Rule.PublicNonTerminating (nts, recipe) ->
        let child_trees = List.map (generate_all g) (Nelist.to_list nts) in
        Derivation.make_derivation_tree nonterm child_trees rule_selected

let generate grammar_file = 
  let (g,start_symbol) = Grammar.get_input_grammar grammar_file in
  let rec add_n g n =
    if n < 1 then []
    else (generate_all g start_symbol)::(add_n g (n-1))
  in
  let ntrees = add_n g 300 in (* magic number: 300 samples *)
  let eq t1 t2 = (Derivation.compare_derivations compare t1 t2 = 0) in
  let sort_trees = List.sort (Derivation.compare_derivations compare) in
  List.map (fun t -> (t, Derivation.get_weight t)) (sort_trees (Util.uniques ~eq:eq ntrees))

