(* Zhong Chen, Sep 18, 2011,
a radom tree generator based on WMCFG format (currently ignoring string_yield and weights information) 
Tree type and drawing were borrowed from LIN4424 course material by John Hale
added function get_rhs in rule.ml and updated rule.mli
*)

(*
Make sure to run it on Caml toplevel
ocaml -I +ocamlgraph -I kbest -I mcfgread

An example:

generate a random tree in Chinese:
let random_Chinese_tree = generate "grammars/wmcfg/chinese.wmcfg" "S";;

and draw it in a .dot file:
write_tree random_Chinese_tree "random_chinese_tree";;


*)

#use "loading.ml";;

   (* trees are a typical kind of value for parsers to return *)
type 'a tree = Node of 'a * 'a tree list

   (* AT&T dot I/O *)

   (* adapted from Maxime Amblard: dot needs each node to have a unique integer identifier *)
   (*   dot_of_tree  takes a starting node number i and a tree into a pair consisting of   *)
   (*   a dot-input string and the highest node number used in making that script          *)
   let rec dot_of_tree i = function
       Node (label,[]) -> ("n"^(string_of_int i)^" [label = \""^label^"\"];\n" , i)
     | Node (label,children) -> 
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


let rec generate_all g items = 
    let nonterm = List.map Rule.get_nonterm g in
	if List.mem (List.hd items) nonterm then
	  let productions = List.filter (fun x -> Rule.get_nonterm x = (List.hd items)) g in
	  let rule_selected = Rule.get_rhs (List.nth productions (Random.int (List.length productions))) in
	    if List.length rule_selected = 1 then Node ((List.hd items),[generate_all g rule_selected])
	    else Node ((List.hd items),[(generate_all g [List.hd rule_selected]);(generate_all g (List.tl rule_selected))])
	else Node ((List.hd items),[])

let generate grammar_file start =
  let g = Grammar.get_input_grammar grammar_file in
  let items = [start] in
    generate_all g items



(* need to calculate the weights*)
(* random should based on probs?*)
