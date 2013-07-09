
let rec generate_one g nonterm = 
    let productions = List.filter (fun x -> Rule.get_nonterm x = nonterm) g in
    assert (productions <> []) ;
    let rule_selected =
        try
            Util.weighted_random (Util.map_tr (fun r -> (r, Rule.get_weight r)) productions)
        with
            Failure str -> Printf.eprintf "generate_one: Call to weighted_random failed for expansions of nonterminal %s\n" nonterm ;
            failwith str
    in
    match Rule.get_expansion rule_selected with
    | Rule.PublicTerminating str -> Derivation.make_derivation_tree nonterm [] rule_selected
    | Rule.PublicNonTerminating (nts, recipe) ->
        let child_trees = List.map (generate_one g) (Nelist.to_list nts) in
        Derivation.make_derivation_tree nonterm child_trees rule_selected

let generate num_trees grammar_file = 
  let (g,start_symbol) = Grammar.get_input_grammar grammar_file in
  let add_tree ts = (generate_one g start_symbol) :: ts in
  let sample = List.fold_left (fun trees n -> if (n<1) then trees else add_tree trees) [] (Util.range 0 300) in   (* magic number: 300 samples *)
  let eq t1 t2 = (Derivation.compare_derivations compare t1 t2 = 0) in
  let sort_trees = List.sort (Derivation.compare_derivations compare) in
  let rec uniques_sorted =  (* eliminates duplicates, assuming that duplicates are together (e.g. that the list is sorted) *)
    function [] -> []
           | [x] -> [x]
           | x::y::rest -> if (eq x y) then uniques_sorted (y::rest) else x::(uniques_sorted (y::rest))
  in
  Util.take num_trees (uniques_sorted (sort_trees sample))

