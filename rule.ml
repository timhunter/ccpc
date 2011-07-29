open Util
open Rational

    type component = Component of int * int | Epsilon
    type stringrecipe = component Nelist.t
    type tuplerecipe = stringrecipe Nelist.t
    type expansion = PublicTerminating of string | PublicNonTerminating of (string Nelist.t * tuplerecipe)
    type r = Terminating of (string * string * (Rational.rat option)) | NonTerminating of (string * string Nelist.t * tuplerecipe * (Rational.rat option)) 

    (**********************************************************)

    let getstr yields pair =
      match pair with 
        | Component (i,j) ->
              let relevant_yield = List.nth yields i in
             (List.nth relevant_yield j)
        | Epsilon -> (EpsVar, EpsVar) 

    let makestr list_of_pairs yields concat =
      let pieces_to_concatenate = Nelist.map (getstr yields) list_of_pairs in
      (Nelist.fold concat pieces_to_concatenate)

    let apply srecipes yields concat =
      let res = Nelist.to_list (Nelist.map (fun sf -> makestr sf yields concat) srecipes) in 
    (*  let r = List.fold_left (fun acc x -> match x with 
                                             None -> acc
                                             | Some a -> (a::acc)) [] res in *)
      res

    let create_tuplerecipe lst =
      Nelist.from_list [(Nelist.from_list lst)]

    let add_to_recipe lst recipe =
      let component = Nelist.from_list lst in 
      Nelist.from_list (component::(Nelist.to_list recipe))
    
    (**********************************************************)

    (* Input to these creation functions should be aggressively verified *)

    let create_terminating (nonterm, term, weight) = Terminating (nonterm, term, weight)

    (* TODO: Still need to make sure the ``rights'' and the ``recipes'' are compatible *)
    let create_nonterminating (nonterm, rights, recipes, weight) =
      let checked_rights =
        try Nelist.from_list rights
        with EmptyListException -> failwith("Nonterminating rule must have nonterminals to expand to") in
      let checked_recipes =
        try Nelist.from_list (List.map Nelist.from_list recipes)
        with EmptyListException -> failwith("Nonterminating rule must have a function for composing yields") in
      NonTerminating (nonterm, checked_rights, checked_recipes, weight)

    let create_rule (nonterm, rights, srecipes, weight) = create_nonterminating (nonterm, rights, List.map Nelist.to_list (Nelist.to_list srecipes), weight)

    (**********************************************************)

    (* How many nonterminals does this rule put together? *)
    let rule_arity rule =
      match rule with
      | Terminating _ -> 0
      | NonTerminating (left, rights, _, _) -> Nelist.length rights
    
    let max_arity rules = List.fold_left max 0 (map_tr rule_arity rules)

    (* How many components does the tuple produced by this rule have? *)
    let nonterm_degree rule =
      match rule with
      | Terminating _ -> 1
      | NonTerminating (left, rights, recipes, weight) -> Nelist.length recipes

    let get_nonterm rule =
      match rule with
      | Terminating (nt, _, _) -> nt
      | NonTerminating (nt, _, _, _) -> nt

    let get_weight rule =
      match rule with 
        Terminating (_,_,w) -> w
      | NonTerminating (_,_,_,w) -> w

    let get_expansion rule =
      match rule with
      | Terminating (_, str, weight) -> PublicTerminating str
      | NonTerminating (_, rights, recipes, weight) -> PublicNonTerminating (rights, recipes)

    (**********************************************************)

    let component_to_string comp =
      match comp with
        | Component (a,b) -> Printf.sprintf "%d,%d" a b
        | Epsilon         -> "eps"

    let stringrecipe_to_string lst =
      let component_strings = List.map component_to_string (Nelist.to_list lst) in
      "[" ^ (String.concat ";" component_strings) ^ "]"

    let to_string rule =
      let left = get_nonterm rule in
      let rhs_output = 
        match (get_expansion rule) with
        | PublicTerminating s -> Printf.sprintf "%S" s
        | PublicNonTerminating (rights, _) -> List.fold_left (^^) "" (Nelist.to_list rights)
      in
      let recipe =
        match (get_expansion rule) with
          | PublicTerminating s -> []
          | PublicNonTerminating (_,recs) -> List.map stringrecipe_to_string (Nelist.to_list recs) in
      let recipe = String.concat "" recipe in 
      let weight_str =
        match (get_weight rule) with
        | Some (w1,w2) -> Printf.sprintf "%d / %d     " w1 w2
        | None -> ""
      in
      Printf.sprintf "%s %s --> %s %s" weight_str left rhs_output recipe


