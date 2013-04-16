open Util
open Num

    type component = Component of int * int | Epsilon
    type stringrecipe = component Nelist.t
    type tuplerecipe = stringrecipe Nelist.t
    type expansion = PublicTerminating of string | PublicNonTerminating of (string Nelist.t * tuplerecipe)    
    type r = Terminating of (string * string * weight) | NonTerminating of (string * string Nelist.t * tuplerecipe * weight)

    (**********************************************************)

    (* Certain MCFG rules correspond to special ``marked'' MG rules; MG rules that aren't the normal 
       merge and move rules. These rules need to be indicated in the ``derivation string'' that we 
       construct in visualise.ml to pass to Stabler's prolog code. *)
    type marked_mg_rule = LeftAdjunction | RightAdjunction

    (* Given an MCFG rule, gives back the corresponding marked MG rule if there is one.
       NB: May not give the right results if you pass it a situated grammar rule! If you need to check a 
       situated rule, use Grammar.desituate_rule first. *)
    let get_marked_mg_rule dict rule =
        match rule with
        | Terminating _ -> None
        | NonTerminating (left, rights, trecipe, _) ->
            let is_adjunct =   (* See if this is an adjunction rule, i.e. of the form A -> B C where A and C map to the same feature sequence *)
                try (
                    let left_features = Hashtbl.find dict left in
                    let right1_features = Hashtbl.find dict (Nelist.nth rights 1) in
                    (Nelist.length rights = 2) && (left_features = right1_features)
                ) with _ -> false
            in
            if is_adjunct then (
                (* It might (must?) be an adjunction rule. Now ... HACK! ... check the tuple recipe to see which direction. *)
                (* We only look at the first three string recipes because these look after the head, comp and spec of the phrase; 
                   we ignore the rest, which look after any ``moving sub-parts'', since these are unaffected by adjunction. *)
                let stringrecipes = take 3 (List.map Nelist.to_list (Nelist.to_list trecipe)) in
                if stringrecipes = [ [Component(0,0);Component(0,1);Component(0,2);Component(1,0)]; [Component(1,1)]; [Component(1,2)] ] then
                    Some LeftAdjunction
                else if stringrecipes = [ [Component(1,0)]; [Component(1,1)]; [Component(1,2);Component(0,0);Component(0,1);Component(0,2)] ] then
                    Some RightAdjunction
                else
                    None   (* Not sure if this should ever happen, actually *)
            ) else (
                None
            )

    (**********************************************************)

    let getstr yields pair =
      match pair with 
        | Component (i,j) ->
              let relevant_yield = List.nth yields i in
             (List.nth relevant_yield j)
        | Epsilon -> failwith "Don't know what this case is meant to be; not used as far as I can tell (TH, 12-Aug-2011)"

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

   let get_rhs rule =
      match rule with
      | Terminating (_,rhs, _) -> rhs :: []
      | NonTerminating (_,rhs, _, _) -> Nelist.to_list rhs

    let get_weight rule =
      match rule with 
        Terminating (_,_,w) -> w
      | NonTerminating (_,_,_,w) -> w

    let get_expansion rule =
      match rule with
      | Terminating (_, str, weight) -> PublicTerminating str
      | NonTerminating (_, rights, recipes, weight) -> PublicNonTerminating (rights, recipes)

    let get_recipe rule =
      match rule with 
          Terminating _ -> failwith ("Terminating rule, no recipe")
        | NonTerminating (_,_,rcp,_) -> rcp
      
    let get_indices comp = 
      match comp with
          Component (a,b) -> (a,b)
        | Epsilon -> failwith "This case should never be encountered"

    (**********************************************************)

    (* transforms a rule by modifying every nonterminal mentioned in the rule (and nothing else) *)
    let map_nonterms f rule =
      match rule with
      | Terminating (nonterm, str, weight) -> Terminating (f nonterm, str, weight)
      | NonTerminating (left, rights, recipes, weight) -> NonTerminating (f left, Nelist.map f rights, recipes, weight)

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
      Printf.sprintf "%s      %s --> %s %s" (show_weight (get_weight rule)) left rhs_output recipe