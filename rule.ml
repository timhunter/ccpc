open Util

    type component = Component of int * int
    type stringrecipe = component Nelist.t
    type tuplerecipe = stringrecipe Nelist.t
    type expansion = PublicTerminating of string | PublicNonTerminating of (string Nelist.t * tuplerecipe)    
    type r = Terminating of (string * string * weight) | NonTerminating of (string * string Nelist.t * tuplerecipe * weight)

(**********************************************************************************)

let rec situate nonterm ranges =
  let helper range =
    match Fsa.get_consumed_span range with
    | Some (p,q) -> Printf.sprintf "_%d-%d" (Fsa.index_of p) (Fsa.index_of q)
    | None       -> Printf.sprintf "_eps"
  in
  List.fold_left (^) nonterm (map_tr helper ranges)

(* This function is the ``inverse'' (sort of) of situate above.
   Must be kept in sync if that changes. *)
let desituate nonterm =
	let regex = Str.regexp "\\(_[0-9]+-[0-9]+\\|_eps\\)*$" in
	let idx = Str.search_forward regex nonterm 0 in
	Str.string_before nonterm idx

let desituate_rule rule =
    match rule with
    | Terminating (nonterm, str, weight) -> Terminating (desituate nonterm, str, weight)
    | NonTerminating (left, rights, recipes, weight) -> NonTerminating (desituate left, Nelist.map desituate rights, recipes, weight)

(**********************************************************************************)

    type marked_mg_rule = LeftAdjunction | RightAdjunction

    let get_marked_mg_rule dict rule =
        match (desituate_rule rule) with
        | Terminating _ -> None
        | NonTerminating (left, rights, trecipe, _) ->
            (* See if this is an adjunction rule, i.e. of the form A -> B C where A and C map to the same feature sequence, 
               modulo the distinction between ":" (non-lexical) and "::" (lexical) on C. *)
            let is_adjunct =
                try (
                    let left_features = Hashtbl.find dict left in
                    let right1_features = Hashtbl.find dict (Nelist.nth rights 1) in
                    let ignore_lexical fs = Str.replace_first (Str.regexp "^:: ") ": " fs in
                    (Nelist.length rights = 2) && (left_features = ignore_lexical right1_features)
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

    let create_component i j = Component (i,j)

    let add_to_recipe lst recipe =
      let component = Nelist.from_list lst in 
      Nelist.from_list (component::(Nelist.to_list recipe))
    
    (**********************************************************)

    (* How many nonterminals does this rule put together? *)
    let rule_arity rule =
      match rule with
      | Terminating _ -> 0
      | NonTerminating (left, rights, _, _) -> Nelist.length rights

    let get_nonterm rule =
      match rule with
      | Terminating (nt, _, _) -> nt
      | NonTerminating (nt, _, _, _) -> nt

    let get_weight rule =
      match rule with 
        Terminating (_,_,w) -> w
      | NonTerminating (_,_,_,w) -> w

    let reweight rule w =
      match rule with
      | Terminating (nt, str, _) -> Terminating (nt, str, w)
      | NonTerminating (left, rights, recipes, _) -> NonTerminating (left, rights, recipes, w)

    let get_expansion rule =
      match rule with
      | Terminating (_, str, weight) -> PublicTerminating str
      | NonTerminating (_, rights, recipes, weight) -> PublicNonTerminating (rights, recipes)

    (**********************************************************)

    let component_to_string comp =
      match comp with
        | Component (a,b) -> Printf.sprintf "%d,%d" a b

    let stringrecipe_to_string lst =
      let component_strings = List.map component_to_string (Nelist.to_list lst) in
      "[" ^ (String.concat ";" component_strings) ^ "]"

    let to_string rule =
      let left = get_nonterm rule in
      let rhs_output = 
        match (get_expansion rule) with
        | PublicTerminating s -> "\""^s^"\"" (* Printf.sprintf "%S" s preserve French accented chars *)
        | PublicNonTerminating (rights, _) -> List.fold_left (^^) "" (Nelist.to_list rights)
     in
      let recipe =
        match (get_expansion rule) with
          | PublicTerminating s -> []
          | PublicNonTerminating (_,recs) -> List.map stringrecipe_to_string (Nelist.to_list recs) in
      let recipe = String.concat "" recipe in 
      Printf.sprintf "%s      %s --> %s %s" (show_weight (get_weight rule)) left rhs_output recipe

    (**********************************************************)

    let create_terminating (nonterm, term, weight) = Terminating (nonterm, term, weight)

    (* TODO: When we get a rule that has a component (i,j) in its recipe, i.e. referring to the j-th 
       element of the tuple that is the yield of the i-th nonterminal on the RHS, we do proper 
       validation on i, but not yet on j. Validating i is easy because it only depends on the number of 
       nonterminals on the RHS of this particular rule. Validating j requires more work because it depends 
       on the rank of the relevant nonterminal; at the moment, nothing even enforces that a nonterminal 
       has a unique consistent rank across all rules. *)
    let create_nonterminating (nonterm, rights, recipes, weight) =
        let nonempty_rights =
            try Nelist.from_list rights
            with Nelist.EmptyListException -> failwith("Nonterminating rule must have nonterminals to expand to")
        in
        let component_is_valid (Component(i,_)) = (0 <= i) && (i < List.length rights) in
        let stringrecipe_is_valid = Nelist.for_all component_is_valid in
        if (Nelist.for_all stringrecipe_is_valid recipes) then
            NonTerminating (nonterm, nonempty_rights, recipes, weight)
        else
            let rule_str = Printf.sprintf "%s --> %s" nonterm (String.concat " " rights) in
            let recipe_str = String.concat "" (List.map stringrecipe_to_string (Nelist.to_list recipes)) in
            failwith (Printf.sprintf "Rule does not have enough nonterminals on the right-hand side for this recipe:   %s   %s" rule_str recipe_str)


