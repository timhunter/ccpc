open Util

module type MULTISET = sig
    type t
    val empty : unit -> t
    val count : t -> Rule.r -> int
    val add_one : t -> Rule.r -> unit
    val iter : (Rule.r -> unit) -> t -> unit
    val intersect : t -> t -> t
    val difference : t -> t -> t
    val print : t -> unit
end

module MyMultiset : MULTISET = struct
    type t = (Rule.r, int) Hashtbl.t
    let empty () = Hashtbl.create 20
    let count tbl x = try Hashtbl.find tbl x with Not_found -> 0
    let add_one tbl x = Hashtbl.replace tbl x ((count tbl x) + 1)
    let iter f tbl = Hashtbl.iter (fun k v -> for i=1 to v do f k done) tbl
    let keywise f tbl1 tbl2 = let result = empty () in
                              Hashtbl.iter (fun k v -> Hashtbl.replace result k (f v (count tbl2 k))) tbl1 ;
                              result
    let intersect = keywise min
    let difference = keywise (-)
    let print = iter (fun r -> Printf.printf "    %s\n" (Rule.to_string r))
end


let rules_of_derivation derivation =
    let rec rule_list t =
        let root_rule = Derivation.get_rule t in
        let child_trees = Derivation.get_children t in
        let subsequent_rule_lists = List.map rule_list child_trees in
        root_rule :: (List.concat subsequent_rule_lists)
    in
    let result = MyMultiset.empty () in
    List.iter (MyMultiset.add_one result) (rule_list derivation) ;
    result

let get_derivations (rules,start_symbol) derivation_ranks =
        let max_rank = List.fold_left (fun x y -> if (x > y) then x else y) 0 derivation_ranks in
        let derivation_trees = Derivation.get_n_best_from_grammar max_rank rules start_symbol in
        List.map (fun i -> (i, List.nth derivation_trees (i-1))) derivation_ranks

let pretty_print_rule dict r =
        let process_nonterm nt =
            match dict with
            | None -> nt
            | Some tbl -> try Printf.sprintf "(%s)" (Hashtbl.find tbl (Grammar.desituate nt))
                          with Not_found -> nt
        in
        let lhs = process_nonterm (Rule.get_nonterm r) in
        let rhs = List.map process_nonterm (match (Rule.get_expansion r) with
                                            | Rule.PublicTerminating str -> [str]
                                            | Rule.PublicNonTerminating (nts,_) -> Nelist.to_list nts)
        in
        let weight_str = show_weight_float (Rule.get_weight r) in
        Printf.sprintf "%s\t%s --> %s" weight_str lhs (String.concat " " rhs)

let compare_derivations (rules,start_symbol) dict derivation_ranks =
        let derivations = get_derivations (rules,start_symbol) derivation_ranks in
        print_endline "========================================================" ;
        List.iter (fun (i,t) -> Printf.printf "Derivation #%d: %s\n" i (Derivation.print_tree_compact t)) derivations ;
        print_endline "========================================================" ;
        let rule_multisets = List.map (fun (i,t) -> (i, rules_of_derivation t)) derivations in
        let common_rules = match (List.map snd rule_multisets) with
                           | (hd::tl) -> List.fold_left MyMultiset.intersect hd tl
                           | _ -> assert false
        in
        let show_additional_rules (i,rs) =
            print_endline "========================================================" ;
            Printf.printf "Additional rules for Derivation #%d:\n" i ;
            let rules = MyMultiset.difference rs common_rules in
            (* MyMultiset.iter (fun r -> if compare_weights (Rule.get_weight r) (weight_from_float 1.0) <> 0 then Printf.printf "    %s\n" (Rule.to_string r)) rules ; *)
            MyMultiset.iter (fun r -> if compare_weights (Rule.get_weight r) (weight_from_float 1.0) <> 0 then Printf.printf "    %s\n" (pretty_print_rule dict r)) rules ;
            print_endline "========================================================"
        in
        List.iter show_additional_rules rule_multisets

let set_rank_list ranks_ref str =
    try
        let n = int_of_string str in
        if (n < 1) then (raise (Failure "Negative ranks not allowed")) ;
        ranks_ref := n::(!ranks_ref) ;
    with Failure _ -> Printf.eprintf "WARNING: Ignoring bad command-line argument (not a valid rank): '%s'\n" str

let main () =
    let grammar_file = ref "" in
    let dict_file = ref "" in
    let ranks = ref [] in
    let speclist = [("-g", Arg.Set_string(grammar_file), "wmcfg grammar file (obligatory)");
                    ("-d", Arg.Set_string(dict_file),    "dict file (optional)")] in
    let usage_msg = "" in
    Arg.parse speclist (set_rank_list ranks) usage_msg ;
    if (!grammar_file = "") then (
        Printf.eprintf "Must provide a grammar file\n" ;
        Arg.usage speclist usage_msg
    ) else if (List.length !ranks < 2) then (
        Printf.eprintf "Not enough derivations to compare, I'm outta here\n"
    ) else (
        let grammar = Grammar.get_input_grammar !grammar_file in
        let dict = match (!dict_file) with "" -> None | s -> Some (Grammar.get_guillaumin_dict s) in
        compare_derivations grammar dict (reverse_tr !ranks)
    )

let _ = if (!Sys.interactive) then () else main ()


