module type HYPERGRAPH =
        sig
                type v
                type g
                val compare : v -> v -> int
                val show : v -> string
                val tails : g -> v -> (v list * Rule.r * Util.weight) list
                val sort_derivations : (v Derivation.derivation_tree) list -> (v Derivation.derivation_tree) list
        end

(* This implements the HYPERGRAPH signature, but we don't declare it as such here 
 * in order to allow other code to know that Graph.g is Chart.chart, etc. *)
module GrammarAsGraph =
        struct
                type v = string   (* nonterminals *)
                type g = Rule.r list
                let compare = Pervasives.compare   (* fine for strings *)
                let show nonterm = nonterm
                let tails rules nonterm =
                        let relevant_rules = List.filter (fun r -> Rule.get_nonterm r = nonterm) rules in
                        let get_children r = match (Rule.get_expansion r) with
                                             | Rule.PublicTerminating _ -> []
                                             | Rule.PublicNonTerminating(xs,_) -> Nelist.to_list xs
                        in
                        Util.map_tr (fun r -> (get_children r, r, Rule.get_weight r)) relevant_rules
                let sort_derivations = List.sort (Derivation.compare_derivations compare)
        end

(*********************************************************************************************************)

(* Get single best derivation, without tracking history *)
module Algo1 = functor (G : HYPERGRAPH) -> struct

        (* Here's what happens in the case of exactly two children:
        let get_best_derivation_by_route g v (children,rule,wt) =
                let [left_child; right_child] = children in
                let left_best  = get_best_derivation g left_child  in
                let right_best = get_best_derivation g right_child in
                Derivation.make_derivation_tree v [left_best; right_best] rule wt
        *)

        let rec get_best_derivation_by_route g v (children,rule,wt) =

                (* Get the best derivation of each child nonterminal *)
                let child_derivations = List.map (get_best_derivation g) children in

                (* Put those derivations together as sisters *)
                Derivation.make_derivation_tree v child_derivations rule wt

        and get_best_derivation g v =
                let routes = G.tails g v in
                let candidates = List.map (fun r -> get_best_derivation_by_route g v r) routes in
                let result = match (G.sort_derivations candidates) with
                             | [] -> failwith "Oh dear! Not a single candidate!"
                             | (x::_) -> x
                in result

end

(*********************************************************************************************************)

(* Get single best derivation, tracking history as we go *)
module Algo2 = functor (G : HYPERGRAPH) -> struct

        let empty_visit_history = []
        let add_to_history x h = x::h
        let is_in_history h x = List.mem x h

        let rec get_best_derivation_by_route history g v (children,rule,wt) =
                if (List.exists (is_in_history history) children) then
                        None
                else
                        let child_derivations = List.map (get_best_derivation' history g) children in
                        Some (Derivation.make_derivation_tree v child_derivations rule wt)

        and get_best_derivation' history g v =
                let routes = G.tails g v in
                let new_history = add_to_history v history in
                let candidates = Util.optlistmap (get_best_derivation_by_route new_history g v) routes in
                let result = match (G.sort_derivations candidates) with
                             | [] -> failwith "Oh dear! Not a single candidate!"
                             | (x::_) -> x
                in result

        let get_best_derivation g v =
                get_best_derivation' empty_visit_history g v

end

(*********************************************************************************************************)

(* Get n best derivations, without tracking history *)
module Algo3 = functor (G : HYPERGRAPH) -> struct

        let rec one_from_each (lists : 'a list list) : ('a list list) =
                let prepend_one_of xs ys = List.map (fun x -> x::ys) xs in
                match lists with
                | [] -> [[]]
                | (l::ls) -> List.concat (List.map (prepend_one_of l) (one_from_each ls))

        let rec get_n_best_derivations_by_route n g v (children,rule,wt) =

                (* Get the list of n best derivations, for each child nonterminal *)
                (* i.e. a list of lists of derivations *)
                let child_derivation_lists = List.map (get_n_best_derivations n g) children in

                (* Combine those in all possible ways *)
                let child_combinations = one_from_each child_derivation_lists in
                let derivations = List.map (fun cs -> Derivation.make_derivation_tree v cs rule wt) child_combinations in

                (* Take the best n *)
                Util.take n derivations

        and get_n_best_derivations n g v =
                let routes = G.tails g v in
                let candidates = List.concat (List.map (get_n_best_derivations_by_route n g v) routes) in
                Util.take n candidates

end

(*********************************************************************************************************)

(* Get n-best derivations, tracking history as we go *)
module Algo4 = functor (G : HYPERGRAPH) -> struct

        let empty_visit_history = []
        let add_to_history x h = x::h
        let show_history h = Util.show_list (fun (v,i) -> Printf.sprintf "(%s,%d)" (G.show v) i) h
        let ok_to_visit (i,v) h = List.exists (fun (x,y) -> (v = x) && (i >= y)) h

        let rec require_no_nones (lst : 'a option list) : 'a list option =
                match lst with
                | [] -> Some []
                | (None :: xs) -> None
                | ((Some x) :: xs) -> match (require_no_nones xs) with | None -> None | Some rest -> Some (x::rest)

        let rec guarded_get_nth g' new_h (i,it) =
                if (ok_to_visit (i,it) new_h) then
                        None
                else
                        get_nth_best_derivation new_h i g' it

        and get_n_best_derivations_by_route history n g v ((children,r,wt) : (G.v list * Rule.r * Util.weight)) =
                match children with
                | [] -> [Derivation.make_derivation_tree v [] r wt]    (* if v is an axiom, there's only one derivation *)
                | _ ->
                        let route_arity = List.length children in        (* r = |e| *)
                        let rank_vectors = Util.all_lists (Util.range 1 (n+1)) route_arity in
                        let zipped_vectors : (int * G.v) list list = Util.map_tr (fun vec -> List.combine vec children) rank_vectors in

                        (* child_lists maps a 'recipe vector' into a list of maybe-derivation-trees *)
                        let child_list : (int * G.v) list -> (G.v Derivation.derivation_tree option) list =
                                let new_history = add_to_history (v,n) history in
                                Util.map_tr (guarded_get_nth g new_history)
                        in

                        let all_child_lists : (G.v Derivation.derivation_tree option) list list
                                            = Util.map_tr child_list zipped_vectors
                        in
                        let all_complete_child_lists : (G.v Derivation.derivation_tree) list list
                                                     = Util.optlistmap require_no_nones all_child_lists
                        in
                        let derivations : (G.v Derivation.derivation_tree) list
                                        = Util.map_tr (fun children -> Derivation.make_derivation_tree v children r wt) all_complete_child_lists
                        in

                        let sorted_derivations = G.sort_derivations derivations in
                        Util.take n sorted_derivations

        and get_nth_best_derivation history n g v =
                assert (n >= 1) ;
                let routes = G.tails g v in
                let candidates = List.concat (List.map (get_n_best_derivations_by_route history n g v) routes) in
                if (List.length candidates >= n) then (
                        let sorted_candidates = G.sort_derivations candidates in
                        Some (List.nth sorted_candidates (n-1))
                ) else
                        None

        let get_n_best_derivations n g v =
                let ranks = Util.range 1 (n+1) in   (* the list [1..n] *)
                let rec throw_away_nones = function [] -> [] | (None::xs) -> throw_away_nones xs | (Some(x)::xs) -> x::(throw_away_nones xs) in
                throw_away_nones (List.map (fun i -> get_nth_best_derivation empty_visit_history i g v) ranks)

end

(*********************************************************************************************************)

module A1 = Algo1(GrammarAsGraph)
module A2 = Algo2(GrammarAsGraph)
module A3 = Algo3(GrammarAsGraph)
module A4 = Algo4(GrammarAsGraph)

let main () =

        let grammar_file = Sys.argv.(1) in
        let n = int_of_string (Sys.argv.(2)) in
        let (rules, start_symbol) = Grammar.get_input_grammar grammar_file in

        (* Using Algo1 *)
        let run1 () = 
                print_endline "===== Using Algo1 =====" ;
                let d = A1.get_best_derivation rules start_symbol in
                print_endline (Derivation.print_tree_compact d) ;
                print_endline "======================="
        in

        (* Using Algo2 *)
        let run2 () =
                print_endline "===== Using Algo2 =====" ;
                let d = A2.get_best_derivation rules start_symbol in
                print_endline (Derivation.print_tree_compact d) ;
                print_endline "======================="
        in

        (* Using Algo3 *)
        let run3 () =
                print_endline "===== Using Algo3 =====" ;
                let ds = A3.get_n_best_derivations n rules start_symbol in
                List.iter (fun d -> print_endline (Derivation.print_tree_compact d)) ds ;
                print_endline "======================="
        in

        (* Using Algo4 *)
        let run4 () =
                print_endline "===== Using Algo4 =====" ;
                let ds = A4.get_n_best_derivations n rules start_symbol in
                List.iter (fun d -> print_endline (Derivation.print_tree_compact d)) ds ;
                print_endline "======================="
        in

        (* Using the real thing *)
        let run_real () =
                print_endline "===== The 'real thing' =====" ;
                let ds = Derivation.get_n_best_from_grammar n rules start_symbol in
                List.iter (fun d -> print_endline (Derivation.print_tree_compact d)) ds ;
                print_endline "======================="
        in

        run2 () ;
        run4 () ;
        run_real ()

let _ = if (!Sys.interactive) then () else main ()

