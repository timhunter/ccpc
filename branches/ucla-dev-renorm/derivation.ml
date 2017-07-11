open Util

(* The weight stored with each derivation is the cumulative weight of the entire derivation.
 * The weight introduced by any individual step in the derivation can be determined from the corresponding rule. *)
type 'a derivation_tree = Leaf of 'a * Rule.r * weight | NonLeaf of 'a * ('a derivation_tree) list * Rule.r * weight

let get_children t =
	match t with
	| Leaf(_,_,_) -> []
	| NonLeaf(_,ts,_,_) -> ts

let get_root_item t =
	match t with
	| Leaf (i,_,_) -> i
	| NonLeaf (i,_,_,_) -> i

let get_weight t =
        match t with
        | Leaf (_,_,w) -> w
        | NonLeaf (_,_,_,w) -> w

let get_rule t =
        match t with
        | Leaf (_,r,_) -> r
        | NonLeaf (_,_,r,_) -> r

(* Compare derivations by weight; if they're equal by weight, back off and compare the other 
 * components. We don't want to return 0 in cases where the weights are equal, because 
 * this would mean that two distinct derivations could be considered equal for the purposes 
 * of sorting and k-best lists, which would destroy the one-to-one correspondence between 
 * derivation trees and what Huang & Chiang call 'dbp's. *)
let rec compare_derivations compare_vertices t1 t2 =
        let rec compare_lists f lst1 lst2 =
                match lst1,lst2 with
                | [],[] -> 0
                | [],_ -> -1
                | _,[] -> 1
                | (x::xs),(y::ys) -> (match (f x y) with 0 -> compare_lists f xs ys | other -> other)
        in
        let weight_result = compare_weights (get_weight t2) (get_weight t1) in
        if (weight_result <> 0) then
                weight_result
        else (
                let children_result = compare_lists (compare_derivations compare_vertices) (get_children t1) (get_children t2) in
                if (children_result <> 0) then
                        children_result
                else (
                        let item_result = compare_vertices (get_root_item t1) (get_root_item t2) in
                        if (item_result <> 0) then
                                item_result
                        else (
                                compare (Rule.to_string (get_rule t1)) (Rule.to_string (get_rule t2))
                        )
                )
        )

let make_derivation_tree item children rule =
	let weight_factor = Rule.get_weight rule in
	let product = List.fold_left (mult_weights) weight_factor (map_tr get_weight children) in
	match children with
	| [] -> Leaf (item, rule, product)
	| _ -> NonLeaf (item, children, rule, product)

let derived_string tree =
    (* argument to helper might not be a root tree, so it might return a non-singleton list of strings *)
    let rec helper t =
        match (t, Rule.get_expansion (get_rule t)) with
        | (Leaf _,                      Rule.PublicTerminating str) -> if str = " " then [""] else [str]
        | (NonLeaf (_, children, _, _), Rule.PublicNonTerminating (nts, recipe)) ->
            let subresults = map_tr helper children in
            (Rule.apply recipe subresults (^^))
        | _ -> failwith "derived_string: mismatch between tree structure and rules"
    in
    match (helper tree) with
    | [x] -> x
    | xs -> failwith (Printf.sprintf "derived_string: expected a one-tuple but got an %d-tuple: %s\n" (List.length xs) (Util.show_list (fun s -> s) xs))

let rec one_from_each (lists : 'a list list) : ('a list list) =
	let prepend_one_of xs ys = map_tr (fun x -> x::ys) xs in
	match lists with
	| [] -> [[]]
	| (l::ls) -> List.concat (map_tr (prepend_one_of l) (one_from_each ls))

let print_tree_compact tree =
        let rec print_tree' t =
                match (get_children t, Rule.get_expansion (get_rule t)) with
                | ([], Rule.PublicTerminating s) -> s
                | ([c], Rule.PublicNonTerminating _) -> print_tree' c
                | (cs, Rule.PublicNonTerminating _) -> "[" ^ (String.concat " " (map_tr print_tree' cs)) ^ "]"
                | _ -> failwith "Inconsistent tree in print_tree"
        in
        (show_weight_float (get_weight tree)) ^^ (print_tree' tree)

let print_tree_sexp f tree =
    let rec print' t =
        match (get_children t, Rule.get_expansion (get_rule t)) with
        | ([], Rule.PublicTerminating s) -> Printf.sprintf "(%s \"%s\")" (f (get_root_item t)) s
        | (cs, Rule.PublicNonTerminating _) -> "(" ^ (f (get_root_item t)) ^ " " ^  (String.concat " " (map_tr print' cs)) ^ ")"
        | _ -> failwith "Inconsistent tree in print_tree"
    in
    (print' tree)

(* Of the three print_tree functions here, this is really the only one 
 * that's even remotely human-readable. *)
let print_tree f tree =
	let rec print_tree' t =      (* returns a list of strings, each representing one line *)
		let item = get_root_item t in
		let children = get_children t in
		let yield =
			match (children, Rule.get_expansion (get_rule t)) with
			| ([]    , Rule.PublicTerminating s) -> Printf.sprintf "\"%s\"" s
			| ((_::_), Rule.PublicNonTerminating _) -> ""
			| _ -> failwith "Inconsistent tree in print_tree"
		in
		let first_line = (f item) ^^ yield ^^ (show_weight_float (get_weight t)) in
		let children_printed : (string list) = map_tr ((^) "    ") (List.concat (map_tr print_tree' children : (string list list))) in
		first_line :: children_printed
	in
	String.concat "\n" (print_tree' tree)

let rec get_derivations chart item =
        let routes = Chart.get_routes item chart in
        let children_lists antecedent_items = one_from_each (map_tr (get_derivations chart) antecedent_items) in
        let use_route (antecedents,rule) =
                map_tr (fun children -> (children,rule)) (children_lists antecedents) in
        let results_from_all_routes = List.concat (map_tr use_route routes) in
        map_tr (fun (children,rule) -> make_derivation_tree item children rule) results_from_all_routes

(**************************************************************************)
(****** Stuff for random generation ***************************************)

(* NB: This code could be moved inside the hypergraph functor we use for exact 
   k-best lists, to allow random generation from either a grammar or a chart. 
   It would probably be very easy, but I'm not going to bother right now. *)

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
    | Rule.PublicTerminating str -> make_derivation_tree nonterm [] rule_selected
    | Rule.PublicNonTerminating (nts, recipe) ->
        let child_trees = List.map (generate_one g) (Nelist.to_list nts) in
        make_derivation_tree nonterm child_trees rule_selected

(* NB: If the default value of num_samples changes, remember to adjust the ocamldoc comment in derivation.mli! *)
let generate ?(num_samples = 300) num_trees grammar_file = 
  let (g,start_symbol) = Grammar.get_input_grammar grammar_file in
  let sample = map_tr (fun _ -> generate_one g start_symbol) (Util.range 0 num_samples) in
  let eq t1 t2 = (compare_derivations compare t1 t2 = 0) in
  let sort_trees = List.sort (compare_derivations compare) in
  let rec uniques_sorted =  (* eliminates duplicates, assuming that duplicates are together (e.g. that the list is sorted) *)
    function [] -> []
           | [x] -> [x]
           | x::y::rest -> if (eq x y) then uniques_sorted (y::rest) else x::(uniques_sorted (y::rest))
  in
  Util.take num_trees (uniques_sorted (sort_trees sample))

(**************************************************************************)
(****** Stuff for computing k-best lists **********************************)

module type HYPERGRAPH =
        sig
                type v
                type g
                val compare : v -> v -> int
                val show : v -> string
                val tails : g -> v -> (v list * Rule.r) list
                val all_vertices : g -> v list
        end

(* This implements the HYPERGRAPH signature, but we don't declare it as such here 
 * in order to allow other code to know that Graph.g is Chart.chart, etc. *)
module ChartAsGraph =
        struct
                type v = Chart.item
                type g = Chart.chart
                let compare = Chart.compare_items
                let show = Chart.debug_str
                let tails c i = Chart.get_routes i c
                let all_vertices c = Chart.map_items c (fun i -> i)    (* VERY SLOW, not for production, according to comment in chart.ml *)
        end
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
                        map_tr (fun r -> (get_children r, r)) relevant_rules
                let all_vertices rules = uniques (map_tr Rule.get_nonterm rules)
        end

module KBestCalculation = functor (Graph : HYPERGRAPH) ->
struct

    type dbp = (Graph.v list * Rule.r) * int list
    type kbest_state = { g : Graph.g ;
                         dhat : (Graph.v, dbp list) Hashtbl.t ;             (* dhat[v] is an ordered list of the best dbps for vertex v *)
                         dhat_best_weight : (Graph.v, weight) Hashtbl.t ;   (* dhat_best_weight[v] is the weight of dhat[v][0]; just for get_one_best *)
                         cand : (Graph.v, dbp list) Hashtbl.t ;             (* cand[v] is a heap of candidate dbps for vertex v *)
                         cand_accum : (Graph.v, dbp list) Hashtbl.t ;       (* cand_accum[v] is a list of all candidates that have ever been added to cand[v] *)
                       }

    let show_dbp ((children, r, wt), ns) = Printf.sprintf "(%s, %s)" (Rule.to_string r) (show_list string_of_int ns)

    let rec dbp_interpret state dbp interpret_terminal combine =
        let helper v n =
            if (Graph.tails state.g v = []) then (     (* If v is a terminal *)
                if n = 1 then
                    interpret_terminal v
                else
                    failwith (Printf.sprintf "dbp_interpret: terminal has only one derivation (not %d)!" n)
            ) else (
                let d = List.nth (Hashtbl.find state.dhat v) (n-1) in
                dbp_interpret state d interpret_terminal combine
            )
        in
        let ((children,rule),ns) = dbp in
        assert (List.length children = List.length ns) ;
        let subresults = List.map2 helper children ns in
        combine (Rule.get_weight rule) subresults

    let dbp_weight state dbp =
        dbp_interpret state dbp (fun x -> weight_from_float 1.0) (List.fold_left mult_weights)

    let rec dtree_of_dbp state root_vertex dbp =    (* dbp is a derivation of root_vertex *)
        let helper v n =
            if (Graph.tails state.g v = []) then (
                if n = 1 then
                    failwith "Didn't think we'd get here"
                else
                    failwith (Printf.sprintf "dtree_of_dbp: terminal has only one derivation (not %d)!" n)
            ) else (
                let d = List.nth (Hashtbl.find state.dhat v) (n-1) in
                dtree_of_dbp state v d
            )
        in
        let ((children,rule),ns) = dbp in
        assert (List.length children = List.length ns) ;
        if (children = []) then (
            Leaf(root_vertex, rule, Rule.get_weight rule)
        ) else (
            let child_trees = List.map2 helper children ns in
            let total_weight = List.fold_left mult_weights (Rule.get_weight rule) (List.map get_weight child_trees) in
            NonLeaf(root_vertex, child_trees, rule, total_weight)
        )

    let extract_min state v =
        let candidates = Hashtbl.find state.cand v in
        let compare_by_weight dbp1 dbp2 = compare_weights (dbp_weight state dbp1) (dbp_weight state dbp2) in
        match reverse_tr (List.sort compare_by_weight candidates) with
        | (x::xs) -> Hashtbl.replace state.cand v xs ;
                     x
        | _ -> failwith "extract_min: no candidates for this vertex"

    exception NoMoreCandidates

    let rec lazy_kth_best state v k kp =          (* kp is k', the global k *)
        assert (Graph.tails state.g v <> []) ;
        try
            while (List.length (Hashtbl.find state.dhat v) < k) do
                if (Hashtbl.find state.dhat v <> []) then (
                    let (e,j) = List.hd (reverse_tr (Hashtbl.find state.dhat v)) in
                    lazy_next state v e j kp
                ) ;
                if (Hashtbl.find state.cand v <> []) then (
                    let current_list = Hashtbl.find state.dhat v in
                    Hashtbl.replace state.dhat v (current_list @ [extract_min state v])
                ) else (
                    raise NoMoreCandidates
                )
            done
        with NoMoreCandidates -> ()

    and lazy_next state v e j kp =
        let (children,rule) = e in
        for i=0 to (List.length children - 1) do
            let child = List.nth children i in
            if (Graph.tails state.g child <> []) then (
                let jp = List.map2 (fun pos x -> if (pos=i) then (x+1) else x) (range 0 (List.length j)) j in
                lazy_kth_best state child (List.nth jp i) kp ;
                if (List.nth jp i <= List.length (Hashtbl.find state.dhat child)) && (not (List.mem (e,jp) (Hashtbl.find state.cand_accum v))) then (
                    Hashtbl.replace state.cand v ((Hashtbl.find state.cand v) @ [(e,jp)]) ;
                    Hashtbl.replace state.cand_accum v ((Hashtbl.find state.cand_accum v) @ [(e,jp)]) ;
                )
            )
        done

    let tails_equal (children1,rule1) (children2,rule2) =
        let result = (children1 = children2) && (Rule.to_string rule1 = Rule.to_string rule2) in
        result

    let rec get_one_best state v visited =
        if Hashtbl.mem state.dhat v then
            Some (Hashtbl.find state.dhat v, Hashtbl.find state.dhat_best_weight v, Hashtbl.find state.cand v)
        else (
            let tails = Graph.tails state.g v in
            let new_visited = v::visited in
            let non_looping_tail (children, rule) = not (List.exists (fun x -> List.mem x new_visited) children) in
            let best_weight_via_tail (children, rule) : weight option =
                let best_subderivations = List.map (fun x -> get_one_best state x new_visited)
                                                   (List.filter (fun x -> Graph.tails state.g x <> []) children) in
                if List.mem None best_subderivations then
                    None
                else
                    let component_weights : Util.weight list = optlistmap (function None -> None | Some (_,w,_) -> Some w) best_subderivations in
                    Some (List.fold_left mult_weights (Rule.get_weight rule) component_weights)
            in
            let tails_with_weights = List.map (fun t -> (t, best_weight_via_tail t)) (List.filter non_looping_tail tails) in
            let tails_with_usable_weights = optlistmap (function (t,None) -> None | (t,Some w) -> Some (t,w)) tails_with_weights in
            let compare_tails (t1,w1) (t2,w2) = compare_weights w1 w2 in
            match reverse_tr (List.sort compare_tails tails_with_usable_weights) with
            | [] ->
                assert (visited <> []) ;
                None
            | (best::_) ->
                let (best_tail, best_weight) = best in
                let dbp_of_tail (children,rule) = ((children,rule), (List.map (fun _ -> 1) children)) in
                let rest = List.filter (fun x -> not (tails_equal x best_tail)) tails in
                let candidate_heap = map_tr dbp_of_tail rest in
                let best_dbp = dbp_of_tail best_tail in
                if (visited = []) then (
                    Hashtbl.replace state.cand v candidate_heap ;
                    Hashtbl.replace state.cand_accum v candidate_heap ;
                    Hashtbl.replace state.dhat v [best_dbp] ;
                    Hashtbl.replace state.dhat_best_weight v best_weight ;
                ) ;
                Some ([best_dbp], best_weight, candidate_heap)
        )

    let random_permutation xs =
        Random.self_init () ;
        let rand = 5 + Random.int 100 in
        let key x =
            let hash_value = (Hashtbl.hash (Graph.show x)) * (Hashtbl.hash (Graph.show x)) in
            hash_value mod rand
        in
        List.sort (fun x y -> compare (key x) (key y)) xs

    let get_n_best n graph vertex =
        let state = {g = graph ; dhat = Hashtbl.create 20 ; dhat_best_weight = Hashtbl.create 20 ; cand = Hashtbl.create 20 ; cand_accum = Hashtbl.create 20} in
        List.iter (fun v -> ignore (get_one_best state v [])) (random_permutation (Graph.all_vertices graph)) ;   (* Correctness should not depend on order *)
        lazy_kth_best state vertex n n ;
        let result = Hashtbl.find state.dhat vertex in
        let result_dtrees = List.map (dtree_of_dbp state vertex) result in
        result_dtrees

end (* end of the KBestCalculation functor *)

module KBestFromChart = KBestCalculation(ChartAsGraph)
module KBestFromGrammar = KBestCalculation(GrammarAsGraph)

let get_n_best_from_chart = KBestFromChart.get_n_best
let get_n_best_from_grammar = KBestFromGrammar.get_n_best

