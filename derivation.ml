open Util

(* The weight stored with each derivation is the cumulative weight of the entire derivation.
 * The weight introduced by any individual step in the derivation can be determined from the corresponding rule. *)
type derivation_tree = Leaf of Chart.item * Rule.r * weight | NonLeaf of Chart.item * derivation_tree list * Rule.r * weight

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

let (>*>) t1 t2 = compare_weights (get_weight t2) (get_weight t1)

let make_derivation_tree item children rule weight_factor =
	let product = List.fold_left (mult_weights) weight_factor (map_tr get_weight children) in
	match children with
	| [] -> Leaf (item, rule, product)
	| _ -> NonLeaf (item, children, rule, product)

let rec one_from_each (lists : 'a list list) : ('a list list) =
	let prepend_one_of xs ys = map_tr (fun x -> x::ys) xs in
	match lists with
	| [] -> [[]]
	| (l::ls) -> List.concat (map_tr (prepend_one_of l) (one_from_each ls))

let print_tree tree =
	let rec print_tree' t =      (* returns a list of strings, each representing one line *)
		let item = get_root_item t in
		let children = get_children t in
		let yield =
			match (children, Rule.get_expansion (get_rule t)) with
			| ([]    , Rule.PublicTerminating s) -> Printf.sprintf "\"%s\"" s
			| ((_::_), Rule.PublicNonTerminating _) -> ""
			| _ -> failwith "Inconsistent tree in print_tree"
		in
		let first_line = (Chart.get_nonterm item) ^^ yield ^^ (Printf.sprintf "(%s)" (show_weight (get_weight t))) in
		let children_printed : (string list) = map_tr ((^) "    ") (List.concat (map_tr print_tree' children : (string list list))) in
		first_line :: children_printed
	in
	String.concat "\n" (print_tree' tree)

let rec get_derivations chart item =
        let routes = Chart.get_routes item chart in
        let children_lists antecedent_items = one_from_each (map_tr (get_derivations chart) antecedent_items) in
        let use_route (antecedents,rule,weight_factor) : ((derivation_tree list * Rule.r * weight) list) =
                map_tr (fun children -> (children,rule,weight_factor)) (children_lists antecedents) in
        let results_from_all_routes = List.concat (map_tr use_route routes) in
        map_tr (fun (children,rule,weight_factor) -> make_derivation_tree item children rule weight_factor) results_from_all_routes

(* Return type: derivation_tree option
   Returns None if there are less than n derivations of item. *)
let rec get_nth_best_derivation n chart (visited : (Chart.item * int) list) item =
        assert (n >= 1) ;
        let get_n_best_by_route ((items,r,wt) : (Chart.item list * Rule.r * Util.weight)) : derivation_tree list =
                match items with
                | [] -> [make_derivation_tree item [] r wt]    (* if item is an axiom, there's only one derivation *)
                | _ ->
                        let route_arity = List.length items in        (* r = |e| *)
                        let rank_vectors = all_lists (range 1 (n+1)) route_arity in     (* use numbers [1..n] inclusive because 1 means best (0 is invalid) *)
                        let zipped_vectors : (int * Chart.item) list list = map_tr (fun vec -> List.combine vec items) rank_vectors in
                        let guarded_get_nth i chart it =
                                let new_visited = (item,n)::visited in
                                if List.exists (fun (x,y) -> (it = x) && (i >= y)) new_visited then (
                                        None
                                ) else
                                        get_nth_best_derivation i chart new_visited it
                        in
                        let child_lists : (int * Chart.item) list -> (derivation_tree option) list = map_tr (fun (i,it) -> guarded_get_nth i chart it) in
                        let all_child_lists : (derivation_tree option) list list = map_tr child_lists zipped_vectors in
                        let rec require_no_nones (lst : 'a option list) : 'a list option =
                                match lst with
                                | [] -> Some []
                                | (None :: xs) -> None
                                | ((Some x) :: xs) -> match (require_no_nones xs) with | None -> None | Some rest -> Some (x::rest)
                        in
                        let all_complete_child_lists : derivation_tree list list = optlistmap require_no_nones all_child_lists in
                        let derivations = map_tr (fun children -> make_derivation_tree item children r wt) all_complete_child_lists in
                        take n derivations
        in
        let all_candidates = concatmap_tr get_n_best_by_route (Chart.get_routes item chart) in
        if (List.length all_candidates >= n) then (
                let sorted_candidates = List.sort (>*>) all_candidates in
                Some (List.nth sorted_candidates (n-1))
        ) else
                None

