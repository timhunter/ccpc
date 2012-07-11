open Util

type derivation_tree = Leaf of Chart.item * weight | NonLeaf of Chart.item * derivation_tree list * weight

let get_children t =
	match t with
	| Leaf(_,_) -> []
	| NonLeaf(_,ts,_) -> ts

let get_root_item t =
	match t with
	| Leaf (i,_) -> i
	| NonLeaf (i,_,_) -> i

let get_weight t =
        match t with
        | Leaf (_,w) -> w
        | NonLeaf (_,_,w) -> w

let make_derivation_tree item children weight_factor =
	let product = List.fold_left (mult_weights) weight_factor (map_tr get_weight children) in
	match children with
	| [] -> Leaf (item, product)
	| _ -> NonLeaf (item, children, product)

let rec one_from_each (lists : 'a list list) : ('a list list) =
	let prepend_one_of xs ys = map_tr (fun x -> x::ys) xs in
	match lists with
	| [] -> [[]]
	| (l::ls) -> List.concat (map_tr (prepend_one_of l) (one_from_each ls))

let rec get_derivations chart item =
        let routes = Chart.get_routes item chart in
        let children_lists antecedent_items = one_from_each (map_tr (get_derivations chart) antecedent_items) in
        let use_route (antecedents,_,weight_factor) : ((derivation_tree list * weight) list) = map_tr (fun children -> (children,weight_factor)) (children_lists antecedents) in
        let results_from_all_routes = List.concat (map_tr use_route routes) in
        map_tr (fun (children,weight_factor) -> make_derivation_tree item children weight_factor) results_from_all_routes
