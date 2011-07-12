open Util

type derivation_tree = Leaf of Chart.item | NonLeaf of (Chart.item * derivation_tree list)

let get_children t =
	match t with
	|Leaf(_) -> []
	| NonLeaf(_,ts) -> ts

let get_root_item t =
	match t with
	| Leaf (i) -> i
	| NonLeaf (i,_) -> i

let make_derivation_tree item children =
	match children with
	| [] -> Leaf (item)
	| _ -> NonLeaf (item, children)

let rec one_from_each (lists : 'a list list) : ('a list list) =
	let prepend_one_of xs ys = map_tr (fun x -> x::ys) xs in
	match lists with
	| [] -> [[]]
	| (l::ls) -> List.concat (map_tr (prepend_one_of l) (one_from_each ls))

let rec get_derivations chart (item : Chart.item) : (derivation_tree list) =
	let (routes : Chart.item list list) = map_tr (fun (items,_,_) -> items) (Chart.get_routes item chart) in
	let route_to_childrens (route : Chart.item list) : (derivation_tree list list) = one_from_each (map_tr (get_derivations chart) route) in
	let (childrens : derivation_tree list list) = List.concat (map_tr route_to_childrens routes) in
	map_tr (fun children -> make_derivation_tree item children) childrens


