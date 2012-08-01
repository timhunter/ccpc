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

(* Compare derivations by weight; if they're equal by weight, back off to the built-in 
 * compare function. We don't want to return 0 in cases where the weights are equal, because 
 * this would mean that two distinct derivations could be considered equal for the purposes 
 * of sorting and k-best lists, which would destroy the one-to-one correspondence between 
 * derivation trees and what Huang & Chiang call 'dbp's. *)
let (>*>) t1 t2 =
        match (compare_weights (get_weight t2) (get_weight t1)) with
        | 0 -> compare t2 t1
        | x -> x

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
		let first_line = (Chart.get_nonterm item) ^^ yield ^^ (show_weight_float (get_weight t)) in
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

(**************************************************************************)
(****** Stuff for computing k-best lists **********************************)

(* Viterbi search for the single best derivation. This in effect performs what 
 * Huang & Chiang call the ``initial parsing phase'' of their Algorithm 3, although 
 * we do it along the way whenever we need to find a 1-best derivation rather than 
 * in an initial separated phase. *)
let rec get_best_derivation mem chart item =

        try
                (* Could probably clean up the types to make this nicer at some point *)
                match (Hashtbl.find (!mem) (1, item)) with
                | Some d -> d
                | None -> failwith (Printf.sprintf "Something went very wrong: memoised None for best derivation of item %s\n" (Chart.debug_str item))

        with Not_found ->

                let get_best_by_route (antecedents,r,wt) =
                        if (List.mem item antecedents) then  (* No loops in the best derivation *)
                                None
                        else
                                let children = map_tr (get_best_derivation mem chart) antecedents in
                                Some (make_derivation_tree item children r wt)
                in

                let routes = Chart.get_routes item chart in
                let candidates = optlistmap get_best_by_route routes in
                let result =
                        match (List.sort (>*>) candidates) with
                        | [] -> failwith (Printf.sprintf "Couldn't find any derivations for item: %s\n" (Chart.debug_str item))
                        | (x::_) -> x
                in
                Hashtbl.add (!mem) (1, item) (Some result) ;
                result

let rec require_no_nones (lst : 'a option list) : 'a list option =
        match lst with
        | [] -> Some []
        | (None :: xs) -> None
        | ((Some x) :: xs) -> match (require_no_nones xs) with | None -> None | Some rest -> Some (x::rest)

let rec deriv_equal t1 t2 =
        match (t1,t2) with
        | (Leaf(_,_,_), NonLeaf(_,_,_,_)) -> false
        | (NonLeaf(_,_,_,_), Leaf(_,_,_)) -> false
        | (Leaf(i1,r1,w1), Leaf(i2,r2,w2)) -> (i1 = i2) && (r1 = r2) && (compare_weights w1 w2 = 0)
        | (NonLeaf(i1,cs1,r1,w1), NonLeaf(i2,cs2,r2,w2)) -> (i1 = i2) && (r1 = r2) && (compare_weights w1 w2 = 0) && (List.length cs1 = List.length cs2) &&
                                                            (List.for_all2 deriv_equal cs1 cs2)

module VisitHistory :
        sig
                type t
                val empty : t
                val add : t -> Chart.item -> int -> t
                val ok_to_visit : t -> Chart.item -> int -> bool
        end
=
        struct
                type t = (Chart.item, int) Hashtbl.t
                let empty = Hashtbl.create 20
                let add hist item n =
                        let result = Hashtbl.copy hist in
                        begin
                        try     let current = Hashtbl.find hist item in
                                if (n < current) then Hashtbl.replace result item n
                        with Not_found -> Hashtbl.add result item n
                        end ;
                        result
                let ok_to_visit hist item n = (*not (List.exists (fun (it',i') -> (item = it') && (n >= i')) lst) *)
                        try     let current = Hashtbl.find hist item in
                                n < current
                        with Not_found -> true
        end

(* Make sure we don't go back to an (int,item) pair that is the same as, or worse than, one 
 * we've already visited. *)
let rec guarded_get_nth mem visited i chart it =
        if not (VisitHistory.ok_to_visit visited it i) then
                None
        else
                get_nth_best_derivation' mem visited i chart it

(* The neighbours of a derivation are other derivations of the same item, that use 
 * the same route in their final step. *)
and neighbours chart mem visited ((d,vec) : (derivation_tree * int list)) : (derivation_tree * int list) list =
        let rec add_one_at_position p lst =
                match (p,lst) with
                | (_,[]) -> []
                | (0,(x::xs)) -> (x+1)::xs
                | (n,(x::xs)) -> x :: (add_one_at_position (n-1) xs)
        in
        let neighbour_vecs = map_tr (fun p -> add_one_at_position p vec) (range 0 (List.length vec)) in
        let antecedent_items = map_tr get_root_item (get_children d) in
        let derivation_from_vec vec =
                let child_derivations = List.map2 (fun i -> fun it -> guarded_get_nth mem visited i chart it) vec antecedent_items in
                match (require_no_nones child_derivations) with
                | None -> None
                | Some cs -> Some (make_derivation_tree (get_root_item d) cs (get_rule d) (Rule.get_weight (get_rule d)), vec)
        in
        optlistmap derivation_from_vec neighbour_vecs

and get_n_best_by_route mem n chart item visited ((items,r,wt) : (Chart.item list * Rule.r * Util.weight)) : derivation_tree list =
        match items with
        | [] -> [make_derivation_tree item [] r wt]    (* if item is an axiom, there's only one derivation *)
        | _ ->
                let best_children = map_tr (get_best_derivation mem chart) items in
                let best = make_derivation_tree item best_children r wt in
                let result = ref [] in
                let candidates = ref [(best, map_tr (fun _ -> 1) items)] in
                while (List.length !result < n) && (!candidates <> []) do
                        match (List.sort (fun (d1,_) -> fun (d2,_) -> d1 >*> d2) !candidates) with
                        | [] -> assert false  (* Shouldn't be possible. This would not be so ugly if we had a break statement. *)
                        | (next,vec)::rest ->
                                (* We check for doubles because it's possible to end up at the same vector twice.
                                 * For example, we might go from [1,1] to [1,2] to [2,2], or from [1,1] to [2,1] to [2,2].
                                 * But ideally we would somehow prevent this doubling-up before building up the whole derivation itself. *)
                                if (not (List.exists (deriv_equal next) !result)) then
                                        result := next::(!result) ;
                                candidates := rest @ (neighbours chart mem visited (next,vec))
                done ;
                !result

(* Return type: derivation_tree option
   Returns None if there are less than n derivations of item. 
   This is basically Algorithm 3 from Huang & Chiang, ``Better k-best parsing'' *)
and get_nth_best_derivation' mem visited n chart item =
        assert (n >= 1) ;
        try Hashtbl.find (!mem) (n, item)
        with Not_found ->
                let lists = map_tr (get_n_best_by_route mem n chart item (VisitHistory.add visited item n)) (Chart.get_routes item chart) in
                let n_best_overall = take n (List.sort (>*>) (List.concat lists)) in
                let result = (try Some (List.nth n_best_overall (n-1)) with Failure _ -> None) in
                Hashtbl.add (!mem) (n, item) result ;   (* Turns out the memoisation need not be conditioned on visit history. Not immediately obvious, but true. *)
                result

let get_nth_best_derivation = get_nth_best_derivation' (ref (Hashtbl.create 1000)) VisitHistory.empty

