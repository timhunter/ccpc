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

(* Compare derivations by weight; if they're equal by weight, back off and compare the other 
 * components. We don't want to return 0 in cases where the weights are equal, because 
 * this would mean that two distinct derivations could be considered equal for the purposes 
 * of sorting and k-best lists, which would destroy the one-to-one correspondence between 
 * derivation trees and what Huang & Chiang call 'dbp's. *)
let rec (>*>) t1 t2 =
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
                let children_result = compare_lists (>*>) (get_children t1) (get_children t2) in
                if (children_result <> 0) then
                        children_result
                else (
                        let item_result = compare (Chart.debug_str (get_root_item t1)) (Chart.debug_str (get_root_item t2)) in
                        if (item_result <> 0) then
                                item_result
                        else (
                                compare (Rule.to_string (get_rule t1)) (Rule.to_string (get_rule t2))
                        )
                )
        )

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

let print_tree_compact tree =
        let rec print_tree' t =
                match (get_children t, Rule.get_expansion (get_rule t)) with
                | ([], Rule.PublicTerminating s) -> s
                | ([c], Rule.PublicNonTerminating _) -> print_tree' c
                | (cs, Rule.PublicNonTerminating _) -> "[" ^ (String.concat " " (map_tr print_tree' cs)) ^ "]"
                | _ -> failwith "Inconsistent tree in print_tree"
        in
        (show_weight_float (get_weight tree)) ^^ (print_tree' tree)

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
(* Returns None if all derivations of item involve returning to items we have previously visited; 
 * in other words, when there is no derivation of item that is part of the best derivation of all its parents. 
 * Note this means that this function never returns None if visited is empty. *)
let rec get_best_derivation' chart visited item =

        let get_best_by_route (antecedents,r,wt) =
                let new_visited = item::visited in
                if (List.exists (fun v -> List.mem v antecedents) new_visited) then (* No loops in the best derivation *)
                        None
                else
                        let children = map_tr (get_best_derivation' chart new_visited) antecedents in
                        match (require_no_nones children) with
                        | None -> None
                        | Some xs -> Some (make_derivation_tree item xs r wt)
        in

        let routes = Chart.get_routes item chart in
        let candidates = optlistmap get_best_by_route routes in
        let result =
                match (List.sort (>*>) candidates) with
                | [] -> None
                | (x::_) -> Some x
        in
        result

(* Wrapper for the above: assumes no history of visited nodes, and therefore 
 * does not return an option type. *) 
let get_best_derivation mem chart item =
        let result =
                try Hashtbl.find (!mem) (1,item)
                with Not_found ->
                        let x = get_best_derivation' chart [] item in
                        Hashtbl.add (!mem) (1,item) x ;
                        x
        in
        match result with
                | Some d -> d
                | None   -> failwith (Printf.sprintf "Couldn't find a best derivation for %s, even with an empty list of visited items\n" (Chart.debug_str item))

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

module type MYQUEUE =
        sig
                type t
                type recipe = ((Chart.item * int) list) * (derivation_tree list -> derivation_tree)
                val empty : t
                val size : t -> int
                val add : t -> recipe -> t
                val add' : t -> (derivation_tree * recipe) -> t
                val max_elt : t -> (int -> Chart.item -> derivation_tree option) -> ((derivation_tree * recipe) * t)
        end

module CandidateVectorQueueFast : MYQUEUE =
        struct
                type recipe = ((Chart.item * int) list) * (derivation_tree list -> derivation_tree)
                module EvaluatedCandidateQueue = Set.Make(
                        struct
                                type t = derivation_tree * recipe
                                let compare (d1,_) (d2,_) = d2 >*> d1
                        end
                )
                type t = (recipe list) * EvaluatedCandidateQueue.t
                let empty = ([], EvaluatedCandidateQueue.empty)
                let size (uneval,eval) = List.length uneval + EvaluatedCandidateQueue.cardinal eval
                let add (uneval,eval) recipe = (recipe::uneval, eval)
                let add' (uneval,eval) (d,recipe) = (uneval, EvaluatedCandidateQueue.add (d,recipe) eval)
                let try_eval get_nth recipe =
                        let (ingredients,f) = recipe in
                        let child_derivations = map_tr (fun (it,i) -> get_nth i it) ingredients in
                        match (require_no_nones child_derivations) with
                        | None -> None
                        | Some cs -> Some (f cs)
                let update get_nth (uneval,eval) =
                        let f (uneval',eval') recipe =
                                match (try_eval get_nth recipe) with
                                | None -> add (uneval',eval') recipe
                                | Some d -> add' (uneval',eval') (d,recipe) in
                        List.fold_left f ([],eval) uneval
                let max_elt q get_nth =
                        let (uneval,eval) = update get_nth q in
                        let max = EvaluatedCandidateQueue.max_elt eval in
                        (max, (uneval, EvaluatedCandidateQueue.remove max eval))
        end

module CandidateVectorQueue = CandidateVectorQueueFast

(* The neighbours of a derivation are other derivations of the same item, that use 
 * the same route in their final step. *)
let neighbours (ingredients,f) =
        let rec add_one_at_position p lst =
                match (p,lst) with
                | (_,[]) -> []
                | (0,((it,i)::rest)) -> (it,i+1)::rest
                | (n,((it,i)::rest)) -> (it,i) :: (add_one_at_position (n-1) rest)
        in
        let neighbour_ingredients = map_tr (fun p -> add_one_at_position p ingredients) (range 0 (List.length ingredients)) in
        map_tr (fun ing -> (ing,f)) neighbour_ingredients

(* Make sure we don't go back to an (int,item) pair that is the same as, or worse than, one 
 * we've already visited. *)
let rec guarded_get_nth mem visited i chart it =
        if not (VisitHistory.ok_to_visit visited it i) then (
                None
        ) else (
                get_nth_best_derivation' mem visited i chart it
        )

and get_n_best_all_routes mem n chart item visited routes : derivation_tree list =
        let result = ref [] in
        let candidates = ref (CandidateVectorQueue.empty) in
        let initialise (antecedents,r,wt) =
                if (antecedents = []) then
                        (* Initialise result list with the one corresponding derivation. *)
                        result := (make_derivation_tree item [] r wt)::(!result)
                else
                        (* Initialise with the result of the ``initial parsing phase''. *)
                        let best_children = map_tr (get_best_derivation mem chart) antecedents in
                        let best = make_derivation_tree item best_children r wt in
                        candidates := CandidateVectorQueue.add' (!candidates) (best, (map_tr (fun x -> (x,1)) antecedents, fun cs -> make_derivation_tree item cs r wt)) ;
        in
        List.iter initialise routes ;
        begin try
                while (List.length !result < n) do
                        let new_visited = VisitHistory.add visited item (List.length !result + 1) in
                        let ((next,recipe),new_cand) = CandidateVectorQueue.max_elt (!candidates) (fun i it -> guarded_get_nth mem new_visited i chart it) in
                        candidates := new_cand ;
                        if (not (List.exists (fun d -> ((d >*> next) = 0)) !result)) then
                                result := next::(!result) ;
                        let add_candidate x = candidates := CandidateVectorQueue.add (!candidates) x in
                        List.iter add_candidate (neighbours recipe)
                done
        with Not_found -> () end ;
        List.sort (>*>) (!result)

(* Return type: derivation_tree option
   Returns None if there are less than n derivations of item. 
   This is basically Algorithm 3 from Huang & Chiang, ``Better k-best parsing'' *)
and get_nth_best_derivation' mem visited n chart item =
        assert (n >= 1) ;
        try Hashtbl.find (!mem) (n, item)
        with Not_found ->
                let n_best_overall = get_n_best_all_routes mem n chart item visited (Chart.get_routes item chart) in
                let result = (try Some (List.nth n_best_overall (n-1)) with Failure _ -> None) in
                Hashtbl.add (!mem) (n, item) result ;   (* Turns out the memoisation need not be conditioned on visit history. Not immediately obvious, but true. *)
                result

let get_n_best n chart item =
        let mem = ref (Hashtbl.create 1000) in   (* create a single memoising hashtable to be used in every call to get_nth_best_derivation' *)
        let lst = map_tr (fun i -> get_nth_best_derivation' mem VisitHistory.empty i chart item) (range 1 (n+1)) in
        let rec take_while_not_none = function ((Some x)::xs) -> x :: (take_while_not_none xs) | _ -> [] in
        take_while_not_none lst

