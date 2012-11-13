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
        let use_route (antecedents,rule,weight_factor) =
                map_tr (fun children -> (children,rule,weight_factor)) (children_lists antecedents) in
        let results_from_all_routes = List.concat (map_tr use_route routes) in
        map_tr (fun (children,rule,weight_factor) -> make_derivation_tree item children rule weight_factor) results_from_all_routes

(**************************************************************************)
(****** Stuff for computing k-best lists **********************************)

module type HYPERGRAPH =
        sig
                type v
                type g
                val compare : v -> v -> int
                val show : v -> string
                val tails : g -> v -> (v list * Rule.r * Util.weight) list
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
                        map_tr (fun r -> (get_children r, r, Rule.get_weight r)) relevant_rules
        end

module KBestCalculation = functor (Graph : HYPERGRAPH) ->
struct

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

                let routes = Graph.tails chart item in
                let candidates = optlistmap get_best_by_route routes in
                let result =
                        match (List.sort (compare_derivations Graph.compare) candidates) with
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
                        | None   -> failwith (Printf.sprintf "Couldn't find a best derivation for %s, even with an empty list of visited items\n" (Graph.show item))

        module VisitHistory :
                sig
                        type t
                        val empty : t
                        val add : t -> Graph.v -> int -> t
                        val ok_to_visit : t -> Graph.v -> int -> bool
                        val has_visited_some_n : t -> Graph.v -> bool
                        (*val show : t -> string*)
                end
        =
                (*struct
                        type t = (Graph.v * int) list
                        let empty = []
                        let add hist item n = (item,n)::hist
                        let ok_to_visit hist item n = not (List.exists (fun (it',i') -> (item = it') && (n >= i')) hist)
                        let has_visited_some_n hist item = List.mem_assoc item hist
                        let show hist = Util.show_list (fun (v,i) -> Printf.sprintf "(%s,%d)" (Graph.show v) i) hist
                end*)
                struct
                        type t = (Graph.v, int) Hashtbl.t
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
                                (* Printf.eprintf "ok_to_visit %d-th best derivation of %s?\t" n (Graph.show item) ;
                                if (Hashtbl.mem hist item) then (
                                        let current = Hashtbl.find hist item in
                                        if (n < current) then
                                                Printf.eprintf "    yes, so far we've only visited the %d-th\n" current
                                        else
                                                Printf.eprintf "    no, we've already visited the %d-th\n" current
                                ) else (
                                        Printf.eprintf "    yes, we've never been to any derivation of this nonterminal\n"
                                ) ; *)
                                try     let current = Hashtbl.find hist item in
                                        assert (n < current) ;
                                        n < current
                                with Not_found -> true
                        let has_visited_some_n hist item = Hashtbl.mem hist item
                end

        module type MYQUEUE =
                sig
                        type t
                        type recipe = ((Graph.v * int) list) * (Graph.v derivation_tree list -> Graph.v derivation_tree)
                        val empty : t
                        val size : t -> int
                        val add : t -> recipe -> t
                        val add' : t -> (Graph.v derivation_tree * recipe) -> t
                        val max_elt : t -> (int -> Graph.v -> Graph.v derivation_tree option) -> ((Graph.v derivation_tree * recipe) * t)
                end

        module CandidateVectorQueueFast : MYQUEUE =
                struct
                        type recipe = ((Graph.v * int) list) * (Graph.v derivation_tree list -> Graph.v derivation_tree)
                        module EvaluatedCandidateQueue = Set.Make(
                                struct
                                        type t = Graph.v derivation_tree * recipe
                                        let compare (d1,_) (d2,_) = compare_derivations Graph.compare d2 d1
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
                        (* Turns out we never get here; the core algorithm ``knows'' never to ask for 
                         * a ``worse'' derivation of a node than we have already chosen to pursue. *)
                        assert false ;
                        None
                ) else (
                        if (VisitHistory.has_visited_some_n visited it) then (
                                assert (Hashtbl.mem (!mem) (i,it)) ;
                                if (Graph.show it = "[ S 0:0 ]") then Printf.eprintf "    Pulling %d-th best out of memowy\n" i ;
                                Hashtbl.find (!mem) (i,it)
                        ) else
                                get_nth_best_derivation' mem visited i chart it
                )

        and get_n_best_all_routes mem n chart item visited routes =
                let result = ref [] in
                let candidates = ref (CandidateVectorQueue.empty) in
                let initialise (antecedents,r,wt) =
                        Printf.eprintf "Initialising result and candidates for item %s\n" (Graph.show item) ;
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
                                if (Graph.show item = "[ S 0:0 ]") then Printf.eprintf "Just got %d-th best out of CandidateVectorQueue\n" (1 + List.length !result) ;
                                candidates := new_cand ;
                                if (not (List.exists (fun d -> (compare_derivations Graph.compare d next = 0)) !result)) then
                                        result := next::(!result) ;
                                let add_candidate x = candidates := CandidateVectorQueue.add (!candidates) x in
                                List.iter add_candidate (neighbours recipe) ;
                                if (Graph.show item = "[ S 0:0 ]") then (
                                        Printf.eprintf "    Working out %d best, got %d so far:\n" n (List.length !result) ;
                                        List.iter (fun t -> Printf.eprintf "        %s\n" (print_tree_compact t)) (List.sort (compare_derivations Graph.compare) !result) ;
                                        let max_recorded mem it =
                                            let i = ref 1 in
                                            while (Hashtbl.mem (!mem) (!i,it)) do i := (!i)+1 done ;
                                            (!i) - 1
                                        in
                                        Printf.eprintf "    And memowy contains results down to %d-th best\n" (max_recorded mem item)
                                )
                        done
                with Not_found -> () end ;
                reverse_tr (!result)

        (* Return type: derivation_tree option
           Returns None if there are less than n derivations of item. 
           This is basically Algorithm 3 from Huang & Chiang, ``Better k-best parsing'' *)
        and get_nth_best_derivation' mem visited n chart item =
                assert (n >= 1) ;
                assert (List.for_all (fun i -> Hashtbl.mem (!mem) (i,item)) (range 1 n)) ;
                (* Every time this function is called on a repeat nonterminal, we have the result memoised *)
                (* if (Graph.show item = "[ S 0:0 ]") then (
                        Printf.eprintf "*** Call for the %d-th best derivation of %s      \trepeat nonterminal: %b        \tmemoised: %b\n"
                                       n (Graph.show item) (VisitHistory.has_visited_some_n visited item) (Hashtbl.mem !mem (n,item))
                ) ; *)
                try
                        let result = Hashtbl.find (!mem) (n, item) in
                        if (Graph.show item = "[ S 0:0 ]") then Printf.eprintf "    Pulling %d-th best out of memowy\n" n ;
                        result
                with Not_found ->
                        if (Graph.show item = "[ S 0:0 ]") then (
                                Printf.eprintf "Don't have %d-th best saved, going to work it out ...\n" n
                        ) ;
                        assert ((n=1) || (Hashtbl.mem (!mem) (n-1, item))) ;
                        assert (not (VisitHistory.has_visited_some_n visited item)) ;
                        (* At this point, we know the nth best derivation of item isn't memoised.
                         * So we call get_n_best_all_routes for the full list of n, because then we can take the last.
                         * But if the first (n-1) derivations are all memoised, we don't take advantage of them! *)
                        let n_best_overall = get_n_best_all_routes mem n chart item visited (Graph.tails chart item) in
                        let result = (try Some (List.nth n_best_overall (n-1)) with Failure _ -> None) in
                        assert (not (Hashtbl.mem (!mem) (n,item))) ;
                        if (Graph.show item = "[ S 0:0 ]") then Printf.eprintf "Memoising %d-th best derivation of S\n" n ;
                        Hashtbl.add (!mem) (n, item) result ;   (* Turns out the memoisation need not be conditioned on visit history. Not immediately obvious, but true. *)
                        result

        (* Wrapper function, to be used from outside this module/functor *)
        let get_n_best n graph vertex =
                assert (n >= 1) ;
                let mem = ref (Hashtbl.create 1000) in   (* create a single memoising hashtable to be used in every call to get_nth_best_derivation' *)
                let lst = map_tr (fun i -> get_nth_best_derivation' mem VisitHistory.empty i graph vertex) (range 1 (n+1)) in
                let rec take_while_not_none = function ((Some x)::xs) -> x :: (take_while_not_none xs) | _ -> [] in
                take_while_not_none lst

        let get_nth_best n graph vertex =
                assert (n >= 1) ;
                let mem = ref (Hashtbl.create 1000) in   (* create a single memoising hashtable to be used in every call to get_nth_best_derivation' *)
                match (get_nth_best_derivation' mem VisitHistory.empty n graph vertex) with
                | None -> []
                | Some d -> [d]

end (* end of the KBestCalculation functor *)

module KBestFromChart = KBestCalculation(ChartAsGraph)
module KBestFromGrammar = KBestCalculation(GrammarAsGraph)

let get_n_best_from_chart = KBestFromChart.get_n_best
let get_n_best_from_grammar = KBestFromGrammar.get_n_best

module Koller_KBestCalculation = functor (Graph : HYPERGRAPH) ->
struct

        (* AK's UnEvaluatedItem *)
        type recipe = ((Graph.v * int) list) * (Graph.v * Rule.r * Util.weight)  (* second component is everything we need for make_derivation_tree, except the children *)

        (* AK's EvaluatedItem *)
        type evaluated_item = recipe * (Graph.v derivation_tree)

        (* recipe -> recipe list *)
        let make_variations recipe =
                let rec add_one_at_position p lst =
                        match (p,lst) with
                        | (_,[]) -> []
                        | (0,((it,i)::rest)) -> (it,i+1)::rest
                        | (n,((it,i)::rest)) -> (it,i) :: (add_one_at_position (n-1) rest)
                in
                map_tr (fun p -> add_one_at_position p recipe) (range 0 (List.length recipe))

        module rec StreamForRule :
                sig
                        type t
                        val init : Graph.v -> (Graph.v list * Rule.r * Util.weight) -> t
                        val peek : t -> Graph.g -> Graph.v list -> Graph.v derivation_tree option
                        val pop : t -> Graph.g -> Graph.v list -> Graph.v derivation_tree option
                end
        =
                struct
                        type t = { mutable evaled : evaluated_item list ;   (* evaluated items, stored in order *)
                                   mutable unevaled : recipe list ;         (* unevaluated items, order not meaningful *)
                                   mutable discovered : recipe list         (* things we've already added to unevaled (and shouldn't add again) *)
                                 }

                        let init parent (child_list, r, w) =
                                let best_vector = map_tr (fun i -> (i,0)) child_list in
                                let f = (parent,r,w) in
                                let best_recipe = (best_vector,f) in
                                {evaled = [] ; unevaled = [best_recipe] ; discovered = [best_recipe]}

                        let try_eval_all g visited stream =
                                let to_be_removed = ref [] in
                                let try_eval recipe =
                                        let (child_vertices, (parent,r,wt)) = recipe in
                                        let children = map_tr (fun (v,i) -> StreamForVertex.get_tree (StreamForVertex.get_stream g v) i visited) child_vertices in
                                        match (require_no_nones children) with
                                        | None -> ()
                                        | Some xs ->
                                                let new_tree = make_derivation_tree parent xs r wt in
                                                stream.evaled <- (recipe, new_tree)::(stream.evaled) ;
                                                to_be_removed := recipe::(!to_be_removed)
                                in
                                List.iter try_eval stream.unevaled ;
                                stream.unevaled <- List.filter (fun x -> not (List.mem x !to_be_removed)) stream.unevaled ;
                                let compare_evaled_items (r1,t1) (r2,t2) = compare_derivations Graph.compare t1 t2 in
                                stream.evaled <- List.sort compare_evaled_items stream.evaled

                        let peek stream g visited =
                                try_eval_all g visited stream ;
                                match stream.evaled with
                                | []                 -> None
                                | ((recipe,tree)::_) -> Some tree

                        let pop stream g visited =
                                try_eval_all g visited stream ;
                                match stream.evaled with
                                | []                  -> None
                                | ((recipe,tree)::xs) -> stream.evaled <- xs ;
                                                         let (children,prw) = recipe in
                                                         let variations = make_variations children in
                                                         let new_variations = List.filter (fun v -> not (List.mem (v,prw) stream.discovered)) variations in
                                                         stream.discovered <- stream.discovered @ (map_tr (fun v -> (v,prw)) new_variations) ;
                                                         stream.unevaled   <- stream.unevaled   @ (map_tr (fun v -> (v,prw)) new_variations) ;
                                                         Some tree
                end

        and StreamForVertex :
                sig
                        type t
                        val get_stream : Graph.g -> Graph.v -> t
                        val get_tree : t -> int -> (Graph.v list) -> Graph.v derivation_tree option   (* second arg is list of visited states *)
                end
        =
                struct
                        type t = {         g : Graph.g ;                           (* the graph we're part of *)
                                           vertex : Graph.v ;                      (* the nonterminal we're responsible for *)
                                           rule_streams : StreamForRule.t list ;   (* the streams corresponding to the rules expanding this nonterminal *)
                                   mutable known : (Graph.v derivation_tree) list  (* evaluated items, stored in order *)
                                 }

                        let streams = Hashtbl.create 100

                        let init g v =
                                let routes = Graph.tails g v in
                                { g = g ; vertex = v ; rule_streams = reverse_tr (map_tr (StreamForRule.init v) routes) ; known = [] }

                        let get_stream g v =
                                try Hashtbl.find streams v
                                with Not_found ->
                                        let s = init g v in
                                        Hashtbl.add streams v s ;
                                        s

                        let get_tree vstream n visited =
                                let rand = Random.int 10000 in
                                if (List.length (Graph.tails vstream.g vstream.vertex) != 1) then
                                        Printf.eprintf "%04d: Trying for the %d-th best tree for %s\n" rand n (Graph.show vstream.vertex) ;
                                assert (n >= 0) ;
                                try
                                        let tree = List.nth vstream.known n in
                                        Some tree
                                with Failure _ -> (
                                        assert (n = List.length vstream.known) ; (* Assert we're looking for the very next thing not in our known list *)
                                        if (List.mem vstream.vertex visited) then (
                                                assert (n = 0) ;
                                                if (List.length (Graph.tails vstream.g vstream.vertex) != 1) then
                                                    Printf.eprintf "%04d: Turning back from %s with visitedStates %s\n"
                                                                   rand (Graph.show vstream.vertex) (show_list Graph.show visited) ;
                                                None
                                        ) else (
                                                let new_visited = vstream.vertex :: visited in
                                                if (List.length (Graph.tails vstream.g vstream.vertex) != 1) then
                                                    Printf.eprintf "%04d: Looking at streams for %d-th best derivation of %s, with visitedStates %s\n"
                                                                   rand n (Graph.show vstream.vertex) (show_list Graph.show visited) ;
                                                let try_each_rule = map_tr (fun s -> (StreamForRule.peek s vstream.g new_visited, s)) vstream.rule_streams in
                                                let results = optlistmap (function (None,_) -> None | (Some y, s) -> Some (y,s)) try_each_rule in
                                                let cmp (t1,s1) (t2,s2) = compare_derivations Graph.compare t1 t2 in
                                                match (List.sort cmp results) with
                                                | [] ->         None
                                                | ((t,s)::_) -> vstream.known <- (vstream.known @ [t]) ;
                                                                if (List.length (Graph.tails vstream.g vstream.vertex) != 1) then
                                                                    Printf.eprintf "%04d: Recorded %s as %d-th best derivation of %s, with visitedStates %s\n"
                                                                                   rand (print_tree_compact t) n (Graph.show vstream.vertex) (show_list Graph.show visited) ;
                                                                let t' = StreamForRule.pop s vstream.g visited in
                                                                let derivations_equal d1 d2 = (compare_derivations Graph.compare d1 d2 = 0) in
                                                                assert (match t' with (Some t'') -> derivations_equal t'' t | _ -> false) ;
                                                                t'
                                        )
                                )
                end

        let get_n_best n graph vertex =
                assert (n >= 0) ;
                let s = StreamForVertex.get_stream graph vertex in
                let result = ref [] in
                let finished = ref false in
                while ((not !finished) && (List.length !result < n)) do
                        let next_tree = StreamForVertex.get_tree s (List.length !result) [] in
                        match next_tree with
                        | None   -> finished := true
                        | Some t -> result := t::(!result)
                done ;
                reverse_tr !result

end (* end of the Koller_KBestCalculation functor *)

module Koller = Koller_KBestCalculation(ChartAsGraph)
let get_n_best_koller = Koller.get_n_best

