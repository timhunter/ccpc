
open Util
open Rule

exception Failure of string

type mode = Naive | Newton


(****************************************************************************************)
(*part1*)
(* Helper functions*)

(* helper function of sameSet 
return true if the string element is within the string list 
otherwise return false
*)
let rec contains (element: string)(stringList: string list)=
(*false;;*)
match stringList with
|h::t->if h=element then true else (contains element t)
|_->false;;

(* helper function
return true if the two strings are in the same set
return false otherwise *)
let rec sameSet (nonterminal:string) (comparedString: string) (mutuallyRecursiveSets: string list list)=
(*false;*)
match mutuallyRecursiveSets with 
|h::t-> if (contains nonterminal h) then 
            if (contains comparedString h) then true else false
        else (sameSet nonterminal comparedString t)
|_->false

(* get a string as nonterminal and a rule list, return a list of tuples consisting of 
nonterminal list and a probability
for example, if in the rules there are VP -> V NP (0.4); VP -> V CP(0.6)
calling (findRule "VP" rules) will return [(["V";"NP"],0.4); (["V";"CP"],0.6)]
*)
let rec findRule (nonterminal:string) (rules: Rule.r list)(* =[(["nonterminal1";"nonterminal12"],0.05)] *)
= match rules with
|h::t -> if (get_nonterm h) = nonterminal then 
        match (get_expansion h) with
        |PublicTerminating str -> ([],(float_of_weight (get_weight h)))::(findRule nonterminal t)
        |PublicNonTerminating (nts,_) ->  ((Nelist.to_list nts),(float_of_weight (get_weight h)))::
        (findRule nonterminal t)
    else (findRule nonterminal t)
| _-> [];;

module StringMap = Map.Make (String);;

(*Given a list, and a map representing each unique value in the list, remove duplicates from the list*)
let rec removeDupWithMap xs m = 
    match xs with 
        |[] -> []
        |h::t -> (match StringMap.mem h m with 
            | false -> (removeDupWithMap t m)
            | true -> h::(removeDupWithMap t (StringMap.remove h m))
            );;

(*Given a list, build a map that associates each unique value of the list with 1*)
let rec buildUniqueMap xs = 
    match xs with 
        |[] -> StringMap.empty;
        |h::t -> StringMap.add h (Some 1) (buildUniqueMap t);;
        
let rec remove_from_left xs = removeDupWithMap xs (buildUniqueMap xs);;

(* remove duplicates from the rules *)
let removeDup xs x = 
    if List.mem x xs then xs else x:: xs;;

let remove_from_left_old xs = List.rev (List.fold_left removeDup [] xs);;

(* get all the nonterminals in the rules *)
let getAllNonterm rules = 
    remove_from_left (List.map (fun r -> get_nonterm r) rules);;

(* merge two list in a way that no item is duplicated *)

let mergeList l1 l2 = 
    remove_from_left (List.append l1 l2);;

(* find the first element from the list and return its index *)
let rec find x lst = 
     match lst with
      | [] -> raise (Failure "Not Found")
      | h :: t -> if x = h then 0 else 1 + find x t;;

(* if return true, the two lists are equivalent. If return false, then the lists are not equivalent*)
let equalStringList l1 l2 = 
    if (List.length l1 = List.length l2) then not (List.mem false (List.map (fun ele -> List.mem ele l2) l1))
    else false;;

(* similar to List.mem, check whether this element appeared later *)
let rec existEquivalent l ll =
    match ll with
      | [] -> false
      | h::t -> if (equalStringList l h ) then true else existEquivalent l t;;

(* to remove lists that share the same element, for example [A;B] and [B;A]*)
let removeDupList x xs = 
    if (existEquivalent x xs) then xs else x::xs ;;

(* recursive remove equivalent lists *)
let removeDupListFromLeft xs = List.fold_right removeDupList xs [];;

(* find an element from lst1 that does not exist in lst2*)
let rec findUnique lst1 lst2=
    match lst1 with
      | [] -> raise (Failure "Not Found") (*This condition shall not appear*)
      | h::t -> if (List.mem h lst2) then findUnique t lst2 else h;;

(* same as zip function in python, combine two list together *)
let rec zip lst1 lst2=
    match (lst1,lst2) with
      | ([],[]) -> []
      | (x::xs, y::ys) -> (x,y) :: (zip xs ys)
      | _ -> raise (Failure "unmatched left and right hand side");;

(* the default value of lst1 and lst2 are [] *)
let rec unzip zippedList lst1 lst2= 
    match zippedList with
      | [] -> (List.rev lst1, List.rev lst2)
      | h::t -> unzip t ((fst h)::lst1) ((snd h)::lst2);;



(****************************************************************************************)
(*part2*)
(***implementation of getOrderedMutuallyRecursiveSets***)
(*
  The concept of Mutually Recursive Sets is mentioned at the end of Section 3 of the paper.
  It is considered an optimization for Fixed-point method and a necessarity for Newton's method.
*)

(* Returns a pair of directed graphs, each represented by a hashtable mapping a vertex to its children. 
   In the first graph, there's an edge from X to Y iff there's a rule that has X as the LHS and Y 
   somewhere on the RHS. The second graph is the transpose/reverse of the first. *)
let grammar_to_tables (rules : Rule.r list) : (string, string) Hashtbl.t * (string, string) Hashtbl.t =
    let (tbl, tbl_transpose) = (Hashtbl.create 100, Hashtbl.create 100) in
    let add_rule (t,tt) r =
        let lhs = get_nonterm r in
        match get_expansion r with
        | PublicTerminating s -> (t,tt)
        | PublicNonTerminating (nts, recipe) -> (
            List.iter (fun nt -> Hashtbl.add t lhs nt; Hashtbl.add tt nt lhs) (Nelist.to_list nts) ;
            (t,tt)
        )
    in
    List.fold_left add_rule (tbl, tbl_transpose) rules

let all_vertices (t : (string,string) Hashtbl.t) : string list =
    let add_vertex x acc =
        if List.mem x acc then
            acc
        else
            (x :: acc)
    in
    Hashtbl.fold (fun v1 v2 vs -> add_vertex v1 (add_vertex v2 vs)) t []

(* Based on Cormen et al., page 604. *)
(* TODO: Can be refactored; we don't need all these attributes, could use callbacks instead. *)
let dfs (t : ('a,'a) Hashtbl.t) (compare : 'a -> 'a -> int) : (('a,int) Hashtbl.t * ('a,'a) Hashtbl.t * 'a list) =

    let (cWHITE, cGREY, cBLACK) = (0, 1, 2) in
    let tblColor = Hashtbl.create 100 in
    let tblParent = Hashtbl.create 100 in
    let tblFinish = Hashtbl.create 100 in
    let roots = ref [] in
    let time = ref 0 in

    let vertices = List.sort compare (all_vertices t) in

    (* for each vertex, set color to white *)
    List.iter (fun v -> Hashtbl.add tblColor v cWHITE) vertices ;

    let rec dfs_visit u =
        incr time ;
        Hashtbl.add tblColor u cGREY ;
        List.iter (fun v ->     (* explore each edge (u,v) *)
            if Hashtbl.find tblColor v = cWHITE then (
                Hashtbl.add tblParent v u ;
                dfs_visit v
            ) else ()
        ) (List.sort compare (Hashtbl.find_all t u)) ;
        Hashtbl.add tblColor u cBLACK ;
        incr time ;
        Hashtbl.add tblFinish u (!time)
    in

    (* for each vertex, if it's white do dfs_visit *)
    List.iter (fun v -> if Hashtbl.find tblColor v = cWHITE then (roots := v::(!roots) ; dfs_visit v) else ()) vertices ;

    (tblFinish, tblParent, !roots)

(* Given a list of vertices and a table mapping each vertex to its 
   parent (if it has one in a forest), 
   return a list of lists that groups elements from the same tree together. *)
let components_from_forest (vertices : 'a list) (tblParent : ('a,'a) Hashtbl.t) (roots : 'a list) : ('a list list) =

    let tblChildren = Hashtbl.create 100 in
    Hashtbl.iter (fun c p -> Hashtbl.add tblChildren p c) tblParent ;

    let rec all_descendents v =
        let children = Hashtbl.find_all tblChildren v in
        v :: (concatmap_tr all_descendents children)
    in

    map_tr all_descendents roots

(* Computes a topologically sorted list of the vertices in a directed acyclic graph.
   The graph is specified by a list of vertices, and a function f such that `f x y' is 
   true iff there is an edge from x to y. *)
(* See Cormen et al., page 613. *)
(* TODO: This is a form of DFS, should be refactored to use the dfs function above. *)
let topological_sort (vertices : 'a list) (edge_check : 'a -> 'a -> bool) : 'a list =

    let result = ref [] in

    let rec visit u =
        let children = List.filter (edge_check u) vertices in
        List.iter (fun v ->
            if (not (List.mem v (!result))) then (
                visit v
            ) else ()
        ) children ;
        result := u::(!result)
    in

    List.iter (fun v ->
        if (not (List.mem v (!result))) then visit v else ()
    ) vertices ;

    reverse_tr (!result)

let getOrderedMutuallyRecursiveSets (rules : Rule.r list) : (string list list) =

    let (t,tt) = grammar_to_tables rules in

    (* Find the strongly connected components. See Cormen et al., page 617. *)
    let (tblFinish, tblParent, roots) = dfs t Stdlib.compare in
    let compare_decreasing_finish_time x y =
        0 - Stdlib.compare (Hashtbl.find tblFinish x) (Hashtbl.find tblFinish y)
    in
    let (tblFinish2, tblParent2, roots2) = dfs tt compare_decreasing_finish_time in
    let components = components_from_forest (List.sort Stdlib.compare (all_vertices t)) tblParent2 roots2 in

    let components_linked xs ys =
        (xs <> ys) && List.exists (fun x -> List.exists (fun y -> List.mem y (Hashtbl.find_all t x)) ys) xs
    in

    let sorted_components = topological_sort components components_linked in

    sorted_components

(****************************************************************************************)
(*part 3: implementation of Naive method and Newton's method*)

(*part3.1 storage table set up*) 
(* the type used in the table function storing the in-progress Z value for all terminals at depth k
 and the settled Z value for all terminals*)
type indicator= Depth of int | Settled;;


(* initialTable is the initial getZ function that takes an indicator
-(Depth int) if the approximate has not been reached
-(Settled) if the approximate has already been reached
and a string as the non-terminal 
and returns the float 0.0 *)
(* val initialTable : indicator -> string -> float = <fun> *)
let initialTable (k:indicator) (s:string)= 0.0;;


(*add is a function that takes in a getZ function and an indicator, a string and a float to return
a new getZ function that will add in the information of if getting that indicator and that string,
it will return that float*)
(* val add :
  (indicator -> string -> float) ->
  indicator -> string -> float -> indicator -> string ->float = <fun> *)
let add (getZ:indicator->string->float) (k:indicator) (s:string)(v:float)=
(*fun k' s'=0.0;*) 
fun k' s'->
    if k'=k && s'=s then v else getZ k' s';;

(****************************************************************************************)
(*part3.2*)
(*naive special function*)

(*
  this function returns a float representing the probability of one expansion of one nonterminal at depth k based on
  either probability of other sameSet nonterminals at depth (k-1)
  or probability of other non-sameSet nonterminals's settled approximate
*)
(* Namely, this function implements Equation 15 - 16 *)
let rec naiveOneNonTeminalOneRuleAtLevelK (k:int)(nonterminal:string)(onePair:(string list*float))
(someMiddleTable: indicator->string->float)(mutuallyRecursiveSets: string list list)=
(*return float;*)
match onePair with
|(h::t,f)-> (* (Printf.eprintf "I am here, the h is %s and the two floats are %f and %f \n" h (someMiddleTable Settled h)(naiveOneNonTeminalOneRuleAtLevelK k nonterminal (t,f) someMiddleTable mutuallyRecursiveSets));*)
        (* Equation 16 *)
        if (sameSet nonterminal h mutuallyRecursiveSets) then (someMiddleTable (Depth (k-1)) h)*. 
        (naiveOneNonTeminalOneRuleAtLevelK k nonterminal (t,f) someMiddleTable mutuallyRecursiveSets) 
        (* Equation 15 *)
        else (someMiddleTable Settled h)*. (naiveOneNonTeminalOneRuleAtLevelK k nonterminal (t,f) someMiddleTable mutuallyRecursiveSets)

|(_,f)-> f;;

(* This function implements Equation 7 *)
let rec naiveOneNonTeminalAtLevelK (k:int)(nonterminal:string)(alllist:(string list*float) list)
(someMiddleTable: indicator->string->float)(mutuallyRecursiveSets: string list list)
=match alllist with
                |(a,b)::t-> (* (Printf.eprintf "the head is %f and the float is %f \n" b 
                  (naiveOneNonTeminalOneRuleAtLevelK k nonterminal (a,b) someMiddleTable mutuallyRecursiveSets)); *)
                  (naiveOneNonTeminalOneRuleAtLevelK k nonterminal (a,b) someMiddleTable mutuallyRecursiveSets) +. 
                  (naiveOneNonTeminalAtLevelK k nonterminal t someMiddleTable mutuallyRecursiveSets)
                | _ -> 0.0

(****************************************************************************************)
(*part3.3 Newton's Method's special functions*)
(*part3.3.1*)
(* This section devotes to implement Equation 21 - 25 of the original paper *)

(* This function implements Equation 22 - 25*)
let rec getf (nonterminal: string)(nonterminalList: string list) (k:int)(someMiddleTable: indicator->string->float)
(mutuallyRecursiveSets: string list list)
(* return float *)
=match nonterminalList with 
		|[]->1.0 (* Equation 22 *)
		|(h::t)-> 
				let rest=getf nonterminal t k someMiddleTable mutuallyRecursiveSets in
				if (sameSet nonterminal h mutuallyRecursiveSets) then (someMiddleTable (Depth k) h)*.rest (* Equation 25 *)
				else (someMiddleTable Settled h)*.rest;; (* Equation 24 *)

(* This function implements Equation 21 *)
let rec getFForOneX (k: int) (nonterminal: string) (someMiddleTable: indicator->string->float)
(expansion: (string list * float) list) (mutuallyRecursiveSets: string list list)
=match expansion with
  (* Equation 21 second half (starting from Sigma) *)
	|((a,b)::t)-> (b *.(getf nonterminal a k someMiddleTable mutuallyRecursiveSets))+.(getFForOneX k nonterminal someMiddleTable t mutuallyRecursiveSets) 
	(* Equation 21 first half (-x_i) *)
  |([]) -> 0.0 -. (someMiddleTable (Depth k) nonterminal);; 

(* This function implements the third part of Equation 19, namely F(x^(k)) *)
let rec getFForVector (k: int)(oneSet: string list) (someMiddleTable: indicator->string->float)
(rules: Rule.r list) (mutuallyRecursiveSets: string list list)
(* return type = float list *)
=match oneSet with
	|[] -> []
	|(h::t) -> Util.append_tr [getFForOneX k h someMiddleTable (findRule h rules) mutuallyRecursiveSets] 
				(getFForVector k t someMiddleTable rules mutuallyRecursiveSets)

(****************************************************************************************)
(*part3.3.2*)
(* This section devotes to implement Equation 27 - 32 of the original paper *)

(* This function implements Equation 27 *)
let calculateDelta (i: string) (j: string)=
  if (i = j) then 1.0 else 0.0;;

(* 
   This function implements Equation 29 - 32  
   Notice that the equation No.32 in the original paper might be confusing. The index i for the delat(i,j) in equation 32 no longer
   stands for the nontermial, instead, it stands for the head of alpha(or in the equation, B).
*)
let rec getDFAlphaJ (alpha: string list)(i: string) (j: string) (k:int) (someMiddleTable: indicator -> string -> float)
(mutuallyRecursiveSets: string list list)=
  match (alpha, j) with
    |([], j) -> 0.0 (*Equation No.29*)
    |(h::t, j) ->
        let rest = getDFAlphaJ t i j k someMiddleTable mutuallyRecursiveSets in 
          if (sameSet i h mutuallyRecursiveSets) 
            then ((someMiddleTable (Depth k) h)*. rest) +. ((calculateDelta h j) *. (getf i t k someMiddleTable mutuallyRecursiveSets)) (* Equation No.32 *)
            else (someMiddleTable Settled h) *. rest;; (* Equation No. 31*)

(* This function implements Equation 28 *)
let rec getDFijHelper (i: string) (j: string) (k:int) (someMiddleTable: indicator->string->float) (allList:(string list*float) list)
(mutuallyRecursiveSets: string list list) (rules: Rule.r list)=
  match allList with
    |(a,b)::t ->  (* Second half of Equation 28 (starting from Sigma) *)
                  ((getDFAlphaJ a i j k someMiddleTable mutuallyRecursiveSets) *. b) +. 
                  (getDFijHelper i j k someMiddleTable t mutuallyRecursiveSets rules)
    |[] -> 0.0 -. (calculateDelta i j);; (* First half of Equation 28 (-delta(i,j)) *)

(* This function is simply reformatting the previous function so it can be used to generate Jacobian Matrix*)
let getDFij k someMiddleTable mutuallyRecursiveSets rules i j  = 
    getDFijHelper i j k someMiddleTable (findRule i rules) mutuallyRecursiveSets rules;;


(* Implement the Jacobian Matrix, namely Equation 20 *)
let getJFMatrix (k: int)(oneSet: string list) (someMiddleTable: indicator->string->float)
(rules: Rule.r list) (mutuallyRecursiveSets: string list list)
= Matrix.create_square_matrix oneSet (getDFij k someMiddleTable mutuallyRecursiveSets rules);;

(****************************************************************************************)
(*part3.3.3 this section devotes to fill in the table for Newton's method*) 

(*this table fills one set of non-terminals at depth k in the storage table*)
let rec fillTableForSetAtDepthKNewton (k: int)(oneSet: string list) (someMiddleTable: indicator->string->float)
(floatlist: float list)
=match (oneSet,floatlist) with 
	| (h1::t1,h2::t2) -> 
		(* if(k<10) then Printf.eprintf "at level %i for %s put in value %f \n" k h1 h2; (* testing function to see the value*) *)
	fillTableForSetAtDepthKNewton k t1 (add someMiddleTable (Depth k) h1 h2) t2
	| ([],[])->someMiddleTable
	| _->raise (Failure "string list and float lost don't match in length");;

(*helper function for getNewFloatListForSetAtLevelK. This functions get the values of non-terminals in one set at depth k into a float list.
It will be used when next depth (k+1) of the same set of non-terminals is calculated*)
let rec getFloatListForSetAtLevelKFromTable (k: int)(oneSet: string list) (someMiddleTable: indicator->string->float)
=match oneSet with 
	|(h::t)-> Util.append_tr [someMiddleTable (Depth k) h] (getFloatListForSetAtLevelKFromTable k t someMiddleTable)
	| [] -> [];;

(* This function implements Equation 19 *)
(* the float list returned is the list of values for non-terminals in a set at depth k*)
let getNewFloatListForSetAtLevelK (k: int)(oneSet: string list) (someMiddleTable: indicator->string->float)
(rules: Rule.r list) (mutuallyRecursiveSets: string list list)
= List.map2 (-.) (getFloatListForSetAtLevelKFromTable k oneSet someMiddleTable) (Matrix.mult_by_vec 
	(Matrix.invert (getJFMatrix k oneSet someMiddleTable rules mutuallyRecursiveSets))
	(getFForVector k oneSet someMiddleTable rules mutuallyRecursiveSets)
	)


(****************************************************************************************)
(*part 4*)
(* The following section implements the filling process of the table*)
(* Detailed and visualized information about the data structure will be in a seperate note file *)

(*put in values into the table for a set at depth k*)
(*this is where Newton's method and naive method takes apart*)
(* The most basic building block for *)
let rec oneSetAtLevelK mode (k: int)(oneSet: string list) (someMiddleTable: indicator->string->float) 
(rules: Rule.r list) (mutuallyRecursiveSets: string list list)
=(* fun (k':indicator) (s':string) -> 5.0;; *)
match mode with 
  |Naive->(match oneSet with
              | h::t -> 
              (* Printf.eprintf "I am adding the value %.20f for %s at level %i \n" (naiveOneNonTeminalAtLevelK mode k h (findRule h rules) someMiddleTable mutuallyRecursiveSets) h k; *)
              let newTable = (add someMiddleTable (Depth k) h 
                  (naiveOneNonTeminalAtLevelK k h (findRule h rules) someMiddleTable mutuallyRecursiveSets))  in
              (oneSetAtLevelK mode k t newTable rules mutuallyRecursiveSets)
              | _ -> someMiddleTable)
  |Newton->fillTableForSetAtDepthKNewton k oneSet someMiddleTable 
  	(getNewFloatListForSetAtLevelK (k-1) oneSet someMiddleTable rules mutuallyRecursiveSets);;
   

(*helper of oneSetWithInRangeAtLevelK, check whether one non-terminal has change below shreshold at depth k*)
let oneNonTerminalWithInRangeAtLevelK (r:float) (someMiddleTable: indicator->string->float) 
(k:int) (nonterminal:string)=
(* return type : bool *)
abs_float((someMiddleTable (Depth k) nonterminal)-.(someMiddleTable (Depth (k-1)) nonterminal))<=r 


(*whether every element in a given set has change below shreshold at depth k*)
let rec oneSetWithInRangeAtLevelK (r:float) (someMiddleTable: indicator->string->float) 
(k:int) (oneSet:string list)=
(* return type : bool *)
match oneSet with
| h::t -> if (oneNonTerminalWithInRangeAtLevelK r someMiddleTable k h) 
            then (oneSetWithInRangeAtLevelK r someMiddleTable k t)
        else
            false

| _ -> true;;


(*settle down the Z value for the set*)
let rec settleTableForSet (oneSet: string list) (k: int) (someMiddleTable: indicator->string->float)
=match oneSet with
| h::t-> 
(* Printf.eprintf "I am settling the value for %s at level %i: %.20f \n" h k (someMiddleTable (Depth k) h); *)
let updateTable= add someMiddleTable Settled h (someMiddleTable (Depth k) h) in
(settleTableForSet t k updateTable)
| _-> someMiddleTable;;

(*this function checks whether depth k has already fulfilled the threshold requrement for the set of non-terminals-- all the
difference of values in depth k and k-1 is below threshold, if yes, settle the table; if not, continue to incremenation of k*)
let rec oneSetCheckAtLevelKAndIncrement mode (k:int)(r:float)(oneSet: string list)(someMiddleTable: indicator->string->float)
(rules: Rule.r list) (mutuallyRecursiveSets: string list list)=
if (oneSetWithInRangeAtLevelK r someMiddleTable k oneSet) 
then (settleTableForSet oneSet k someMiddleTable)
else (oneSetCheckAtLevelKAndIncrement mode
(k+1) r oneSet 
(oneSetAtLevelK mode (k+1) oneSet someMiddleTable rules mutuallyRecursiveSets)
rules mutuallyRecursiveSets
);;

 
(* the settleOneSet is the helper function for fillInTableBySets 
This one calculates the Z value from depth 0 to depth k needed for a set of nonterminals and settle down their values
*)
let rec settleOneSet mode (oneSet: string list) (someMiddleTable: indicator->string->float) (rules: Rule.r list) 
(r: float)(mutuallyRecursiveSets: string list list)=
(* fun (k':indicator) (s':string) -> 5.0;; *)
(oneSetCheckAtLevelKAndIncrement mode
1 r oneSet 
(oneSetAtLevelK mode 1 oneSet someMiddleTable rules mutuallyRecursiveSets)
rules mutuallyRecursiveSets
);;


(*the fillInTableBySets takes in a ordered mutuallyRecursiveSets, an initialTable, rule list and threshold and
returns a newTable which can tell you what Z value each non-terminal has within the threshold*)
let rec fillInTableBySets mode (mutuallyRecursiveSets: string list list) (initialTable: indicator->string->float)
(rules: Rule.r list) (r: float) 
(*  = fun (k':indicator) (s':string) -> 5.0;; *)
=match mutuallyRecursiveSets with
|h::t-> let updateTable= (settleOneSet mode h initialTable rules r mutuallyRecursiveSets) in
(fillInTableBySets mode t updateTable rules r)
|_-> initialTable;;


(*this is the final function, returns the filled out table for all non-terminals*)
let getTable mode (rules: Rule.r list) (r: float)
(*  = fun (k':indicator) (s':string) -> 5.0;; *)
= fillInTableBySets mode (getOrderedMutuallyRecursiveSets rules) initialTable rules r ;;



(****************************************************************************************)
(*part5*)
(* The following section devotes to implement user interface*)

(* The arguments are an existing grammar (list of rules) to be normalized and that grammar's start symbol. 
 * The return value is a pair; the first member is the total probability mass associated with the start 
 * symbol in the argument grammar, and the second member is the list of rules with reweighted probabilities. 
 *)
let renormalize_grammar mode rules start_symbol =
    let threshold = 0.000000001 in
    let tbl = getTable mode rules threshold in
    let z nt = weight_from_float (tbl Settled nt) in
    let new_weight r =
        let lhs = get_nonterm r in
        match get_expansion r with
        | PublicTerminating _ -> div_weights (get_weight r) (z lhs)
        | PublicNonTerminating (nts,_) ->
            let z_nts = Nelist.fold mult_weights (Nelist.map z nts) in
            div_weights (mult_weights (get_weight r) z_nts) (z lhs)
    in
    let new_rules = map_tr (fun r -> Rule.reweight r (new_weight r)) rules in
    let new_rules_with_fixed_denoms = Grammar.ensure_common_denominators new_rules in
    (z start_symbol, new_rules_with_fixed_denoms)

let get_metadata grammar_file =
    let channel =
        try Some (open_in grammar_file)
        with Sys_error _ -> (Printf.eprintf "WARNING: Couldn't open file %s to retrieve metadata\n" grammar_file; None)
    in
    match channel with
    | None -> []
    | Some ch -> (
        let re = Str.regexp "^ *(\\*\\(.*\\)\\*) *$" in
        let result = ref [] in
        begin
        try
            while true; do
                let line = input_line ch in
                if (Str.string_match re line 0) then
                    result := ((Str.matched_group 1 line)::(!result))
                else
                    ()
            done
        with End_of_file -> close_in ch
        end ;
        reverse_tr (!result)
    )

let main () =
    let mode = ref Newton in
    let grammar_file = ref "" in
    let speclist = Arg.align([  ("-g",      Arg.Set_string(grammar_file),           " WMCFG grammar file (obligatory)") ;
                                ("-naive",  Arg.Unit(fun () -> mode := Naive),      " use naive method") ;
                                ("-newton", Arg.Unit(fun () -> mode := Newton),     " use Newton's method (default)") ;
                            ]) in
    let usage_msg = Printf.sprintf "Usage: %s -g <grammar file>" Sys.argv.(0) in
    let superfluous_arg s = raise (Arg.Bad (Printf.sprintf "Bad extra argument: %s" s)) in
    Arg.parse speclist superfluous_arg usage_msg ;
    if (!grammar_file = "") then (
        Printf.eprintf "Must provide a grammar file\n" ;
        Arg.usage speclist usage_msg
    ) else (
        let (rules,start_symbol) = Grammar.get_input_grammar (!grammar_file) in
        let m = Grammar.fertility_matrix (rules, start_symbol) in
        let spectral_radius = Matrix.spectral_radius m in 
        if (spectral_radius > 1.0) then (
            Printf.eprintf "\nGrammar is not consistent.\n" ;
            Matrix.print ~ch:stderr m
        ) else (
            if (spectral_radius = 1.0) then (
                Printf.eprintf "\nGrammar might be consistent ... ?\n" ;
            ) else (
                Printf.eprintf "\nGrammar is consistent.\n" ;
            ) ;
            Matrix.print ~ch:stderr m ;
            let (prob, new_rules) = renormalize_grammar (!mode) rules start_symbol in
            List.iter (Printf.printf "(*%s*)\n") (get_metadata (!grammar_file)) ;
            Printf.printf "(* \"probability = %.18f\" *)\n" (float_of_weight prob) ;
            List.iter (fun r -> Printf.printf "%s\n" (Rule.to_string r)) new_rules
        )
    )
let _ = if (!Sys.interactive) then () else main ()

