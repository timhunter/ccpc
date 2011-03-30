
(**************************************************************************)
(* NB: These commands refer to files, not modules.
       They still need to be written with an initial capital. *)
open Util
open Rule
open Deriver
open Parser
open Parse

(******************************************************************************************)

(*let sample_grammar =
	let f1 = [[(0,0);(0,1)]] in
	let f2 = [[(0,0)]; [(0,1)]] in
	let f3 = [[(0,0);(1,0)]; [(0,1);(1,1)]] in
	let f4 = [[(0,0)]; [(1,0)]] in
	let rules1 = map_tr Rule.create_nonterminating
					[ ("S", ["R"], f1) ;
					  ("R", ["X"], f2) ;
					  ("R", ["X";"R"], f3) ;
					  ("X", ["A";"A"], f4) ;
					  ("X", ["B";"B"], f4) ] in
	let rules2 = map_tr Rule.create_terminating [ ("A", "a") ; ("B", "b") ] in
	rules1 @ rules2*)

let sample_grammar =
  try 	
		let lexbuf = Lexing.from_channel stdin in
		Parse.mcfgrule Lexer.token lexbuf 
  with _ -> print_string "Can't parse input mcfg file\n"; exit 0

(******************************************************************************************)

module type DEDUCTIVE_SYSTEM =
	sig
		type prim  (* eg. a list of these makes up a grammar *)
		type item  (* the things we deduce *)
		type input (* once-off input, eg. a sentence or a prefix *)
		val get_axioms : prim list -> input -> item list
		val is_goal : input -> item -> bool
		val max_arity : prim list -> int
		val rule_arity : prim -> int
		val build_nary : prim list -> item list -> item list
	end ;;

module Deducer = functor (D : DEDUCTIVE_SYSTEM) ->
	struct

		(*** TODO: Clever things for getting stuff from the chart quickly ***)
		(***       Probably requires sacrificing some generality; TBD.    ***)
		let rec consequences max_depth prims recent_items old_items =
		        let all_existing_items = recent_items @ old_items in
			if max_depth = 0 then
				all_existing_items
			else
			       match recent_items with
					  [] -> old_items
					| (i::is) ->
						let rules_of_arity n = List.filter (fun rule -> D.rule_arity rule = n) prims in
						let new_items n = concatmap_tr (D.build_nary (rules_of_arity n)) (all_lists all_existing_items n) in
   						let all_new_items = concatmap_tr new_items (range 1 ((D.max_arity prims)+1)) in
		 				let useful_new_items = List.filter (fun x -> not (List.mem x all_existing_items)) all_new_items in
					      	consequences (max_depth-1) prims (is @ useful_new_items) (i :: old_items)

		let deduce max_depth prims input =
			let axioms = D.get_axioms prims input in
			consequences max_depth prims axioms []

	end;;

module MCFG_Derivation_Deducer = Deducer(MCFG_Deriver)
module MCFG_ParseGen_Deducer = Deducer(MCFG_ParserGen) 

(******************************************************************************************)
(* Extract the intersection grammar *)
(* See Albro's dissertation, appendix C section C.4 *)

(* Like map2 but each of the xs gets used with each of the ys *)
let all_combinations f xs ys =
	let exhaust x ys = map_tr (f x) ys in
	concatmap_tr (fun x -> exhaust x ys) xs

let rec cartesian lists =
	match lists with
	  [] -> [[]]
	| (l :: ls) -> all_combinations (fun x -> fun xs -> x :: xs) l (cartesian ls)

let rec build_symbol sym ranges =
	match ranges with
	  [] -> sym
	| ((p,q)::rs) -> build_symbol (sym ^ (Printf.sprintf "_%d%d" p q)) rs

let result_matches f input_ranges expected_result =
	try
		(expected_result = Rule.apply f input_ranges concat_ranges)
	with
		RangesNotAdjacentException -> false

let make_new_rule sit_nonterm rights func range_lists =
	let new_rights = List.map2 build_symbol rights range_lists in
	let new_agenda_items = List.map2 MCFG_ParserGen.create_item rights range_lists in
	(Rule.create_rule (sit_nonterm, new_rights, func), new_agenda_items)

(* NB: There is a "bug" in Albro's dissertation where he describes this algorithm.
       On page 293, where he says "check whether f is well-defined", it should read something like 
       "check whether f is well-defined and evaluates to the ranges in the trigger item".
 *)
let intersection_rules_per_rule all_items item rule =
	let sit_nonterm = build_symbol (MCFG_ParserGen.get_nonterm item) (MCFG_ParserGen.get_ranges item) in
	match Rule.get_expansion rule with
	| PublicTerminating str -> ([Rule.create_terminating (sit_nonterm, str)], [])
	| PublicNonTerminating (nts', func) -> (* ((nt,nts), func) -> *)
		let items_headed_by nt = List.filter (fun item -> (MCFG_ParserGen.get_nonterm item) = nt) all_items in
		let items_grouped = NEList.to_list (NEList.map items_headed_by nts') in
		let item_combinations = cartesian items_grouped in
		let ranges_from_item_comb items = map_tr MCFG_ParserGen.get_ranges items in
		let function_inputs = map_tr ranges_from_item_comb item_combinations in
		let defined_function_inputs = List.filter (fun input -> result_matches func input (MCFG_ParserGen.get_ranges item)) function_inputs in
		(* results_to_combine :: (Rule.r * MCFG_ParserGen.item list) list *)
		let results_to_combine = map_tr (make_new_rule sit_nonterm (NEList.to_list nts') func) defined_function_inputs in
		let new_rules = map_tr fst results_to_combine in
		let new_agenda_items = concatmap_tr snd results_to_combine in
		(new_rules, new_agenda_items)

let new_intersection_grammar_rules orig_grammar chart item =
	let relevant_rules = List.filter (fun rule -> (Rule.get_nonterm rule = MCFG_ParserGen.get_nonterm item)) orig_grammar in
	let results_to_combine = map_tr (intersection_rules_per_rule chart item) relevant_rules in               (* (rule list * item list) list *)
	let new_rules = concatmap_tr fst results_to_combine in
	let new_agenda_items = concatmap_tr snd results_to_combine in
	(new_rules, new_agenda_items)

let rec build_intersection_grammar orig_grammar chart (agenda,i) grammar_so_far =
	if (i >= List.length agenda) then
		grammar_so_far
	else
		let trigger = List.nth agenda i in
		let (new_rules, new_agenda_items) = new_intersection_grammar_rules orig_grammar chart trigger in
		build_intersection_grammar orig_grammar chart (uniques (agenda @ new_agenda_items), i+1) (grammar_so_far @ new_rules)

let intersection_grammar orig_grammar symbols =
	let chart = uniques (MCFG_ParserGen.deduce (-1) orig_grammar (MCFG_ParserGen.Prefix symbols)) in
	let goal_items = List.filter (MCFG_ParserGen.is_goal (MCFG_ParserGen.Sentence symbols)) chart in
	uniques (build_intersection_grammar orig_grammar chart (goal_items,0) [])

(******************************************************************************************)
(* Top-level stuff for testing *)

let derivations rules =
  MCFG_Derivation_Deducer.deduce 6 rules MCFG_Deriver.null_input



let parse rules symbols = 
(*  MCFG_ParseGen_Deducer.deduce (-1) rules (MCFG_ParserGen.Sentence symbols)*)
	MCFG_ParserGen.deduce (-1) rules (MCFG_ParserGen.Sentence symbols)


let parse_with_intersection prefix sentence =
	let new_grammar = intersection_grammar sample_grammar prefix in
	parse new_grammar sentence

(*
 * Bit of a hack here: the start symbol that makes a goal item a goal item depends on the length of the 
 * prefix, but the span that makes a goal item a goal item depends on the length of the sentence. So I 
 * have to put together a start symbol by hand out here and look at the insides of the items directly. 
 * Not sure what the best approach to fixing this is.
 *)

let run_test prefix sentence expected =
	let intersection_start_symbol = Printf.sprintf "S_0%d" (List.length prefix) in
	let is_goal item = ((MCFG_ParserGen.get_nonterm item) = intersection_start_symbol) && (MCFG_ParserGen.get_ranges item = [(0, List.length sentence)]) in
	let result = List.exists is_goal (parse_with_intersection prefix sentence) in
	let show_result res = if res then "IN" else "OUT" in
	let show_list l = "'" ^ (List.fold_left (^^) "" l) ^ "'" in
	Printf.printf "TEST:   %-10s %-15s \t" (show_list prefix) (show_list sentence);
	Printf.printf "Result: %-3s \tIntended: %-3s \t\t%s\n" (show_result result) (show_result expected) (if (expected = result) then "PASS!" else "FAIL")

let run_sanity_check sentence expected =
	let goal_items = List.filter (MCFG_ParserGen.is_goal (MCFG_ParserGen.Sentence sentence)) (parse sample_grammar sentence) in 
	let result = goal_items <> [] in
	let show_result res = if res then "IN" else "OUT" in
	let show_list l = "'" ^ (List.fold_left (^^) "" l) ^ "'" in
	Printf.printf "SANITY: %-15s \t\t" (show_list sentence);
	Printf.printf "Result: %-3s \tIntended: %-3s \t\t%s\n" (show_result result) (show_result expected) (if (expected = result) then "PASS!" else "FAIL")

let main =
        begin
(*	 let complete_derivations = List.filter (MCFG_Deriver.is_goal ()) (derivations sample_grammar) in
	Printf.printf "Pass?  %b\n" (List.mem (MCFG_Deriver.DerivItem("S", [["b"; "a"; "b"; "b"; "b"; "a"; "b"; "b"]])) complete_derivations) ; *)
	run_sanity_check ["a";"a";"b";"b"] false ; 
	run_sanity_check ["a";"b";"a";"b"] true ;
	run_sanity_check ["a";"a";"b";"a";"a";"b"] true ; 
	run_sanity_check ["a";"a";"b";"b";"a";"a"] false ;
	run_sanity_check ["a";"a";"b";"a";"a";"a";"b";"a"] true ;
	run_test ["a"] ["a";"b";"a";"b"] true ;
	run_test ["a"] ["b";"a";"b";"a"] false ;
	run_test ["a";"b"] ["a";"a";"b";"b"] false ;
	run_test ["a";"b"] ["a";"b";"b";"a";"b";"b"] true ;  
	(* run_test ["a";"b"] ["a";"b";"a";"a";"a";"b";"a";"a"] true ; *)
	end






