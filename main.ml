
(**************************************************************************)
(* NB: These commands refer to files, not modules.
       They still need to be written with an initial capital. *)
open Util
open Rule
(*open Deriver*)
open Parser
open Read 
open Chart
open Rational

(******************************************************************************************)


let get_input_grammar grammar_file =
  try 
    let channel = open_in grammar_file in 
    let lexbuf = Lexing.from_channel channel in 
    Read.mcfgrule Lexer.token lexbuf  
  with _ -> print_string ("Can't parse input mcfg file "^grammar_file^"\n"); []

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
  | ((RangeVal p, RangeVal q)::rs) -> build_symbol (sym ^ (Printf.sprintf "_%d-%d" p q)) rs
  | ((EpsVar, EpsVar)::rs) -> build_symbol (sym ^ (Printf.sprintf "Epsilon")) rs
  | _ -> failwith "Should not be mixing RangeVal with EpsVar!"

let result_matches f input_ranges expected_result =
  try
    (expected_result = Rule.apply f input_ranges concat_ranges)
  with
    RangesNotAdjacentException -> false

let make_new_rule sit_nonterm rights func range_lists weight =
  let new_rights = List.map2 build_symbol rights range_lists in
  let new_agenda_items = List.map2 (fun x y -> Chart.create_item x y None weight) rights range_lists in
  (Rule.create_rule (sit_nonterm, new_rights, func, weight), new_agenda_items)

(* NB: There is a "bug" in Albro's dissertation where he describes this algorithm.
       On page 293, where he says "check whether f is well-defined", it should read something like 
       "check whether f is well-defined and evaluates to the ranges in the trigger item".
 *)
let intersection_rules_per_rule prefix all_items item rule =
  let sit_nonterm = build_symbol (Chart.get_nonterm item) (Chart.get_ranges item) in
  match Rule.get_expansion rule with
  | PublicTerminating str ->
    ( match (Chart.get_ranges item) with
      | [(RangeVal i, RangeVal j)] -> if (i < j) && (not (List.map (List.nth prefix) (range i j) = [str])) then
                                         ([],[])
                                      else
                                         ([Rule.create_terminating (sit_nonterm, str, (Rule.get_weight rule))], [])
      | _ -> ([Rule.create_terminating (sit_nonterm, str, (Rule.get_weight rule))], [])
    )
  | PublicNonTerminating (nts', func) -> (* ((nt,nts), func) -> *)
    let items_headed_by nt = List.filter (fun item -> (Chart.get_nonterm item) = nt) all_items in
    let items_grouped = Nelist.to_list (Nelist.map items_headed_by nts') in
    let item_combinations = cartesian items_grouped in
    let ranges_from_item_comb items = map_tr Chart.get_ranges items in
    let function_inputs = map_tr ranges_from_item_comb item_combinations in
    let defined_function_inputs = List.filter (fun input -> result_matches func input (Chart.get_ranges item)) function_inputs in
    (* results_to_combine :: (Rule.r * Parser.item list) list *)
    let results_to_combine = map_tr (fun x -> (make_new_rule sit_nonterm (Nelist.to_list nts') func x (Rule.get_weight rule))) defined_function_inputs in
    let new_rules = map_tr fst results_to_combine in
    let new_agenda_items = concatmap_tr snd results_to_combine in
    (new_rules, new_agenda_items)

let new_intersection_grammar_rules orig_grammar prefix chart item =
  let relevant_rules = List.filter (fun rule -> (Rule.get_nonterm rule = Chart.get_nonterm item)) orig_grammar in
  let results_to_combine = map_tr (intersection_rules_per_rule prefix chart item) relevant_rules in               (* (rule list * item list) list *)
  let new_rules = concatmap_tr fst results_to_combine in
  let new_agenda_items = concatmap_tr snd results_to_combine in
  (new_rules, new_agenda_items)

let rec build_intersection_grammar orig_grammar prefix chart (agenda,i) grammar_so_far =
  if (i >= List.length agenda) then
    grammar_so_far
  else
    let trigger = List.nth agenda i in
    let (new_rules, new_agenda_items) = new_intersection_grammar_rules orig_grammar prefix chart trigger in
    build_intersection_grammar orig_grammar prefix chart (uniques (agenda @ new_agenda_items), i+1) (grammar_so_far @ new_rules)

let intersection_grammar (rules, start_symbol) symbols =
  let chart = uniques (Parser.deduce (-1) rules (Parser.Prefix symbols)) in
  let goal_items = List.filter (Parser.is_goal start_symbol (List.length symbols)) chart in
  let new_start_symbol = Printf.sprintf "%s_0%d" start_symbol (List.length symbols) in
  let new_rules = build_intersection_grammar rules symbols chart (goal_items,0) [] in
  (new_rules, new_start_symbol)

(******************************************************************************************)
(* Top-level stuff for testing *)





let get_yield (sentence : string list) (r : (Util.range_item * Util.range_item)) : string =
	match r with
	| (RangeVal i, RangeVal j) -> List.fold_left (^^) "" (map_tr (List.nth sentence) (range i j))
	| (EpsVar, EpsVar) -> ""
	| _ -> failwith "Should never mix EpsVar with RangeVal"

let print_tree item sentence =
	let get_children item =
		match (Chart.get_backpointer item) with
		| None -> []
		| Some (Some x, None) -> [!x]
		| Some (Some x, Some y) -> [!x;!y]
		| _ -> failwith "Invalid parent backpointer"
	in
	let rec print_item item =      (* print_item returns a list of strings, each representing one line *)
		let yields_list : (string list) = map_tr (get_yield sentence) (Chart.get_ranges item) in
		let yields_string : string = List.fold_left (^^) "" (map_tr (Printf.sprintf "'%s'") yields_list) in
		let first_line = Printf.sprintf "%s (%s) " (Chart.get_nonterm item) yields_string in
		let child_items : (item list) = get_children item in
		let child_subtrees : (string list) = map_tr ((^) "    ") (List.concat (map_tr print_item child_items : (string list list))) in
		let all_lines : (string list) = first_line :: child_subtrees in
		all_lines
	in
	List.fold_right (Printf.sprintf("%s\n%s")) (print_item item) ""

let run_parser sentence (rules, start_symbol) =
  let chart = Parser.deduce (-1) rules (Parser.Sentence sentence) in
  let goal_items = List.filter (Parser.is_goal start_symbol (List.length sentence)) chart in 
  let rec make_trees goals acc =
    match goals with
      [] -> acc
    | h::t ->  make_trees t ((print_tree h sentence)::acc) in
  let result = make_trees goal_items [] in
  List.iter (fun x -> Util.debug "%s\n" (Chart.to_string x sentence)) chart ;
  Util.debug "Chart contains %d items, of which %d are goals\n" (List.length chart) (List.length goal_items) ;
  (if (List.length goal_items)>0 then 
    (Printf.printf "SUCCESS!\n";)
  else 
    Printf.printf "FAILED\n");
  List.iter (Util.debug "%s\n") result ;
  result

let print_grammar (rules, start_symbol) =
	List.iter (fun r -> Printf.printf "%s\n" (Rule.to_string r)) rules

type options = { debug : bool ; prefix : string option ; sentence : string option ; output_file : string option }
let default_options = {debug = false ; prefix = None ; sentence = None ; output_file = None }

let rec process_args args acc =
	match args with
	| [] -> acc
	| ("-d" :: rest)        -> process_args rest {acc with debug = true}
	| ("-p" :: (p :: rest)) -> process_args rest {acc with prefix = Some p}
	| ("-o" :: (o :: rest)) -> process_args rest {acc with output_file = Some o}
	| (x::rest)             -> process_args rest {acc with sentence = Some x}

let main () =
	match List.tl (Array.to_list Sys.argv) with
	| [] ->
		Printf.eprintf "Usage: mcfg grammar-file (-o output-file) (-d) (-p \"prefix\") \"sentence\"\n";
		Printf.eprintf "Flags in parentheses are optional\n"
	| (x::xs) ->
		(* first arg is the grammar; the rest go to process_args *)
		let grammar_file = x in
		let options = process_args xs default_options in
		if (options.sentence = None) && (options.prefix = None) then failwith "No prefix or sentence given; nothing to do!" ;
		Util.set_debug_mode options.debug ;
		let input_grammar = (get_input_grammar grammar_file, "S") in
		let grammar_for_parsing =
			match options.prefix with
			| None -> input_grammar ;
			| Some p -> intersection_grammar input_grammar (Util.split ' ' p)
		in
		match options.sentence with
		| None -> print_grammar grammar_for_parsing
		| Some s ->
			let result = run_parser (Util.split ' ' s) grammar_for_parsing in
			match options.output_file with
			| None -> ignore result
			| Some o ->
				(*** I don't really understand what's going on here, it's just copied from the previous version of the main function ***)
				let oc = open_out "maketree.pl" in
				Printf.fprintf oc "tikz_qtree(%s, '%s')." (List.nth result 0) o;
				close_out oc;
				let exit_code = Sys.command "prolog -q -s tikz_qtreeSWI.pl < maketree.pl" in
				if exit_code = 1 then Printf.eprintf "Error running tree drawer" ;
				let exit_code = Sys.command "rm maketree.pl" in 
				if exit_code = 1 then Printf.eprintf "Error deleting prolog tree file" ;
				()

let _ = if (!Sys.interactive) then () else main () ;;
