
(**************************************************************************)
(* NB: These commands refer to files, not modules.
       They still need to be written with an initial capital. *)
open Util
open Rule
open Grammar
open Parser
open Read 
open Chart
open Rational

let get_yield (sentence : string list) (r : (Util.range_item * Util.range_item)) : string =
	match r with
	| (RangeVal i, RangeVal j) -> List.fold_left (^^) "" (map_tr (List.nth sentence) (range i j))
	| (EpsVar, EpsVar) -> ""
	| _ -> failwith "Should never mix EpsVar with RangeVal"

let print_tree tree sentence =
	let rec print_tree' t =      (* returns a list of strings, each representing one line *)
		let item = Derivation.get_root_item t in
		let yields_list : (string list) = map_tr (get_yield sentence) (Chart.get_ranges item) in
		let yields_string : string = List.fold_left (^^) "" (map_tr (Printf.sprintf "'%s'") yields_list) in
		let first_line = Printf.sprintf "%s (%s) " (Chart.get_nonterm item) yields_string in
		let children = Derivation.get_children t in
		let children_printed : (string list) = map_tr ((^) "    ") (List.concat (map_tr print_tree' children : (string list list))) in
		let all_lines : (string list) = first_line :: children_printed in
		all_lines
	in
	List.fold_right (Printf.sprintf("%s\n%s")) (print_tree' tree) ""

let run_parser sentence (rules, start_symbol) =
  let chart = Parser.deduce (-1) rules (Parser.Sentence sentence) in
  let goal_items = Chart.goal_items chart start_symbol (List.length sentence) in
  let goal_derivations = List.concat (map_tr (Derivation.get_derivations chart) goal_items) in
  <:DEBUG< "%d goal items, %d goal derivations\n" (List.length goal_items) (List.length goal_derivations) >> ;
  let rec make_trees goals acc =
    match goals with
      [] -> acc
    | h::t ->  make_trees t ((print_tree h sentence)::acc) in
  let result = make_trees goal_derivations [] in
  Chart.iter_items chart (fun x -> <:DEBUG< "%s\n" (Chart.to_string x sentence) >>) ;
  Util.debug "Chart contains %d items, of which %d are goals\n" (Chart.length chart) (List.length goal_items) ;
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
		let input_grammar = (Grammar.get_input_grammar grammar_file, "S") in
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
