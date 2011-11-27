
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

let get_yield (sentence : string list) (r : Util.range) : string =
	match r with
	| Pair (i,j) -> List.fold_left (^^) "" (map_tr (List.nth sentence) (range i j))
	| VarRange _ -> ""

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
 <:DEBUG< "%s\n" (String.concat "\n" (Chart.map_items chart (fun i -> Chart.to_string i sentence))) >> ;
 <:DEBUG< "Chart contains %d items, of which %d are goals\n" (Chart.length chart) (List.length goal_items) >> ;
  (if (List.length goal_items)>0 then 
    (Printf.printf "SUCCESS!\n";)
  else 
    Printf.printf "FAILED\n");
  List.iter (Util.debug "%s\n") result ;
  result

let print_grammar grammar_file prefix (rules, start_symbol) =
	Printf.printf "(* original grammar: %s *)\n" grammar_file ;
	Printf.printf "(* prefix: %s *)\n" prefix ;
	List.iter (fun r -> Printf.printf "%s\n" (Rule.to_string r)) rules

type options = { debug : bool ; graph : string option ; intersect : bool ; input : string list -> Parser.input ; sentence : string option }
let default_options = {debug = false ; graph = None; intersect = false; input = (fun s -> Parser.Sentence s); sentence = None }

let rec process_args args acc =
	match args with
	| [] -> acc
	| ("-d" :: rest)               -> process_args rest {acc with debug = true}
	| ("-graph" :: (filename :: rest))  -> process_args rest {acc with graph = Some filename}
	| ("-intersect" :: rest)  -> process_args rest {acc with intersect = true}
	| ("-p" :: rest)     -> process_args rest {acc with input = (fun s -> Parser.Prefix s); intersect = true}
        | ("-infix" :: rest) -> process_args rest {acc with input = (fun s -> Parser.Infix  s); intersect = true}
	| (x::rest)          -> process_args rest {acc with sentence = Some x}

let main () =
	match List.tl (Array.to_list Sys.argv) with
	| [] ->
		Printf.eprintf "Usage: %s grammar-file (-d) (-graph \"dot-output-file\")  (-intersect) (-p) (-infix) \"sentence\"\n" Sys.argv.(0);
		Printf.eprintf "Flags in parentheses are optional. -p and -infix each imply -intersect \n"
	| (x::xs) ->
		(* first arg is the grammar; the rest go to process_args *)
		let grammar_file = x in
		let options = process_args xs default_options in
		Util.set_debug_mode options.debug ;
		let (rules,start_symbol) as input_grammar = Grammar.get_input_grammar grammar_file in
		let (input_list,input_string,parser_argument) = match options.sentence with
			None -> failwith "No sentence given; nothing to do!"
		      | Some s -> (let x = Util.split ' ' s in
				   (x,s,options.input x)) in
		match (options.intersect,options.graph) with
		    (false,None) -> ignore (run_parser input_list input_grammar)
		  | (_,_) -> 
		      let chart = Parser.deduce (-1) rules parser_argument in
		      let goal_items = Chart.goal_items chart start_symbol (List.length input_list) in
		      begin
			(* user should be able to get an intersection grammar after parsing full sentences OR prefixes *)
			if options.intersect
			then print_grammar grammar_file input_string
			         (intersection_grammar chart goal_items start_symbol input_list)
			else ();
			(* ditto for graphs *)
			match options.graph with
			    (Some graphname) -> Grammar.drawgraph chart goal_items input_list graphname
			  | None -> ()
		      end

let _ = if (!Sys.interactive) then () else main () ;;


