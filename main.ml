
(**************************************************************************)
(* NB: These commands refer to files, not modules.
       They still need to be written with an initial capital. *)
open Util
open Rule
open Grammar
open Parser
open Read 
open Chart
(* open Rational *)

let print_grammar grammar_file prefix (rules, start_symbol) =
	Printf.printf "(* original grammar: %s *)\n" grammar_file ;
	Printf.printf "(* prefix: %s *)\n" prefix ;
	List.iter (fun r -> Printf.printf "%s\n" (Rule.to_string r)) rules

type options = { debug : bool ; graph : string option ; kbest : int option ; trees : bool ; intersect : bool ; input : string list -> Parser.input ; sentence : string option }
let default_options = {debug = false ; graph = None; kbest = None ; trees = false; intersect = false; input = (fun s -> Parser.Sentence s); sentence = None }

let rec process_args args acc =
	match args with
	| [] -> acc
	| ("-d" :: rest)               -> process_args rest {acc with debug = true}
	| ("-graph" :: (filename :: rest))  -> process_args rest {acc with graph = Some filename}
	| ("-trees" :: rest)               -> process_args rest {acc with trees = true}
	| ("-intersect" :: rest)  -> process_args rest {acc with intersect = true}
	| ("-p" :: rest)     -> process_args rest {acc with input = (fun s -> Parser.Prefix s)}
        | ("-infix" :: rest) -> process_args rest {acc with input = (fun s -> Parser.Infix  s)}
        | ("-kbest" :: (k :: rest)) -> process_args rest {acc with kbest = Some (int_of_string k)}
	| (x::rest)          -> process_args rest {acc with sentence = Some x}

(****************************************************************************************)

let print_kbest k chart start_symbol input_list =

        let goal = goal_item start_symbol (List.length input_list) in
        let trees = map_tr (fun i -> Derivation.get_nth_best_derivation i chart [] goal) (range 1 (k+1)) in  (** Not a great way to do this! Will fix. *)
        Printf.printf "Here are the %d best derivations of item %s:\n" k (debug_str goal) ;
        List.iter (function (Some t) -> Printf.printf "%s\n" (Derivation.print_tree t) | None -> Printf.printf "---\n") trees

(****************************************************************************************)

let main () =
	match List.tl (Array.to_list Sys.argv) with
	| [] ->
		Printf.eprintf "\n" ;
		Printf.eprintf "Usage: %s grammar-file (-d) (-graph \"dot-output-file\") (-trees) (-intersect) (-kbest <k>) (-p) (-infix) \"sentence\"\n" Sys.argv.(0);
		Printf.eprintf "\n" ;
		Printf.eprintf "  Flags in parentheses are optional.\n" ;
		Printf.eprintf "\n" ;
		Printf.eprintf "  Only one of the following three flags will take effect.\n" ;
		Printf.eprintf "  If more than one is given, -intersect trumps everything else, and -trees trumps -kbest.\n" ;
		Printf.eprintf "      -intersect     Prints intersection grammar to stdout\n" ;
		Printf.eprintf "      -trees         Prints (all) derivation trees to stdout (not compatible with prefix or infix mode)\n" ;
		Printf.eprintf "      -kbest <k>     Prints the best <k> derivation trees to stdout (WARNING: UNSTABLE and/or VERY SLOW)\n" ;
		Printf.eprintf "\n" ;
		Printf.eprintf "  Only one of the following two flags will take effect. If more than one is given, \n" ;
		Printf.eprintf "  the last one overrides all others. If neither is given, the string is treated as 'exact'.\n" ;
		Printf.eprintf "      -p             Treat the string to be parsed as a prefix\n" ;
		Printf.eprintf "      -infix         Treat the string to be parsed as an infix\n" ;
		Printf.eprintf "\n" ;
		Printf.eprintf "  Others:\n" ;
		Printf.eprintf "      -graph <filename>    Write a graph of the chart to this file in DOT format\n" ;
		Printf.eprintf "      -d                   Debugging mode\n" ;
		Printf.eprintf "\n"
	| (x::xs) ->
		(* first arg is the grammar; the rest go to process_args *)
		let grammar_file = x in
		let options = process_args xs default_options in
		Util.set_debug_mode options.debug ;
		let (rules,start_symbol) = Grammar.get_input_grammar grammar_file in
		let (input_list,input_string,parser_argument) = match options.sentence with
			None -> failwith "No sentence given; nothing to do!"
		      | Some s -> (let x = Util.split ' ' s in
				   (x,s,options.input x)) in
		match (options.intersect,options.graph,options.trees,options.kbest) with
		    (false,None,false,None) -> failwith "Please specify one of -intersect, -graph, -trees or -kbest"
		  | (_,_,_,_) -> 
		      let chart = Parser.deduce (-1) rules parser_argument in
		      <:DEBUG< "%s\n" (String.concat "\n" (Chart.map_items chart (fun i -> Chart.debug_str_long i chart))) >> ;
		      let goal_items = Chart.goal_items chart start_symbol (List.length input_list) in
		      begin
			(* user should be able to get an intersection grammar after parsing full sentences OR prefixes *)
			if options.intersect
			then print_grammar grammar_file input_string
			         (intersection_grammar chart goal_items start_symbol input_list)
			else 
			  (if options.trees then 
			    match parser_argument with
			    | Parser.Sentence _ -> 
			         let goal_derivations = List.concat (map_tr (Derivation.get_derivations chart) goal_items) in
   			         List.iter print_endline (map_tr Derivation.print_tree goal_derivations)
   			    | _ -> failwith "-trees option is incompatible with prefix/infix mode"
			   else 
			     (match options.kbest with
			      | None -> ()
			      | Some k -> print_kbest k chart start_symbol input_list
			     )
			  );
			(* ditto for graphs *)
			match options.graph with
			    (Some graphname) -> Grammar.drawgraph chart goal_items input_list graphname
			  | None -> ()
		      end

let _ = if (!Sys.interactive) then () else main () ;;


