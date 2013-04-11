
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

let print_grammar grammar_file parser_arg (rules, start_symbol) =
	Printf.printf "(* original grammar: %s *)\n" grammar_file ;
	begin
	match parser_arg with
	| Parser.Prefix s   -> Printf.printf "(* intersected with prefix: %s *)\n" (String.concat " " s)
	| Parser.Infix s    -> Printf.printf "(* intersected with infix: %s *)\n" (String.concat " " s)
	| Parser.Sentence s -> Printf.printf "(* intersected with exact string: %s *)\n" (String.concat " " s)
	end ;
	List.iter (fun r -> Printf.printf "%s\n" (Rule.to_string r)) rules

type options = { graph : string option ; kbest : int option ; trees : bool ; intersect : bool ; input : string list -> Parser.input ; sentence : string option }
let default_options = { graph = None; kbest = None ; trees = false; intersect = false; input = (fun s -> Parser.Sentence s); sentence = None }

(* Removes spaces from the beginning and end of the string, 
 * and collapses adjacent spaces into a single space. *)
let cleanup_input s =
        let trimmed = Str.global_replace (Str.regexp "^ *\\| *$") "" s in
        let collapsed = Str.global_replace (Str.regexp " +") " " trimmed in
        collapsed

let rec process_args args acc =
	match args with
	| [] -> acc
	| ("-graph" :: (filename :: rest))  -> process_args rest {acc with graph = Some filename}
	| ("-trees" :: rest)               -> process_args rest {acc with trees = true}
	| ("-intersect" :: rest)  -> process_args rest {acc with intersect = true}
	| ("-p" :: rest)     -> process_args rest {acc with input = (fun s -> Parser.Prefix s)}
        | ("-infix" :: rest) -> process_args rest {acc with input = (fun s -> Parser.Infix  s)}
        | ("-kbest" :: (k :: rest)) -> process_args rest {acc with kbest = Some (int_of_string k)}
	| (x::rest)          -> process_args rest {acc with sentence = Some (cleanup_input x)}

(****************************************************************************************)

let print_kbest k chart start_symbol parser_argument =
        let fsa = match parser_argument with
                  | Parser.Infix xs    -> Fsa.Infix(List.length xs)
                  | Parser.Prefix xs   -> Fsa.Prefix(List.length xs)
                  | Parser.Sentence xs -> Fsa.Sentence(List.length xs)
        in
        let goal = goal_item start_symbol fsa in
        let trees = Derivation.get_n_best_from_chart k chart goal in
        List.iter (fun t -> Printf.printf "%s\n" (Derivation.print_tree_compact t)) trees

(****************************************************************************************)

let main () =
	match List.tl (Array.to_list Sys.argv) with
	| [] ->
		Printf.eprintf "\n" ;
		Printf.eprintf "Usage: %s grammar-file (-graph \"dot-output-file\") (-trees) (-intersect) (-kbest <k>) (-p) (-infix) \"sentence\"\n" Sys.argv.(0);
		Printf.eprintf "\n" ;
		Printf.eprintf "  Flags in parentheses are optional.\n" ;
		Printf.eprintf "\n" ;
		Printf.eprintf "  Only one of the following three flags will take effect.\n" ;
		Printf.eprintf "  If more than one is given, -intersect trumps everything else, and -trees trumps -kbest.\n" ;
		Printf.eprintf "      -intersect     Prints intersection grammar to stdout\n" ;
		Printf.eprintf "      -trees         Prints (all) derivation trees to stdout (not compatible with prefix or infix mode)\n" ;
		Printf.eprintf "      -kbest <k>     Prints the best <k> derivation trees to stdout\n" ;
		Printf.eprintf "\n" ;
		Printf.eprintf "  Only one of the following two flags will take effect. If more than one is given, \n" ;
		Printf.eprintf "  the last one overrides all others. If neither is given, the string is treated as 'exact'.\n" ;
		Printf.eprintf "      -p             Treat the string to be parsed as a prefix\n" ;
		Printf.eprintf "      -infix         Treat the string to be parsed as an infix\n" ;
		Printf.eprintf "\n" ;
		Printf.eprintf "  Others:\n" ;
		Printf.eprintf "      -graph <filename>    Write a graph of the chart to this file in DOT format\n" ;
		Printf.eprintf "\n"
	| (x::xs) ->
		(* first arg is the grammar; the rest go to process_args *)
		let grammar_file = x in
		let options = process_args xs default_options in
		let (rules,start_symbol) = Grammar.get_input_grammar grammar_file in
		let (input_list,input_string,parser_argument) = match options.sentence with
			None -> failwith "No sentence given; nothing to do!"
		      | Some s -> (let x = Str.split (Str.regexp_string " ") s in
				   (x,s,options.input x)) in
		match (options.intersect,options.graph,options.trees,options.kbest) with
		    (false,None,false,None) -> failwith "Please specify one of -intersect, -graph, -trees or -kbest"
		  | (_,_,_,_) -> 
		      let chart = Parser.deduce (-1) rules parser_argument in
		      let goal_items = Chart.goal_items chart start_symbol (List.length input_list) in
		      begin
			(* user should be able to get an intersection grammar after parsing full sentences OR prefixes *)
			if options.intersect
			then print_grammar grammar_file parser_argument
			         (intersection_grammar chart goal_items start_symbol input_list)
			else 
			  (if options.trees then 
			    match parser_argument with
			    | Parser.Sentence _ -> 
			         let goal_derivations = List.concat (map_tr (Derivation.get_derivations chart) goal_items) in
   			         List.iter print_endline (map_tr (Derivation.print_tree_sexp Chart.get_nonterm) goal_derivations)
   			    | _ -> failwith "-trees option is incompatible with prefix/infix mode"
			   else 
			     (match options.kbest with
			      | None -> ()
			      | Some k -> print_kbest k chart start_symbol parser_argument
			     )
			  );
			(* ditto for graphs *)
			match options.graph with
			    (Some graphname) -> Grammar.drawgraph chart goal_items input_list graphname
			  | None -> ()
		      end

let _ = if (!Sys.interactive) then () else main () ;;


