
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

let print_tree tree =
	let rec print_tree' t =      (* returns a list of strings, each representing one line *)
		let item = Derivation.get_root_item t in
		let children = Derivation.get_children t in
		let yield =
			match (children, Rule.get_expansion (Derivation.get_rule t)) with
			| ([]    , PublicTerminating s) -> s
			| ((_::_), PublicNonTerminating _) -> "--"
			| _ -> failwith "Inconsistent tree in print_tree"
		in
		let first_line = Printf.sprintf "%s (%s) %s " (Chart.get_nonterm item) yield (show_weight (Derivation.get_weight t)) in
		let children_printed : (string list) = map_tr ((^) "    ") (List.concat (map_tr print_tree' children : (string list list))) in
		let all_lines : (string list) = first_line :: children_printed in
		all_lines
	in
	List.fold_right (Printf.sprintf("%s\n%s")) (print_tree' tree) ""

let print_sexp tree = 
  let interdigitate s t =
    if (s = "") || (t = "") then
      s ^ t
    else
      s ^ " , " ^ t   in
  let rec show_tree t =
    let item = Derivation.get_root_item t in
    let parent_label = Chart.get_nonterm item in
    let children = Derivation.get_children  t in
    match (List.length children) with
	0 -> parent_label
      | _ ->  ("<"^parent_label^"> "^ (List.fold_left interdigitate "" (List.map show_tree children)) ^ " </"^parent_label^"> ") in
  (show_tree tree)

let run_parser sentence (rules, start_symbol) =
  let chart = Parser.deduce (-1) rules (Parser.Sentence sentence) in
  let goal_items = Chart.goal_items chart start_symbol (List.length sentence) in
  let goal_derivations = List.concat (map_tr (Derivation.get_derivations chart) goal_items) in
 <:DEBUG< "%d goal items, %d goal derivations\n" (List.length goal_items) (List.length goal_derivations) >> ;
  let rec make_trees goals acc =
    match goals with
      [] -> acc
    | h::t ->  make_trees t ((print_tree h)::acc) in
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

type options = { debug : bool ; graph : string option ; kbest : int option ; sexp : bool ; intersect : bool ; input : string list -> Parser.input ; sentence : string option }
let default_options = {debug = false ; graph = None; kbest = None ; sexp = false; intersect = false; input = (fun s -> Parser.Sentence s); sentence = None }

let rec process_args args acc =
	match args with
	| [] -> acc
	| ("-d" :: rest)               -> process_args rest {acc with debug = true}
	| ("-graph" :: (filename :: rest))  -> process_args rest {acc with graph = Some filename}
	| ("-s" :: rest)               -> process_args rest {acc with sexp = true}
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
        List.iter (function (Some t) -> Printf.printf "%s\n" (print_tree t) | None -> Printf.printf "---\n") trees

(****************************************************************************************)

let main () =
	match List.tl (Array.to_list Sys.argv) with
	| [] ->
		Printf.eprintf "Usage: %s grammar-file (-d) (-graph \"dot-output-file\") (-s) (-intersect) (-p) (-infix) \"sentence\"\n" Sys.argv.(0);
		Printf.eprintf "\n" ;
		Printf.eprintf "  Flags in parentheses are optional.\n" ;
		Printf.eprintf "\n" ;
		Printf.eprintf "  Only one of the following three flags will take effect; if more than one is given, \n" ;
		Printf.eprintf "  -intersect trumps everything else, and -s trumps -kbest.\n" ;
		Printf.eprintf "      -intersect     Prints intersection grammar to stdout\n" ;
		Printf.eprintf "      -s             Prints s-expression to stdout\n" ;
		Printf.eprintf "      -kbest <k>     Reports the best <k> parses to stdout\n" ;
		Printf.eprintf "\n" ;
		Printf.eprintf "  Only one of the following two flags will take effect; if more than one is given, \n" ;
		Printf.eprintf "  the last one overrides all others. If neither is given, the string is treated as 'exact'.\n" ;
		Printf.eprintf "      -p             Treat the string to be parsed as a prefix\n" ;
		Printf.eprintf "      -infix         Treat the string to be parsed as an infix\n" ;
		Printf.eprintf "\n"
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
		match (options.intersect,options.graph,options.sexp,options.kbest) with
		    (false,None,false,None) -> ignore (run_parser input_list input_grammar)
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
			  (if options.sexp then 
			    let goal_derivations = List.concat (map_tr (Derivation.get_derivations chart) goal_items) in
			    let rec make_trees goals acc =
			      match goals with
				  [] -> acc
				| h::t ->  make_trees t ((print_sexp h)::acc) in
			    begin
			       (* print out each derivation *)
   			       List.iter print_string (make_trees goal_derivations []);
			      print_newline ()
			    end
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


