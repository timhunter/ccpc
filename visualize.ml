open Num
open Util
open Generate

type grammar_files = {mg_file : string ; wmcfg_file : string ; dict_file : string}

(************************************************************************************************
Tool for visualising the "expected" derivations of a weighted grammar; in particular for 
visualising the "expected" continuations of a prefix, based on the weighted intersection grammar 
produced from that prefix.
************************************************************************************************)

(************************************************************************************************)

(* Reads from the dict file a returns a mapping from guillaumin-generated 
   preterminals (eg. "t123") to feature sequences (eg. ":: =N D -f") *)
let get_guillaumin_dict filename =

	let channel =
		try open_in filename
		with Sys_error _ -> failwith (Printf.sprintf "Couldn't open dict file %s" filename)
	in

	(* This regex matches lines describing both lexical ("::") and non-lexical (":") categories. 
	   We only actually take notice of the lexical categories, but we try to match the others too, 
	   so that we can give a warning if it fails. *)
	let regex = Str.regexp "^\([a-z]+[0-9]+\) : (\(::?\) \(.*\))$" in

	let result = Hashtbl.create 100 in
	begin
		try
			while true; do
				let line = input_line channel in
				if (Str.string_match regex line 0) then
					let category   = Str.matched_group 1 line in
					let lex_or_not = Str.matched_group 2 line in
					let features   = Str.matched_group 3 line in
					if lex_or_not = "::" then (Hashtbl.add result category features) else ()
				else if (line <> "") then
					Printf.eprintf "WARNING: Ignoring unexpected line in dictionary file: %s\n" line
			done
		with End_of_file ->
			close_in channel
	end ;
	result

(************************************************************************************************)

(* Calls Stabler's prolog to get the IDs of each MG lexical item.
   Returns a mapping from (lexical-string, feature-sequence) to ID. *)
let get_stabler_index grammar_files prolog_file =
	let channel =
		if not (Sys.file_exists prolog_file) then
			failwith (Printf.sprintf "Required prolog file does not exist: %s" prolog_file)
		else if not (Sys.file_exists grammar_files.mg_file) then
			failwith (Printf.sprintf "Required MG file does not exist: %s" grammar_files.mg_file)
		else
			let command = Printf.sprintf "swipl -s %s -q -t \"['%s'], showLexicon\" 2>/dev/null" prolog_file grammar_files.mg_file in
			try Unix.open_process_in command
			with _ -> failwith (Printf.sprintf "Error attempting to run shell command: %s" command)
	in
	let result = Hashtbl.create 100 in
	let regex = Str.regexp "^\([0-9]+\)\. \[\(.*\)\]::\[\(.*\)\]$" in
	let remove_commas s = Str.global_replace (Str.regexp ", *") " " s in
	begin
		try
			while true; do
				let line = input_line channel in
				if (Str.string_match regex line 0) then
					let id = int_of_string (Str.matched_group 1 line) in
					let str = Str.matched_group 2 line in
					let features = remove_commas (Str.matched_group 3 line) in
					Hashtbl.add result (str,features) id
				else if (line <> "") then
					Printf.eprintf "WARNING: Ignoring unexpected line in prolog output: %s\n" line
			done
		with End_of_file ->
			let exit_status = Unix.close_process_in channel in
			match exit_status with
			| Unix.WEXITED 0 -> ()
			| Unix.WEXITED n -> Printf.eprintf "WARNING: Prolog shell command exited with code %d\n" n
			| Unix.WSIGNALED n -> Printf.eprintf "WARNING: Prolog shell command was killed by signal number %d\n" n
			| Unix.WSTOPPED n -> Printf.eprintf "WARNING: Prolog shell command was stopped by signal number %d\n" n
	end ;
	result

(************************************************************************************************)

(* Cleans up some of the irrelevant mess introduced by the Guillaumin MCFG-generation:
     (1) removes (preterminal,leaf) pairs of the form (E,"")
     (2) converts preterminals like "t123_tmp1" to "t123"             *)
let clean_preterminal preterminal leaf =
	let new_leaf = (if leaf = " " then "" else leaf) in
	if ((preterminal = "E") && (new_leaf = "")) then
		None
	else if (Str.string_match (Str.regexp "\([a-z]+[0-9]+\)_tmp[0-9]+") preterminal 0) then
		Some (Str.matched_group 1 preterminal, new_leaf)
	else
		Some (preterminal, new_leaf)

(* Returns the yield of a tree, as a list of (preterminal,leaf) pairs, each component being a string *)
(* eg. [("t123","John"), ("t234","saw"), ("t345","Mary")] *)
let rec get_yield tree =
	let (Node (root, children)) = tree in
	match children with
	| [] -> failwith (Printf.sprintf "Tree not of the right form: no preterminal above %s node" root)
	| [Node (child,[])] -> (match (clean_preterminal root child) with None -> [] | Some x -> [x])
	| _ -> List.concat (List.map get_yield children)

(* Returns a list of lexical-item-IDs, given a derivation tree *)
let get_derivation_string tree dict index =

	let yield = get_yield tree in    (* eg. [("t123","John"), ("t234","saw"), ("t345","Mary")] *)
	List.iter (fun (preterm,term) -> Printf.printf "%s:\"%s\"  " preterm term) yield ; print_newline () ;

	let preterm_to_features preterm =
		try Hashtbl.find dict preterm
		with Not_found -> failwith (Printf.sprintf "Couldn't find feature-sequence corresponding to preterm %s in the dictionary" preterm)
	in
	let yield_with_features = List.map (fun (preterm,term) -> (preterm_to_features preterm, term)) yield in

	let lookup_id features term =
		try Hashtbl.find index (term,features)
		with Not_found -> failwith (Printf.sprintf "Couldn't find an ID for lexical item (%s,%s)" term features)
	in
	let yield_as_ids = List.map (fun (features,term) -> lookup_id features term) yield_with_features in

	yield_as_ids

let run_visualization grammar_files prolog_file =
	let dict = get_guillaumin_dict grammar_files.dict_file in
	let index = get_stabler_index grammar_files prolog_file in
	Random.self_init () ;  (* initialise with a random seed *)
	let process_tree (tree,weight) =
		print_endline "===============================================" ;
		Printf.printf "weight is %s\n" (string_of_num weight) ;
		let ids = get_derivation_string tree dict index in
		List.iter (Printf.printf "%d, ") ids ; print_newline () ;
		print_endline "===============================================" ;
	in
	for i = 0 to 5 do
		process_tree (generate grammar_files.wmcfg_file)
	done

(************************************************************************************************)

let print_usage () =
	Printf.eprintf "\n" ;
	Printf.eprintf "Usage: %s <grammars-directory> <grammar-name> <stabler-prolog-directory>\n" Sys.argv.(0) ;
	Printf.eprintf "\n" ;
	Printf.eprintf "    Assumes the three relevant grammar-related files are in these locations:\n" ;
	Printf.eprintf "        <grammars-directory>/mg/<grammar-name>.pl\n" ;
	Printf.eprintf "        <grammars-directory>/mcfgs/<grammar-name>.dict\n" ;
	Printf.eprintf "        <grammars-directory>/wmcfg/<grammar-name>.wmcfg\n" ;
	Printf.eprintf "\n" ;
	Printf.eprintf "    A version of Ed Stabler's setup.pl file with ALL the grammar-loading lines \n" ;
	Printf.eprintf "    commented out should be here:\n" ;
	Printf.eprintf "        <stabler-prolog-directory>/setup.pl\n" ;
	Printf.eprintf "\n"

let main () =
	if (Array.length Sys.argv = 4) then (
		let grammars_dir = Sys.argv.(1) in
		let grammar_name = Sys.argv.(2) in
		let prolog_file  = Sys.argv.(3) ^ "/setup.pl" in
		let grammar_files = { mg_file    = grammars_dir ^ "/mg/" ^ grammar_name ^ ".pl" ;
		                      wmcfg_file = grammars_dir ^ "/wmcfg/" ^ grammar_name ^ ".wmcfg" ;
		                      dict_file  = grammars_dir ^ "/mcfgs/" ^ grammar_name ^ ".dict"
		                    } in
		run_visualization grammar_files prolog_file
	) else
		print_usage ()

let _ = if (!Sys.interactive) then () else main ()
