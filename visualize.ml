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

let check_exit_code code name =
	match code with
	| Unix.WEXITED 0 -> ()
	| Unix.WEXITED n -> Printf.eprintf "WARNING: %s exited with code %d\n" name n
	| Unix.WSIGNALED n -> Printf.eprintf "WARNING: %s was killed by signal number %d\n" name n
	| Unix.WSTOPPED n -> Printf.eprintf "WARNING: %s was stopped by signal number %d\n" name n

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
			check_exit_code (Unix.close_process_in channel) "Prolog shell command for getting dictionary"
	end ;
	result

(************************************************************************************************)

(* De-situates any situated nonterminals in an intersection grammar, then 
   cleans up some of the irrelevant mess introduced by the Guillaumin MCFG-generation:
     (1) removes (preterminal,leaf) pairs of the form (E,"")
     (2) converts preterminals like "t123_tmp1" to "t123"             *)
let clean_preterminal preterminal leaf =
	let new_leaf = (if leaf = " " then "" else leaf) in
	let new_preterminal = Grammar.desituate preterminal in
	if ((new_preterminal = "E") && (new_leaf = "")) then
		None
	else if (Str.string_match (Str.regexp "\([a-z]+[0-9]+\)_tmp[0-9]+") new_preterminal 0) then
		Some (Str.matched_group 1 new_preterminal, new_leaf)
	else
		Some (new_preterminal, new_leaf)

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

(* ids is a list of ints representing the "derivation string"; 
   i is an integer identifying this particular derivation among those sampled *)
let save_to_file grammar_files prolog_file ids i =
	let ids_as_string = "[" ^ (String.concat "," (List.map string_of_int ids)) ^ "]" in
	let channel =
		if not (Sys.file_exists prolog_file) then
			failwith (Printf.sprintf "Required prolog file does not exist: %s" prolog_file)
		else if not (Sys.file_exists grammar_files.mg_file) then
			failwith (Printf.sprintf "Required MG file does not exist: %s" grammar_files.mg_file)
		else
			let fmt = format_of_string "swipl -s %s -q -t \"['%s'], lparse(%s,D,B,X,_,_), user:latex_tree(X,'tree%03d').\" 2>/dev/null" in
			let command = Printf.sprintf fmt prolog_file grammar_files.mg_file ids_as_string i in
			try Unix.open_process_in command
			with _ -> failwith (Printf.sprintf "Error attempting to run shell command: %s" command)
	in
	(* We don't expect any output on stdout *)
	check_exit_code (Unix.close_process_in channel) "Prolog shell command for saving tree to file"

let run_visualization grammar_files prolog_file =
	let dict = get_guillaumin_dict grammar_files.dict_file in
	let index = get_stabler_index grammar_files prolog_file in
	Random.self_init () ;  (* initialise with a random seed *)
	let process_tree (tree,weight) i =
		print_endline "===============================================" ;
		Printf.printf "weight is %s\n" (string_of_num weight) ;
		let ids = get_derivation_string tree dict index in
		List.iter (Printf.printf "%d, ") ids ; print_newline () ;
		Printf.printf "Saving tree to file %d\n" i ;
		save_to_file grammar_files prolog_file ids i ;
		print_endline "===============================================" ;
	in
	let treelist = generate grammar_files.wmcfg_file in
	for i = 0 to 5 do
		process_tree (List.nth treelist i) i
	done

(************************************************************************************************)

let print_usage () =
	Printf.eprintf "\n" ;
	Printf.eprintf "Usage: %s <grammar-file>\n" Sys.argv.(0) ;
	Printf.eprintf "\n" ;
	Printf.eprintf "The grammar file should\n" ;
	Printf.eprintf "   EITHER (i)  be given as a path of the form $GRAMMARS/wmcfg/$NAME.wmcfg\n" ;
	Printf.eprintf "       OR (ii) contain a comment line identifying a weighted MCFG file from which it is derived\n" ;
	Printf.eprintf "               by intersection, as path of the form $GRAMMARS/wmcfg/$NAME.wmcfg\n" ;
	Printf.eprintf "\n" ;
	Printf.eprintf "In either case, the associated MG and dictionary files should be in the following locations:\n" ;
	Printf.eprintf "   $GRAMMARS/mg/$NAME.pl\n" ;
	Printf.eprintf "   $GRAMMARS/mcfgs/$NAME.dict\n" ;
	Printf.eprintf "\n"

(* Returns a pair of strings (grammars_dir, grammar_name), such that the relevant files are 
   grammars_dir/{mg,mcfgs/wmcfg}/grammar_name.{pl,wmcfg,dict} *)
let identify_original_grammar grammar_file =

	let channel =
		if not (Sys.file_exists grammar_file) then
			failwith (Printf.sprintf "Specified grammar file does not exist: %s" grammar_file)
		else
			(* Might as well be picky about this regex *)
			let command = Printf.sprintf "awk ' /^\(\* original grammar: [a-z\/\.]* \*\)/ {print $4} ' %s" grammar_file in
			try Unix.open_process_in command
			with _ -> failwith (Printf.sprintf "Error attempting to run shell command: %s" command)
	in

	let orig_grammar =
		let line1 = try Some (input_line channel) with End_of_file -> None in
		match line1 with
		| None -> grammar_file          (* No matching comments; try to use the grammar file itself *)
		| Some s ->
			let line2 = try Some (input_line channel) with End_of_file -> None in
			match line2 with
			| None -> s                 (* Exactly one matching comment; use that one *)
			| Some _ -> failwith (Printf.sprintf "Two distinct original grammars identified in %s, don't know what to do." grammar_file)
	in
	check_exit_code (Unix.close_process_in channel) "Shell command reading grammar file" ;

	(* Now we've got a guess at the original grammar file, let's try to parse it according to the pattern $GRAMMARS/wmcfg/$NAME.wmcfg *)
	let regex = Str.regexp "^\([a-zA-Z0-9\.-]+\)\/wmcfg\/\([a-zA-Z0-9\.-]+\)\.wmcfg$" in
	if (Str.string_match regex orig_grammar 0) then
		(Str.matched_group 1 orig_grammar, Str.matched_group 2 orig_grammar)
	else
		if (orig_grammar = grammar_file) then
			failwith (Printf.sprintf "No original grammar identified in %s (and this file itself is not in a location of the form $GRAMMARS/wmcfg/$NAME.wmcfg)" orig_grammar)
		else
			failwith (Printf.sprintf "Original grammar file identified as %s, but this is not of the form $GRAMMARS/wmcfg/$NAME.wmcfg" orig_grammar)

let main () =
	if (Array.length Sys.argv = 2) then (
		let grammar_file = Sys.argv.(1) in
		let (grammars_dir, grammar_name) = identify_original_grammar grammar_file in
		let prolog_file  = "mgcky-swi/setup.pl" in
		let grammar_files = { mg_file    = grammars_dir ^ "/mg/" ^ grammar_name ^ ".pl" ;
		                      wmcfg_file = grammar_file ;
		                      dict_file  = grammars_dir ^ "/mcfgs/" ^ grammar_name ^ ".dict"
		                    } in
		run_visualization grammar_files prolog_file
	) else
		print_usage ()

let _ = if (!Sys.interactive) then () else main ()
