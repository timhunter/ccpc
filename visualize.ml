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

(* Types for representing ``derivation lists''. An element of a derivation list is either a marker 
   of a particular rule, or an encoding of a leaf of the derivation tree.
   In simple cases, where the result of Hale and Stabler 2005 is applicable, we don't need any rule 
   markers, and derivation lists are just lists of leaves. In other cases though, eg. adjunction, 
   Stabler's prolog code requires markers like '>>' and '<<' to be interspersed with these leaves 
   to disambiguate.
   Since Stabler's prolog code represents a leaf of a derivation tree with an int, the eventual 
   derivation list that we compute is of type int dlist. But we allow other types for leaf elements 
   for intermediate representations in the course of computing that final list.
   Intuitively: a "widget dlist" is a list of widgets interspersed with some rule markers. *)
type 'a dlist_element = RuleMarker of Rule.marked_mg_rule | DerivLeaf of 'a
type 'a dlist = ('a dlist_element) list

let rec dlist_map (f : 'a -> 'b) (lst : 'a dlist) : 'b dlist =
	let lifted_f x =
		match x with
		| RuleMarker r -> RuleMarker r
		| DerivLeaf x -> DerivLeaf (f x)
	in
	List.map lifted_f lst

(* Given a way to turn a thing of type 'a into a string, turns everything in the list into a string *)
let dlist_to_strings (f : 'a -> string) (lst : 'a dlist) : string list =
	let one_to_string = function
	| RuleMarker Rule.LeftAdjunction -> ">>"
	| RuleMarker Rule.RightAdjunction -> "<<"
	| DerivLeaf x -> f x
	in
	List.map one_to_string lst

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

(* Returns the yield of a tree, as a list of (preterminal,leaf) pairs, possibly interspersed with "rule markers" *)
(* eg. [DerivLeaf ("t123","John"); RuleMarker LeftAdjoin; DerivLeaf ("t234","saw"), DerivLeaf ("t345","Mary")] *)
let rec get_yield tree =
	match tree with
	| Leaf label -> failwith (Printf.sprintf "Malformed tree: Got to leaf %s without going via a unary preterminal" label)
	| NonLeaf (label, [], _) -> failwith (Printf.sprintf "Malformed tree: NonLeaf node (label %s) with no children" label)
	| NonLeaf (preterm, [Leaf term], _) -> (match (clean_preterminal preterm term) with None -> [] | Some x -> [DerivLeaf x])
	| NonLeaf (_, children, rule) ->
		(* Rule.get_marked_mg_rule unfortunately only works on unsituated rules. Unfortunately it can't desituate 
		   a rule it is passed itself, because that would create a circular dependency between Grammar and Rule modules. :-( *)
		match (Rule.get_marked_mg_rule (Grammar.desituate_rule rule)) with
		| None -> List.concat (List.map get_yield children)
		| Some mg_rule -> (RuleMarker mg_rule) :: (List.concat (List.map get_yield children))

let get_sentence tree =
        let yields = get_yield tree in 
        let rec get_words yields sentence =
               match yields with
                 | [] -> sentence
                 | (DerivLeaf (id, lex_item))::t -> get_words t (lex_item::sentence)
                 | (RuleMarker _)::t -> get_words t sentence in
        get_words yields []

(* Returns a list of lexical-item-IDs, given a derivation tree *)
let get_derivation_string tree dict index =

	let yield = get_yield tree in

	let preterm_to_features preterm =
		try Hashtbl.find dict preterm
		with Not_found -> failwith (Printf.sprintf "Couldn't find feature-sequence corresponding to preterm %s in the dictionary" preterm)
	in
	let yield_with_features = dlist_map (fun (preterm,term) -> (preterm_to_features preterm, term)) yield in

	let lookup_id features term =
		try Hashtbl.find index (term,features)
		with Not_found -> failwith (Printf.sprintf "Couldn't find an ID for lexical item (%s,%s)" term features)
	in
	let yield_as_ids = dlist_map (fun (features,term) -> lookup_id features term) yield_with_features in

	yield_as_ids

(* Apparently there is no simpler way ... *)
let get_timestamp () =
	let tm = Unix.localtime(Unix.gettimeofday ()) in
	Printf.sprintf "%04d-%02d-%02d %02d:%02d:%02d" (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1) tm.Unix.tm_mday tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec;;

(* derivations is a list of pairs; the first component is a derivation list, the second is a weight *)
let save_to_file random_seed grammar_files prolog_file (derivations : (int dlist * num) list) filename =
	(* The function derivation_as_string turns a pair like (0.234, [12,23,34]) in a string (readable as a prolog list) like "[0.234,12,23,34]" 
	   (and deals properly with rule markers in amongst the ids). *)
	(* The prolog code knows to treat the heads of these lists as a "note" to be printed out as is, and treat the tails as derivations *)
	let derivation_as_string (dlist,w) = "[" ^ (String.concat "," ((string_of_float (float_of_num w))::(dlist_to_strings string_of_int dlist))) ^ "]" in
	let derivations_as_string = "[" ^ (String.concat "," (List.map derivation_as_string derivations)) ^ "]" in
	let channel =
		if not (Sys.file_exists prolog_file) then
			failwith (Printf.sprintf "Required prolog file does not exist: %s" prolog_file)
		else if not (Sys.file_exists grammar_files.mg_file) then
			failwith (Printf.sprintf "Required MG file does not exist: %s" grammar_files.mg_file)
		else
			let intro_lines = [ "Here are some lines of latex code that go at the top of the file."; 
			                    "Other useful info can be added here.";
			                    "\\\\\\\\begin{itemize}" ;
			                    Printf.sprintf "\\\\\\\\item random seed: %d" random_seed ;
			                    Printf.sprintf "\\\\\\\\item WMCFG grammar file used for sampling: %s" grammar_files.wmcfg_file ;
			                    Printf.sprintf "\\\\\\\\item md5sum for this grammar file: %s" (Digest.to_hex (Digest.file grammar_files.wmcfg_file)) ;
			                    Printf.sprintf "\\\\\\\\item timestamp for generating this sample: %s" (get_timestamp ()) ;
			                    "\\\\\\\\end{itemize}" ;
			                  ] in
			let intro_lines_as_string = "[" ^ (String.concat "," (List.map (Printf.sprintf "'%s'") intro_lines)) ^ "]" in
			let fmt = format_of_string "swipl -s %s -q -t \"['%s'], parse_and_display(%s,%s,'%s').\" 2>/dev/null" in
			let command = Printf.sprintf fmt prolog_file grammar_files.mg_file intro_lines_as_string derivations_as_string filename in
			try Unix.open_process_in command
			with _ -> failwith (Printf.sprintf "Error attempting to run shell command: %s" command)
	in
	try
		while true; do
			Printf.printf "*** Output from Prolog tree-drawing: %s\n" (input_line channel)
		done
	with End_of_file ->
		check_exit_code (Unix.close_process_in channel) "Prolog shell command for saving trees to file"

let run_visualization grammar_files prolog_file kbest optional_seed =

	let dict = get_guillaumin_dict grammar_files.dict_file in
	let index = get_stabler_index grammar_files prolog_file in

	let random_seed =
		match optional_seed with
		| None -> Random.self_init () ; Random.int 1000
		| Some n -> n
	in
	Printf.printf "Using random seed %d\n" random_seed ;
	Random.init random_seed ;

	let treelist = generate grammar_files.wmcfg_file in
	let kbest_trees = take kbest treelist in (*List.map (fun i -> List.nth treelist i) (range 0 kbest) in*)

	(*** Just leaving this part in here for Sam, not actually relevant to what follows for now ***)
	let process_tree (tree,weight) =
		print_endline "===============================================" ;
		Printf.printf "weight is %s\n" (string_of_num weight) ;
                let sentence = Generate.get_sentence tree in 
                List.iter (fun item -> Printf.printf "%s " item) sentence;
		print_endline "\n===============================================" ;
	in
	List.iter process_tree kbest_trees ;
	(************************************************)

	let kbest_derivations = List.map (fun (t,w) -> (get_derivation_string t dict index,w)) kbest_trees in
	save_to_file random_seed grammar_files prolog_file kbest_derivations "trees.tex"

(************************************************************************************************)

let print_usage () =
	Printf.eprintf "\n" ;
	Printf.eprintf "Usage: %s <grammar-file> <number of trees> (<random-seed>)\n" Sys.argv.(0) ;
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
	if (Array.length Sys.argv = 3) || (Array.length Sys.argv = 4) then (
		let grammar_file = Sys.argv.(1) in
		let kbest = int_of_string Sys.argv.(2) in
		let random_seed = if (Array.length Sys.argv = 4) then (Some (int_of_string Sys.argv.(3))) else None in
		let (grammars_dir, grammar_name) = identify_original_grammar grammar_file in
		let prolog_file  = "mgcky-swi/setup.pl" in
		let grammar_files = { mg_file    = grammars_dir ^ "/mg/" ^ grammar_name ^ ".pl" ;
		                      wmcfg_file = grammar_file ;
		                      dict_file  = grammars_dir ^ "/mcfgs/" ^ grammar_name ^ ".dict"
		                    } in
		run_visualization grammar_files prolog_file kbest random_seed
	) else
		print_usage ()

let _ = if (!Sys.interactive) then () else main ()
