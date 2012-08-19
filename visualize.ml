open Num
open Util
open Generate

type grammar_files = {mg_file : string ; wmcfg_file : string ; dict_file : string}

type mode = KBest | Sample

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

(* Assumes that the result is meant to be unique, and returns None if there is not exactly one line produced. *)
let get_comment_data grammar_file filter_command =
        let channel =
                if not (Sys.file_exists grammar_file) then
                        failwith (Printf.sprintf "Specified grammar file does not exist: %s" grammar_file)
                else
                        let command = Printf.sprintf "cat %s | %s" grammar_file filter_command in
                        try Unix.open_process_in command
                        with _ -> failwith (Printf.sprintf "Error attempting to run shell command: %s" command)
        in
        let results =   (* will be a list of all lines that came back from running the command *)
                let acc = ref [] in
                try
                        while true do
                                acc := (input_line channel)::(!acc)
                        done ;
                        !acc
                with End_of_file -> !acc
        in
        check_exit_code (Unix.close_process_in channel) "Shell command reading grammar file" ;
        match results with
        | (x::[]) -> Some x
        | _ -> None

(************************************************************************************************)

(* Reads from the dict file a returns a mapping from guillaumin-generated 
   preterminals (eg. "t123") to feature sequences (eg. ":: =N D -f") *)
let get_guillaumin_dict filename =

	let channel =
		try open_in filename
		with Sys_error _ -> failwith (Printf.sprintf "Couldn't open dict file %s" filename)
	in

	(* This regex matches lines describing both lexical ("::") and non-lexical (":") categories. 
	   We record both in the dictionary, without recording the distinction anywhere. Therefore 
	   some feature-sequences appear twice in the range of this mapping, 
	   eg. if t123 represents ":: D" and t234 represents ": D", both will simply be mapped to "D" here. *)
	let regex = Str.regexp "^\([a-z]+[0-9]+\) : (::? \(.*\))$" in

	let result = Hashtbl.create 100 in
	begin
		try
			while true; do
				let line = input_line channel in
				if (Str.string_match regex line 0) then
					let category   = Str.matched_group 1 line in
					let features   = Str.matched_group 2 line in
					Hashtbl.add result category features
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
let rec get_yield dict tree =
	match tree with
	| Leaf label -> failwith (Printf.sprintf "Malformed tree: Got to leaf %s without going via a unary preterminal" label)
	| NonLeaf (label, [], _) -> failwith (Printf.sprintf "Malformed tree: NonLeaf node (label %s) with no children" label)
	| NonLeaf (preterm, [Leaf term], _) -> (match (clean_preterminal preterm term) with None -> [] | Some x -> [DerivLeaf x])
	| NonLeaf (_, children, rule) ->
		(* Rule.get_marked_mg_rule unfortunately only works on unsituated rules. Unfortunately it can't desituate 
		   a rule it is passed itself, because that would create a circular dependency between Grammar and Rule modules. :-( *)
		match (Rule.get_marked_mg_rule dict (Grammar.desituate_rule rule)) with
		| None -> List.concat (List.map (get_yield dict) children)
		| Some mg_rule -> (RuleMarker mg_rule) :: (List.concat (List.map (get_yield dict) children))

(* Returns a list of lexical-item-IDs, given a derivation tree *)
let get_derivation_string tree dict index =

	let yield = get_yield dict tree in

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

(* We report the number of parses that have probability greater than this threshold *)
let const_THRESHOLD = div_num (num_of_int 1) (num_of_int 1000)

(* derivations is a list of pairs; the first component is a derivation list, the second is a weight *)
let save_to_file mode_note grammar_files prolog_file (derivations : (int dlist * num) list) filename =

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
			                    Printf.sprintf "\\\\\\\\item %s" mode_note ;
			                    Printf.sprintf "\\\\\\\\item WMCFG grammar file: %s" grammar_files.wmcfg_file ;
			                    Printf.sprintf "\\\\\\\\item md5sum for this grammar file: %s" (Digest.to_hex (Digest.file grammar_files.wmcfg_file)) ;
			                    Printf.sprintf "\\\\\\\\item timestamp: %s" (get_timestamp ()) ;
			                    "\\\\\\\\end{itemize}" ;
			                  ] in
			let intro_lines_as_string = "[" ^ (String.concat "," (List.map (Printf.sprintf "'%s'") intro_lines)) ^ "]" in
			let entropy_note = match (get_comment_data grammar_files.wmcfg_file "sed 's/\\\"//g' | awk '/^\(\* entropy = [0-9\.]* \*\)/ {print $4}'") with
			                   | None -> "Could not find entropy"
			                   | Some s -> try Printf.sprintf "Entropy = %.3f, with %d parses above %.3f"
			                                                  (float_of_string s)
			                                                  (List.length (take_while (fun (_,w) -> w >/ const_THRESHOLD) derivations))
			                                                  (float_of_num const_THRESHOLD)
			                               with _ -> "Could not find entropy"
			in
			let prefix_note = match (get_comment_data grammar_files.wmcfg_file
			                    "awk ' /^\(\* intersected with prefix: .* \*\)/ {$1=$2=$3=$4=\"\"; $NF=\"\"; print $0}'") with
			                  | None -> "Could not find intersection prefix"
			                  | Some s -> Printf.sprintf "Prefix: %s" s
			in
			let fmt = format_of_string "swipl -s %s -q -t \"['%s'], parse_and_display(%s,'%s','%s',%s,'%s').\" 2>/dev/null" in
			let command = Printf.sprintf fmt prolog_file grammar_files.mg_file intro_lines_as_string prefix_note entropy_note derivations_as_string filename in
			try Unix.open_process_in command
			with _ -> failwith (Printf.sprintf "Error attempting to run shell command: %s" command)
	in
	try
		while true; do
			Printf.eprintf "*** Output from Prolog tree-drawing: %s\n" (input_line channel)
		done
	with End_of_file ->
		check_exit_code (Unix.close_process_in channel) "Prolog shell command for saving trees to file" ;

        (* Unfortunate necessary hack: escape any underscores in the latex file produced by prolog *)
        let channel' =
                let command = Printf.sprintf "sed -i 's/_/\\\\_/g' %s" filename in
                try Unix.open_process_in command
                with _ -> failwith (Printf.sprintf "Error attempting to run shell command: %s" command)
        in
        check_exit_code (Unix.close_process_in channel') "sed shell command for escaping underscores in latex"

(* Converts a tree of type:  string Derivation.derivation_tree
 * into a pair of type:     (string Generate.tree * Num.num)
 * FIXME: This is fairly ugly. We should eliminate one or the other tree type altogether. *)
let rec convert_tree (dtree : string Derivation.derivation_tree) : (string Generate.tree * Num.num) =
        let (children, root) = (Derivation.get_children dtree, Derivation.get_root_item dtree) in
        let new_tree =
                match children with
                | [] -> let str = match (Rule.get_expansion (Derivation.get_rule dtree)) with
                                  | Rule.PublicTerminating s -> s
                                  | _ -> failwith "convert_tree: Node has no children but not a terminating rule" in
                        NonLeaf (root, [Leaf str], Derivation.get_rule dtree)
                | _ -> NonLeaf (root, map_tr fst (map_tr convert_tree children), Derivation.get_rule dtree)
        in
        let weight_as_num =
                let w = Derivation.get_weight dtree in
                match (weight_numerator w, weight_denominator w) with
                | (Some n, Some d) -> Num.div_num n d
                | otherwise -> failwith "convert_tree: This tree is unweighted"
        in
        (new_tree, weight_as_num)

let run_visualization grammar_files prolog_file num_trees output_filename mode optional_seed =

	let dict = get_guillaumin_dict grammar_files.dict_file in
	let index = get_stabler_index grammar_files prolog_file in

        let (trees,mode_note) =
                match mode with
                | KBest ->
                        if (optional_seed <> None) then Printf.eprintf "*** WARNING: using kbest mode, so ignoring random seed\n" ;
                        let (rules, start_symbol) = Grammar.get_input_grammar grammar_files.wmcfg_file in
                        let derivation_trees = Derivation.get_n_best_from_grammar num_trees rules start_symbol in  (* of the type declared in derivation.ml *)
                        (map_tr convert_tree derivation_trees, "exact k-best enumeration of most likely derivations")
                | Sample ->
                        begin
                        let random_seed =
                                match optional_seed with
                                | None -> Random.self_init () ; Random.int 1000
                                | Some n -> n
                        in
                        Printf.eprintf "Using random seed %d\n" random_seed ;
                        Random.init random_seed ;
                        (take num_trees (generate grammar_files.wmcfg_file), Printf.sprintf "randomly sampled derivations with random seed %d" random_seed)
                        end
        in

	let process_tree (tree,weight) =
                let sentence = Generate.get_sentence tree in 
                Printf.printf "%.6g\t%s\n" (float_of_num weight) (String.concat " " sentence) ;
	in
	List.iter process_tree trees ;

	let derivations = List.map (fun (t,w) -> (get_derivation_string t dict index,w)) trees in
	save_to_file mode_note grammar_files prolog_file derivations output_filename

(************************************************************************************************)

let print_usage () =
	Printf.eprintf "\n" ;
	Printf.eprintf "Usage: %s (-kbest|-sample) <grammar file> <number of trees> <output latex file> (<random seed>)\n" Sys.argv.(0) ;
	Printf.eprintf "\n" ;
	Printf.eprintf "Exactly one of -kbest or -sample should be given. Random seed is ignored if -kbest is specified.\n" ;
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

	let orig_grammar =
		match (get_comment_data grammar_file "awk ' /^\(\* original grammar: [a-zA-Z0-9\/\.]* \*\)/ {print $4} '") with
		| None -> grammar_file          (* No matching comments; try to use the grammar file itself *)
		| Some s -> s
	in

	(* Now we've got a guess at the original grammar file, let's try to parse it according to the pattern $GRAMMARS/wmcfg/$NAME.wmcfg *)
	let regex = Str.regexp "^\([a-zA-Z0-9\.-]+\)\/wmcfg\/\([a-zA-Z0-9\.-]+\)\.wmcfg$" in
	if (Str.string_match regex orig_grammar 0) then
		(Str.matched_group 1 orig_grammar, Str.matched_group 2 orig_grammar)
	else
		if (orig_grammar = grammar_file) then
			failwith (Printf.sprintf "No original grammar identified in %s (and this file itself is not in a location of the form $GRAMMARS/wmcfg/$NAME.wmcfg)" orig_grammar)
		else
			failwith (Printf.sprintf "Original grammar file identified as %s, but this is not of the form $GRAMMARS/wmcfg/$NAME.wmcfg" orig_grammar)

exception BadCommandLineArguments

let main () =
        try
                if not ((Array.length Sys.argv = 5) || (Array.length Sys.argv = 6)) then raise BadCommandLineArguments ;
                let mode = match Sys.argv.(1) with "-kbest" -> KBest | "-sample" -> Sample | _ -> raise BadCommandLineArguments in
                let grammar_file = Sys.argv.(2) in
                let num_trees = try int_of_string Sys.argv.(3)
                                with _ -> raise BadCommandLineArguments in
                let output_filename = Sys.argv.(4) in
                let random_seed = try if (Array.length Sys.argv = 6) then (Some (int_of_string Sys.argv.(5))) else None
                                  with _ -> raise BadCommandLineArguments in
                let (grammars_dir, grammar_name) = identify_original_grammar grammar_file in
                let prolog_file  = "mgcky-swi/setup.pl" in
                let grammar_files = { mg_file    = grammars_dir ^ "/mg/" ^ grammar_name ^ ".pl" ;
                                      wmcfg_file = grammar_file ;
                                      dict_file  = grammars_dir ^ "/mcfgs/" ^ grammar_name ^ ".dict"
                                    } in
                run_visualization grammar_files prolog_file num_trees output_filename mode random_seed
        with BadCommandLineArguments ->
                print_usage ()

let _ = if (!Sys.interactive) then () else main ()
