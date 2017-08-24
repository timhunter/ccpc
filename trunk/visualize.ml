open Util

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

let check_exit_code code command description =
	match code with
	| Unix.WEXITED 0 -> ()
	| Unix.WEXITED n -> Printf.eprintf "WARNING: %s exited with code %d\n" description n ;
	                    Printf.eprintf "         The command was: %s\n" command ;
	| Unix.WSIGNALED n -> Printf.eprintf "WARNING: %s was killed by signal number %d\n" description n
	| Unix.WSTOPPED n -> Printf.eprintf "WARNING: %s was stopped by signal number %d\n" description n

(************************************************************************************************)

(* Assumes that the result is meant to be unique, and returns None if there is not exactly one line produced. *)
let get_comment_data grammar_file filter_command =
        let command = Printf.sprintf "cat %s | %s" grammar_file filter_command in
        let channel =
                if not (Sys.file_exists grammar_file) then
                        failwith (Printf.sprintf "Specified grammar file does not exist: %s" grammar_file)
                else
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
        check_exit_code (Unix.close_process_in channel) command "Shell command reading grammar file" ;
        match results with
        | (x::[]) -> Some x
        | _ -> None

(************************************************************************************************)

(* Calls Stabler's prolog to get the IDs of each MG lexical item.
   Returns a mapping from (lexical-string, feature-sequence) to ID.
   Note that since we are dealing only with lexical items, feature-sequence always begins with ":: " (never ": "). *)
let get_stabler_index grammar_files prolog_file =
	let command = Printf.sprintf "swipl -s %s -q -t \"['%s'], showLexicon\" 2>/dev/null" prolog_file grammar_files.mg_file in
	let channel =
		if not (Sys.file_exists prolog_file) then
			failwith (Printf.sprintf "Required prolog file does not exist: %s" prolog_file)
		else if not (Sys.file_exists grammar_files.mg_file) then
			failwith (Printf.sprintf "Required MG file does not exist: %s" grammar_files.mg_file)
		else
			try Unix.open_process_in command
			with _ -> failwith (Printf.sprintf "Error attempting to run shell command: %s" command)
	in
	let result = Hashtbl.create 100 in
	let regex = Str.regexp "^\\([0-9]+\\)\\. \\[\\(.*\\)\\]::\\[\\(.*\\)\\]$" in
	let remove_commas s = Str.global_replace (Str.regexp ", *") " " s in
	begin
		try
			while true; do
				let line = input_line channel in
				if (Str.string_match regex line 0) then
					let id = int_of_string (Str.matched_group 1 line) in
					let str = Str.matched_group 2 line in
					let features = "::" ^^ remove_commas (Str.matched_group 3 line) in
					Hashtbl.add result (str,features) id
				else if (line <> "") then
					Printf.eprintf "WARNING: Ignoring unexpected line in prolog output: %s\n" line
			done
		with End_of_file ->
			check_exit_code (Unix.close_process_in channel) command "Prolog shell command for getting dictionary"
	end ;
	result

(************************************************************************************************)

(* De-situates any situated nonterminals in an intersection grammar, then 
   cleans up some of the irrelevant mess introduced by the Guillaumin MCFG-generation:
     (1) removes (preterminal,leaf) pairs of the form (E,"")
     (2) converts preterminals like "t123_tmp1" to "t123"             *)
let clean_preterminal preterminal leaf =
	let new_leaf = (if leaf = " " then "" else leaf) in
	let new_preterminal = Rule.desituate preterminal in
	if ((new_preterminal = "E") && (new_leaf = "")) then
		None
	else if (Str.string_match (Str.regexp "\\([a-z]+[0-9]+\\)_tmp[0-9]+") new_preterminal 0) then
		Some (Str.matched_group 1 new_preterminal, new_leaf)
	else
		Some (new_preterminal, new_leaf)

(* Returns the yield of a tree, as a list of (preterminal,leaf) pairs, possibly interspersed with "rule markers" *)
(* eg. [DerivLeaf ("t123","John"); RuleMarker LeftAdjoin; DerivLeaf ("t234","saw"), DerivLeaf ("t345","Mary")] *)
let rec get_yield dict tree =
    let rule = Derivation.get_rule tree in
    let root = Derivation.get_root_item tree in
    match (Derivation.get_children tree, Rule.get_expansion rule) with
    | ([], Rule.PublicTerminating term) -> (match (clean_preterminal root term) with None -> [] | Some x -> [DerivLeaf x])
    | ([], Rule.PublicNonTerminating _) -> failwith ("Malformed derivation tree: node with no children has a non-terminating rule")
    | (children, _) ->
        match (Rule.get_marked_mg_rule dict rule) with
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
		(* First make sure that the feature-sequence we're asking about is lexical, i.e. starts with "::". *)
		assert (Str.string_match (Str.regexp "^:: ") features 0) ;
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
let const_THRESHOLD = 0.001

(* derivations is a list of pairs; the first component is a derivation list, the second is a weight *)
let save_to_file mode_note grammar_files prolog_file (derivations : (int dlist * Util.weight) list) filename =

	(* The function derivation_as_string turns a pair like (0.234, [12,23,34]) in a string (readable as a prolog list) like "[0.234,12,23,34]" 
	   (and deals properly with rule markers in amongst the ids). *)
	(* The prolog code knows to treat the heads of these lists as a "note" to be printed out as is, and treat the tails as derivations *)
	let derivation_as_string (dlist,w) = "[" ^ (String.concat "," ((Printf.sprintf "%.12f" (float_of_weight w))::(dlist_to_strings string_of_int dlist))) ^ "]" in
	let derivations_as_string = "[" ^ (String.concat "," (List.map derivation_as_string derivations)) ^ "]" in
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
		let entropy_note = match (get_comment_data grammar_files.wmcfg_file "sed 's/\\\"//g' | awk '/^\\(\\* entropy = [0-9\\.]* \\*\\)/ {print $4}'") with
		                   | None -> "Could not find entropy"
		                   | Some s -> try Printf.sprintf "Entropy = %.3f, with %d parses above %.3f"
		                                                  (float_of_string s)
		                                                  (List.length (take_while (fun (_,w) -> (float_of_weight w > const_THRESHOLD)) derivations))
		                                                  const_THRESHOLD
		                               with _ -> "Could not find entropy"
		in
		let prefix_note = match (get_comment_data grammar_files.wmcfg_file
		                    "awk ' /^\\(\\* intersected with prefix: .* \\*\\)/ {$1=$2=$3=$4=\"\"; $NF=\"\"; print $0}'") with
		                  | None -> "Could not find intersection prefix"
		                  | Some s -> Printf.sprintf "Prefix: %s" s
		in
		let fmt = format_of_string "swipl -s %s -q -t \"['%s'], parse_and_display(%s,'%s','%s',%s,'%s').\" 2>/dev/null" in
		let command = Printf.sprintf fmt prolog_file grammar_files.mg_file intro_lines_as_string prefix_note entropy_note derivations_as_string filename in
	let channel =
			try Unix.open_process_in command
			with _ -> failwith (Printf.sprintf "Error attempting to run shell command: %s" command)
	in
	try
		while true; do
			Printf.eprintf "*** Output from Prolog tree-drawing: %s\n" (input_line channel)
		done
	with End_of_file ->
		check_exit_code (Unix.close_process_in channel) command "Prolog shell command for saving trees to file" ;

        (* Unfortunate necessary hack: escape any underscores in the latex file produced by prolog *)
        let command' = Printf.sprintf "sed -i'' -e 's/_/\\\\_/g' %s" filename in
        let channel' =
                try Unix.open_process_in command'
                with _ -> failwith (Printf.sprintf "Error attempting to run shell command: %s" command')
        in
        check_exit_code (Unix.close_process_in channel') command' "sed shell command for escaping underscores in latex"

(* Returns a pair of strings (grammars_dir, grammar_name), such that the relevant files are 
   grammars_dir/{mg,mcfgs}/grammar_name.{pl,mcfg,dict} *)
let identify_original_grammar grammar_file =
	let orig_grammar =
		match (get_comment_data grammar_file "awk ' /^\\(\\* original grammar: [a-zA-Z0-9\\/\\._-]* \\*\\)/ {print $4} '") with
		| None -> grammar_file          (* No matching comments; try to use the grammar file itself *)
		| Some s -> s
	in
	(* Now we've got a guess at the original grammar file, let's try to parse it according to the pattern $GRAMMARS/wmcfg*/$NAME.wmcfg *)
	let regex = Str.regexp "^\\([a-zA-Z0-9\\.\\/_-]+\\)\\/wmcfg[^\\/]*\\/\\([a-zA-Z0-9\\._-]+\\)\\.wmcfg$" in
	if (Str.string_match regex orig_grammar 0) then
		(Str.matched_group 1 orig_grammar, Str.matched_group 2 orig_grammar)
	else
		if (orig_grammar = grammar_file) then
			failwith (Printf.sprintf "No original grammar identified in %s (and this file itself is not in a location of the form $GRAMMARS/wmcfg*/$NAME.wmcfg)" orig_grammar)
		else
			failwith (Printf.sprintf "Original grammar file identified as %s, but this is not of the form $GRAMMARS/wmcfg*/$NAME.wmcfg" orig_grammar)

let run_visualization wmcfg_file num_trees output_filename mode optional_seed =

        let (trees,mode_note) =
                match mode with
                | KBest ->
                        if (optional_seed <> None) then Printf.eprintf "*** WARNING: using kbest mode, so ignoring random seed\n" ;
                        let (rules, start_symbol) = Grammar.get_input_grammar wmcfg_file in
                        let derivation_trees = Derivation.get_n_best_from_grammar num_trees rules start_symbol in  (* of the type declared in derivation.ml *)
                        (derivation_trees, "exact k-best enumeration of most likely derivations")
                | Sample ->
                        begin
                        let random_seed =
                                match optional_seed with
                                | None -> Random.self_init () ; Random.int 1000
                                | Some n -> n
                        in
                        Printf.eprintf "Using random seed %d\n" random_seed ;
                        Random.init random_seed ;
                        let derivation_trees = Derivation.generate num_trees wmcfg_file in
                        (derivation_trees, Printf.sprintf "randomly sampled derivations with random seed %d" random_seed)
                        end
        in

    let process_tree tree =
        let sentence = Derivation.derived_string tree in 
        Printf.printf "%.6g\t%s\n" (float_of_weight (Derivation.get_weight tree)) sentence ;
    in
    List.iter process_tree trees ;

    match output_filename with
    | None -> ()
    | Some f ->
        try
            let prolog_file  = (Filename.dirname Sys.executable_name) ^ "/mgcky-swi/setup.pl" in
            let (grammars_dir, grammar_name) = identify_original_grammar wmcfg_file in
            let grammar_files = { mg_file    = grammars_dir ^ "/mg/" ^ grammar_name ^ ".pl" ;
                                  wmcfg_file = wmcfg_file ;
                                  dict_file  = grammars_dir ^ "/mcfgs/" ^ grammar_name ^ ".dict"
                                } in
            let dict =
                try Grammar.get_guillaumin_dict grammar_files.dict_file
                with Failure str -> failwith (Printf.sprintf "%s (Perhaps there is no MG file from which %s was derived?)" str wmcfg_file) ;
            in
            let index = get_stabler_index grammar_files prolog_file in
            let derivations = map_tr (fun d -> get_derivation_string d dict index, Derivation.get_weight d) trees in
            save_to_file mode_note grammar_files prolog_file derivations f
        with Failure str ->
            Printf.eprintf "%s\n" str ;
            Printf.eprintf "Couldn't write derivations to latex file\n"

(************************************************************************************************)

let main () =
    let mode = ref KBest in
    let grammar_file = ref "" in
    let num_trees = ref 10 in
    let output_file = ref None in
    let random_seed = ref None in
    let speclist = Arg.align([ ("-kbest",  Arg.Unit(fun () -> mode := KBest),           " use exact k-best enumeration of derivations (default)") ;
                               ("-sample", Arg.Unit(fun () -> mode := Sample),          " use random sampling instead of exact k-best enumeration") ;
                               ("-g",      Arg.Set_string(grammar_file),                " (W)MCFG grammar file (obligatory)") ;
                               ("-n",      Arg.Set_int(num_trees),                      " number of derivations to report (optional, default is 10)") ;
                               ("-o",      Arg.String(fun s -> output_file := Some s),  " location for output latex file (optional; only compatible with MG-derived grammars)") ;
                               ("-seed",   Arg.Int(fun n -> random_seed := Some n),     " random seed (optional; ignored if -sample is not used)") ;
                             ]) in
    let usage_msg = String.concat "\n" [Printf.sprintf "Simplest usage example: %s -g <grammar file>" Sys.argv.(0) ;
                                        "" ;
                                        "========================================================================================================" ;
                                        "In order for latex output to be possible, the grammar file should" ;
                                        "   EITHER (i)  be given as a path of the form $GRAMMARS/wmcfg*/$NAME.wmcfg" ;
                                        "       OR (ii) contain a comment line identifying a weighted MCFG file from which it is derived" ;
                                        "               by intersection, as path of the form $GRAMMARS/wmcfg*/$NAME.wmcfg" ;
                                        "" ;
                                        "In either case, the associated MG and dictionary files should be in the following locations:" ;
                                        "   $GRAMMARS/mg/$NAME.pl" ;
                                        "   $GRAMMARS/mcfgs/$NAME.dict" ;
                                        "========================================================================================================" ;
                                        "" ;
                                       ] in
    let superfluous_arg s = raise (Arg.Bad (Printf.sprintf "Bad extra argument: %s" s)) in
    Arg.parse speclist superfluous_arg usage_msg ;
    if (!grammar_file = "") then (
        Printf.eprintf "Must provide a grammar file\n" ;
        Arg.usage speclist usage_msg
    ) else (
        run_visualization (!grammar_file) (!num_trees) (!output_file) (!mode) (!random_seed)
    )
let _ = if (!Sys.interactive) then () else main ()

