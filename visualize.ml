open Util

type grammar_files = {mg_file : string ; wmcfg_file : string ; dict_file : string}

type mode = KBest | Sample

(************************************************************************************************
Tool for visualising the "expected" derivations of a weighted grammar; in particular for 
visualising the "expected" continuations of a prefix, based on the weighted intersection grammar 
produced from that prefix.
************************************************************************************************)

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

(* Apparently there is no simpler way ... *)
let get_timestamp () =
	let tm = Unix.localtime(Unix.gettimeofday ()) in
	Printf.sprintf "%04d-%02d-%02d %02d:%02d:%02d" (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1) tm.Unix.tm_mday tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec;;

(* We report the number of parses that have probability greater than this threshold *)
let const_THRESHOLD = 0.001

(* derivations is a list of pairs; the first component is a derivation list, the second is a weight *)
let save_to_file mode_note grammar_files prolog_file (trees : (string Derivation.derivation_tree) list) filename =

        (* Open the file we'll be writing to *)
        let oc = open_out filename in
        Printf.fprintf oc "Here's a ``practice'' line showing how to write to the file\n" ;
        Printf.fprintf oc "\n" ;

        let entropy_note = match (get_comment_data grammar_files.wmcfg_file "sed 's/\\\"//g' | awk '/^\\(\\* entropy = [0-9\\.]* \\*\\)/ {print $4}'") with
                           | None -> "No entropy reported in grammar file"
                           | Some s -> try Printf.sprintf "Entropy = %.3f, with %d parses above %.3f"
                                                          (float_of_string s)
                                                          (List.length (take_while (fun t -> (float_of_weight (Derivation.get_weight t) > const_THRESHOLD)) trees))
                                                          const_THRESHOLD
                                       with _ -> "No entropy reported in grammar file"
        in
        Printf.fprintf oc "%s\n" entropy_note ;
        Printf.fprintf oc "\n" ;

        let prefix_note = match (get_comment_data grammar_files.wmcfg_file
                            "awk ' /^\\(\\* intersected with prefix: .* \\*\\)/ {$1=$2=$3=$4=\"\"; $NF=\"\"; print $0}'") with
                          | None -> "No intersection prefix reported in grammar file"
                          | Some s -> Printf.sprintf "Prefix: %s" s
        in
        Printf.fprintf oc "%s\n" prefix_note ;
        Printf.fprintf oc "\n" ;

        Printf.fprintf oc "\\begin{itemize}\n" ;
        Printf.fprintf oc "\\item %s\n" mode_note ;
        Printf.fprintf oc "\\item WMCFG grammar file: %s\n" grammar_files.wmcfg_file ;
        Printf.fprintf oc "\\item md5sum for this grammar file: %s\n" (Digest.to_hex (Digest.file grammar_files.wmcfg_file)) ;
        Printf.fprintf oc "\\item timestamp: %s\n" (get_timestamp ()) ;
        Printf.fprintf oc "\\end{itemize}\n" ;
        Printf.fprintf oc "\n" ;

        List.iter (fun t ->
            let sentence = Derivation.derived_string t in 
            Printf.fprintf oc "%.6g\t%s\n" (float_of_weight (Derivation.get_weight t)) sentence ;
            Printf.fprintf oc "%s\n" (Derivation.print_tree_sexp (fun x -> x) t) ;
            Printf.fprintf oc "\n" ;
        ) trees ;

        (* IMPORTANT: Close the file we opened at the beginning *)
        close_out oc

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
            save_to_file mode_note grammar_files prolog_file trees f
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

