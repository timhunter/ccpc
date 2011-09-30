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
	let result = Hashtbl.create 100 in
	let regex = Str.regexp "^\([a-z]+[0-9]+\) : (\(::? .*\))$" in
	begin
		try
			while true; do
				let line = input_line channel in
				if (Str.string_match regex line 0) then
					let category = Str.matched_group 1 line in
					let features = Str.matched_group 2 line in
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
	let command = Printf.sprintf "prolog -s %s -q -t \"['%s'], showLexicon\" 2>/dev/null" prolog_file grammar_files.mg_file in
	let channel =
		try Unix.open_process_in command
		with _ -> failwith (Printf.sprintf "Error attempting to run shell command: %s" command)
	in
	let result = Hashtbl.create 100 in
	let regex = Str.regexp "^\([0-9]+\)\. \[\(.*\)\]::\[\(.*\)\]$" in
	let remove_commas s = Str.global_replace (Str.regexp ",") "" s in
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
			close_in channel
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
let rec get_yield tree =
	let (Node (root, children)) = tree in
	match children with
	| [] -> failwith (Printf.sprintf "Tree not of the right form: no preterminal above %s node" root)
	| [Node (child,[])] -> (match (clean_preterminal root child) with None -> [] | Some x -> [x])
	| _ -> List.concat (List.map get_yield children)

(* Returns a list of lexical-item-IDs, given a derivation tree *)
let get_derivation_string tree grammar_files prolog_file =
	let yield = get_yield tree in
	List.iter (fun (preterm,term) -> Printf.printf "[%s]:[%s] " preterm term) yield ; Printf.printf "\n" ;
	let dict = get_guillaumin_dict grammar_files.dict_file in
	List.iter (fun (preterm,term) -> Printf.printf "[%s]:[%s] " (Hashtbl.find dict preterm) term) yield ; Printf.printf "\n" ;
	let index = get_stabler_index grammar_files prolog_file in
	let lookup_index preterm term : int =
		if (Str.string_match (Str.regexp "^:: \(.*\)$") preterm 0) then
			let features = Str.matched_group 1 preterm in
			try
				Hashtbl.find (index : (string * string, int) Hashtbl.t) ((term,features) : string * string)
			with Not_found -> failwith (Printf.sprintf "Couldn't find pair (%s,%s)" term features)
		else
			failwith (Printf.sprintf "Preterminal category doesn't look like it's of the right form: %s\n" preterm)
	in
	List.iter (fun (preterm,term) -> Printf.printf "%d " ((lookup_index (Hashtbl.find dict preterm) term) : int)) yield ; Printf.printf "\n" ;
	()

let run_visualization grammar_files prolog_file =
	let (random_tree, weight) = generate grammar_files.wmcfg_file in
	Printf.printf "weight is %f\n" weight ;
	write_tree random_tree "random_tree" ;
	get_derivation_string random_tree grammar_files prolog_file

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
