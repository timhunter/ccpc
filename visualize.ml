open Util
open Generate

type grammar_files = {mg_file : string ; wmcfg_file : string ; dict_file : string}

(************************************************************************************************
Tool for visualising the "expected" derivations of a weighted grammar; in particular for 
visualising the "expected" continuations of a prefix, based on the weighted intersection grammar 
produced from that prefix.
************************************************************************************************)

(* Returns a list of (preterminal,leaf) pairs, each component being a string *)
let rec get_yield tree =
	let (Node (root, children)) = tree in
	match children with
	| [] -> failwith (Printf.sprintf "Tree not of the right form: no preterminal above %s node" root)
	| [Node (child,[])] -> [(root,child)]
	| _ -> List.concat (List.map get_yield children)

let get_derivation_string tree =
	let yield = get_yield tree in
	Printf.printf "length of yield is %d\n" (List.length yield) ;
	List.iter (fun (preterm,term) -> Printf.printf "%s:%s " preterm term) yield ; Printf.printf "\n" ;
	()

let run_visualization grammar_files prolog_file =
	let (random_tree, weight) = generate grammar_files.wmcfg_file in
	Printf.printf "weight is %f\n" weight ;
	write_tree random_tree "random_tree" ;
	get_derivation_string random_tree

let print_usage () =
	Printf.eprintf "\n" ;
	Printf.eprintf "Usage: %s <grammars-directory> <grammar-name> <stabler-prolog-directory>\n" Sys.argv.(0) ;
	Printf.eprintf "\n" ;
	Printf.eprintf "    Assumes the three relevant grammar-related files are in these locations:\n" ;
	Printf.eprintf "        <grammars-directory>/mg/<grammar-name>.pl\n" ;
	Printf.eprintf "        <grammars-directory>/mcfg/<grammar-name>.dict\n" ;
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
		                      dict_file  = grammars_dir ^ "/mcfg/" ^ grammar_name ^ ".dict"
		                    } in
		run_visualization grammar_files prolog_file
	) else
		print_usage ()

let _ = if (!Sys.interactive) then () else main ()
