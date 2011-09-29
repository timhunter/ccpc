open Util
open Generate

(************************************************************************************************
Tool for visualising the "expected" derivations of a weighted grammar; in particular for 
visualising the "expected" continuations of a prefix, based on the weighted intersection grammar 
produced from that prefix.
************************************************************************************************)

let run_visualization grammar_file =
	let (random_korean_tree, weight) = generate grammar_file in
	Printf.printf "%f\n" weight

let print_usage () =
	Printf.eprintf "Usage: %s <grammar-file>\n" Sys.argv.(0)

let main () =
	if (Array.length Sys.argv = 2) then
		run_visualization Sys.argv.(1)
	else
		print_usage ()

let _ = if (!Sys.interactive) then () else main ()
