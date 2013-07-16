
open Util

(* Removes spaces from the beginning and end of the string, 
 * and collapses adjacent spaces into a single space. *)
let cleanup_input s =
        let trimmed = Str.global_replace (Str.regexp "^ *\\| *$") "" s in
        let collapsed = Str.global_replace (Str.regexp " +") " " trimmed in
        collapsed

let main () =
    let grammar_file = ref "" in
    let input = ref "" in
    let speclist = Arg.align( [("-g", Arg.Set_string(grammar_file), " (W)MCFG grammar file (obligatory)") ;
                               ("-i", Arg.Set_string(input),        " string to be parsed (obligatory)")] ) in
    let usage_msg = Printf.sprintf "Usage: %s -g <grammar file> -i <input string>" Sys.argv.(0) in
    let superfluous_arg s = raise (Arg.Bad (Printf.sprintf "Bad extra argument: %s" s)) in
    Arg.parse speclist superfluous_arg usage_msg ;
    if (!grammar_file = "") then (
        Printf.eprintf "Must provide a grammar file\n" ;
        Arg.usage speclist usage_msg
    ) else if (!input = "") then (
        Printf.eprintf "Must provide an input string to parse\n" ;
        Arg.usage speclist usage_msg
    ) else (
        (* Everything's OK, let's do our thing ... *)
        let (rules,start_symbol) = Grammar.get_input_grammar (!grammar_file) in
        let input_list = Str.split (Str.regexp_string " ") (cleanup_input (!input)) in
        let input_fsa = Fsa.make_fsa_exact input_list in
        let chart = Parser.deduce rules input_fsa in
        let goal_proposition = Chart.goal_item start_symbol input_fsa in
        let goal_derivations = Derivation.get_derivations chart goal_proposition in
        if (goal_derivations = []) then
            Printf.eprintf "No derivations found\n"
        else
            let show_derivation d = Printf.sprintf "%s\t%s" (show_weight_float (Derivation.get_weight d)) (Derivation.print_tree_sexp Chart.get_nonterm d) in
            List.iter print_endline (map_tr show_derivation goal_derivations)
    )

let _ = if (!Sys.interactive) then () else main ()

