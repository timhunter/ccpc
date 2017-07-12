
open Util

(* All the work will go here. *)
(* The arguments are an existing grammar (list of rules) to be normalized and that grammar's start symbol. 
 * The return value is the entropy of the start symbol in that grammar.
 *)
let find_entropy rules start_symbol =
    let m = Matrix.create_square_matrix 3 (fun r -> fun c -> 0.0) in
    let m' = Matrix.invert m in
    0.0

let main () =
    let grammar_file = ref "" in
    let speclist = Arg.align( [("-g", Arg.Set_string(grammar_file), " WMCFG grammar file (obligatory)") ] ) in
    let usage_msg = Printf.sprintf "Usage: %s -g <grammar file>" Sys.argv.(0) in
    let superfluous_arg s = raise (Arg.Bad (Printf.sprintf "Bad extra argument: %s" s)) in
    Arg.parse speclist superfluous_arg usage_msg ;
    if (!grammar_file = "") then (
        Printf.eprintf "Must provide a grammar file\n" ;
        Arg.usage speclist usage_msg
    ) else (
        (* Everything's OK, let's do our thing ... *)
        let (rules,start_symbol) = Grammar.get_input_grammar (!grammar_file) in
        let entropy = find_entropy rules start_symbol in
        Printf.printf "(* \"entropy = %f\" *)\n" entropy
    )

let _ = if (!Sys.interactive) then () else main ()

