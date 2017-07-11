
open Util

(* Removes spaces from the beginning and end of the string, 
 * and collapses adjacent spaces into a single space. *)
let cleanup_input s =
        let trimmed = Str.global_replace (Str.regexp "^ *\\| *$") "" s in
        let collapsed = Str.global_replace (Str.regexp " +") " " trimmed in
        collapsed

let set_prefix fsa str =
    if (!fsa != None) then raise (Arg.Bad (Printf.sprintf "Multiple intersections not allowed")) ;
    let input_list = Str.split (Str.regexp_string " ") (cleanup_input str) in
    fsa := Some (Fsa.make_fsa_prefix input_list)

let set_exact fsa str =
    if (!fsa != None) then raise (Arg.Bad (Printf.sprintf "Multiple intersections not allowed")) ;
    let input_list = Str.split (Str.regexp_string " ") (cleanup_input str) in
    fsa := Some (Fsa.make_fsa_exact input_list)

let set_infix fsa str =
    if (!fsa != None) then raise (Arg.Bad (Printf.sprintf "Multiple intersections not allowed")) ;
    let input_list = Str.split (Str.regexp_string " ") (cleanup_input str) in
    fsa := Some (Fsa.make_fsa_infix input_list)

let set_file fsa str =
    if (!fsa != None) then raise (Arg.Bad (Printf.sprintf "Multiple intersections not allowed")) ;
    fsa := Some (Fsa.make_fsa_from_file str)

let print_grammar grammar_file fsa (rules, start_symbol) =
    Printf.printf "(* original grammar: %s *)\n" grammar_file ;
    Printf.printf "(* intersected with %s *)\n" (Fsa.description fsa) ;
    List.iter (fun r -> Printf.printf "%s\n" (Rule.to_string r)) rules

let main () =
    let grammar_file = ref "" in
    let input_fsa = ref None in
    let speclist = Arg.align( [("-g", Arg.Set_string(grammar_file),             " (W)MCFG grammar file (obligatory)") ;
                               ("-prefix", Arg.String(set_prefix input_fsa),    " string to be interpreted as a prefix") ;
                               ("-infix", Arg.String(set_infix input_fsa),      " string to be interpreted as an infix") ;
                               ("-exact", Arg.String(set_exact input_fsa),      " string to be interpreted as exact") ;
                               ("-file", Arg.String(set_file input_fsa),        " FSA file") ;
                              ] ) in
    let usage_msg = (Printf.sprintf "Usage: %s -g <grammar file> (-prefix <string>) (-infix <string>) (-file <fsa file>)\n" Sys.argv.(0)) ^
                                    "       Exactly one of '-prefix', '-infix' of '-file' should be given"
    in
    let superfluous_arg s = raise (Arg.Bad (Printf.sprintf "Bad extra argument: %s" s)) in
    Arg.parse speclist superfluous_arg usage_msg ;
    if (!grammar_file = "") then (
        Printf.eprintf "Must provide a grammar file\n" ;
        Arg.usage speclist usage_msg
    ) else (
        match (!input_fsa) with
        | None ->
            begin
            Printf.eprintf "Must provide something to intersect with\n" ;
            Arg.usage speclist usage_msg
            end
        | Some fsa ->
            begin
            (* Everything's OK, let's do our thing ... *)
            let (rules,start_symbol) = Grammar.get_input_grammar (!grammar_file) in
            let chart = Parser.deduce rules fsa in
            let new_grammar = Grammar.intersection_grammar chart start_symbol fsa in
            print_grammar (!grammar_file) fsa new_grammar
            end
    )

let _ = if (!Sys.interactive) then () else main ()

