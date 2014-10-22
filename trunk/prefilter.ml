(* prefilter
  so you don't need so many grammar rules for just this one sentence
*)

module OrderedString =
  struct
    type t = string
    let compare = compare
  end
module StringSet = Set.Make(OrderedString)


(* Removes spaces from the beginning and end of the string, 
 * and collapses adjacent spaces into a single space. *)
let cleanup_input s =
        let trimmed = Str.global_replace (Str.regexp "^ *\\| *$") "" s in
        let collapsed = Str.global_replace (Str.regexp " +") " " trimmed in
        collapsed


(* this is occasionally returning 0 beware *)
let oneminus w = match w with
    weight_one -> weight_one
  | _ ->   let negative_one = Util.weight_from_float (-1.0) in
	   let negative_w = Util.mult_weights negative_one w in
	   Util.add_weights Util.weight_one negative_w

(*
for each rule 
  - if it's phrasal then let it pass
  - if its preterminal and we have already handled its LHS then ignore
  - if its preterminal and we have not already handled its LHS
          then output it. 
          also output (1-weight) for LHS -> unknown  add_weighted weightone
          mark this LHS as done
  if it's in inputwords then we need this exact rule
*)

(* this code could be improved by using a record rather than a triple *)
let filt (lhses,inputwords,accumulator) r = match (Rule.get_expansion r) with
    Rule.PublicNonTerminating(_,_) ->  (lhses,inputwords,r::accumulator)
  | Rule.PublicTerminating(actualword) ->
    let preterminalcategory = Rule.get_nonterm r in
    begin
      match ((StringSet.mem preterminalcategory lhses),(StringSet.mem actualword inputwords)) with
  (* seen before, on input *)
	  true,true ->  (lhses,inputwords,r::accumulator)
	| true,false -> (lhses,inputwords,accumulator)
	| false,false -> let r_unknown =
		   Rule.create_terminating (preterminalcategory,"unknown",(oneminus (Rule.get_weight r))) in
			 ((StringSet.add preterminalcategory lhses),inputwords,r_unknown::accumulator)
	| false,true -> let r_unknown =
		   Rule.create_terminating (preterminalcategory,"unknown",(oneminus (Rule.get_weight r))) in
			 ((StringSet.add preterminalcategory lhses),inputwords,r::r_unknown::accumulator)
    end
  
let project_and_rev (_,_,x) = List.rev x

let readin file =
  let ch = open_in file in
  let lb = Lexing.from_channel ch in
  let rules =
     try
       Read.mcfgrule Lexer.token lb
     with Failure str -> Printf.eprintf "%s\n" str ;
                      failwith ("Can't parse input mcfg file "^file)
  in
  begin
    close_in ch;
    rules
  end


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
      let input_set = StringSet.of_list (Str.split (Str.regexp_string " ") (cleanup_input (!input))) in
      let rule_list = readin !grammar_file in
      let filtered = List.fold_left filt (StringSet.empty,input_set,[]) rule_list in
      List.iter (function r -> print_endline (Rule.to_string r)) (project_and_rev filtered)
    )

let _ = if (!Sys.interactive) then () else main ()

