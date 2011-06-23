
(**************************************************************************)
(* NB: These commands refer to files, not modules.
       They still need to be written with an initial capital. *)
open Util
open Rule
(*open Deriver*)
open Parser
open Read 
open Chart
open Rational

(******************************************************************************************)


let get_input_grammar grammar_file =
  try 
    let channel = open_in grammar_file in 
    let lexbuf = Lexing.from_channel channel in 
    Read.mcfgrule Lexer.token lexbuf  
  with _ -> print_string ("Can't parse input mcfg file "^grammar_file^"\n"); []

(******************************************************************************************)

module type DEDUCTIVE_SYSTEM =
  sig
    type prim  (* eg. a list of these makes up a grammar *)
    type item  (* the things we deduce *)
    type input (* once-off input, eg. a sentence or a prefix *)
    val get_axioms : Rule.r list -> input -> item list
    val is_goal : input -> item -> bool
    val max_arity : Rule.r list -> int
    val rule_arity : Rule.r -> int
    val build_nary : Rule.r list -> item list -> item list
  end ;;

module Deducer = functor (D : DEDUCTIVE_SYSTEM) ->
  struct

    (*** TODO: Clever things for getting stuff from the chart quickly ***)
    (***       Probably requires sacrificing some generality; TBD.    ***)
    let rec consequences max_depth prims recent_items old_items =
            let all_existing_items = recent_items @ old_items in
      if max_depth = 0 then
        all_existing_items
      else
             match recent_items with
            [] -> old_items
          | (i::is) ->
            let rules_of_arity n = List.filter (fun rule -> D.rule_arity rule = n) prims in
            let new_items n = concatmap_tr (D.build_nary (rules_of_arity n)) (all_lists all_existing_items n) in
              let all_new_items = concatmap_tr new_items (range 1 ((D.max_arity prims)+1)) in
            let useful_new_items = List.filter (fun x -> not (List.mem x all_existing_items)) all_new_items in
                  consequences (max_depth-1) prims (is @ useful_new_items) (i :: old_items)

    let deduce max_depth prims input =
      let axioms = D.get_axioms prims input in
      consequences max_depth prims axioms []

  end;;

(*module MCFG_Derivation_Deducer = Deducer(MCFG_Deriver)*)

(******************************************************************************************)
(* Extract the intersection grammar *)
(* See Albro's dissertation, appendix C section C.4 *)

(* Like map2 but each of the xs gets used with each of the ys *)
let all_combinations f xs ys =
  let exhaust x ys = map_tr (f x) ys in
  concatmap_tr (fun x -> exhaust x ys) xs

let rec cartesian lists =
  match lists with
    [] -> [[]]
  | (l :: ls) -> all_combinations (fun x -> fun xs -> x :: xs) l (cartesian ls)

let rec build_symbol sym ranges =
  match ranges with
    [] -> sym
  | ((RangeVal p, RangeVal q)::rs) -> build_symbol (sym ^ (Printf.sprintf "_%d%d" p q)) rs
  | ((EpsVar, EpsVar)::rs) -> build_symbol (sym ^ (Printf.sprintf "Epsilon")) rs
  | _ -> failwith "Should not be mixing RangeVal with EpsVar!"

let result_matches f input_ranges expected_result =
  try
    (expected_result = Rule.apply f input_ranges concat_ranges)
  with
    RangesNotAdjacentException -> false

let make_new_rule sit_nonterm rights func range_lists weight =
  let new_rights = List.map2 build_symbol rights range_lists in
  let new_agenda_items = List.map2 (fun x y -> Chart.create_item x y None weight) rights range_lists in
  (Rule.create_rule (sit_nonterm, new_rights, func, weight), new_agenda_items)

(* NB: There is a "bug" in Albro's dissertation where he describes this algorithm.
       On page 293, where he says "check whether f is well-defined", it should read something like 
       "check whether f is well-defined and evaluates to the ranges in the trigger item".
 *)
let intersection_rules_per_rule all_items item rule =
  let sit_nonterm = build_symbol (Chart.get_nonterm item) (Chart.get_ranges item) in
  match Rule.get_expansion rule with
  | PublicTerminating str -> ([Rule.create_terminating (sit_nonterm, str, (Rule.get_weight rule))], [])
  | PublicNonTerminating (nts', func) -> (* ((nt,nts), func) -> *)
    let items_headed_by nt = List.filter (fun item -> (Chart.get_nonterm item) = nt) all_items in
    let items_grouped = Nelist.to_list (Nelist.map items_headed_by nts') in
    let item_combinations = cartesian items_grouped in
    let ranges_from_item_comb items = map_tr Chart.get_ranges items in
    let function_inputs = map_tr ranges_from_item_comb item_combinations in
    let defined_function_inputs = List.filter (fun input -> result_matches func input (Chart.get_ranges item)) function_inputs in
    (* results_to_combine :: (Rule.r * Parser.item list) list *)
    let results_to_combine = map_tr (fun x -> (make_new_rule sit_nonterm (Nelist.to_list nts') func x (Rule.get_weight rule))) defined_function_inputs in
    let new_rules = map_tr fst results_to_combine in
    let new_agenda_items = concatmap_tr snd results_to_combine in
    (new_rules, new_agenda_items)

let new_intersection_grammar_rules orig_grammar chart item =
  let relevant_rules = List.filter (fun rule -> (Rule.get_nonterm rule = Chart.get_nonterm item)) orig_grammar in
  let results_to_combine = map_tr (intersection_rules_per_rule chart item) relevant_rules in               (* (rule list * item list) list *)
  let new_rules = concatmap_tr fst results_to_combine in
  let new_agenda_items = concatmap_tr snd results_to_combine in
  (new_rules, new_agenda_items)

let rec build_intersection_grammar orig_grammar chart (agenda,i) grammar_so_far =
  if (i >= List.length agenda) then
    grammar_so_far
  else
    let trigger = List.nth agenda i in
    let (new_rules, new_agenda_items) = new_intersection_grammar_rules orig_grammar chart trigger in
    build_intersection_grammar orig_grammar chart (uniques (agenda @ new_agenda_items), i+1) (grammar_so_far @ new_rules)

let intersection_grammar orig_grammar symbols =
  let chart = uniques (Parser.deduce (-1) orig_grammar (Parser.Prefix symbols)) in
  let goal_items = List.filter (Parser.is_goal (Parser.Sentence symbols)) chart in
  uniques (build_intersection_grammar orig_grammar chart (goal_items,0) [])

(******************************************************************************************)
(* Top-level stuff for testing *)

(*let derivations rules =
  MCFG_Derivation_Deducer.deduce 1 rules MCFG_Deriver.null_input*)



let parse rules symbols = 
(*  MCFG_ParseGen_Deducer.deduce (-1) rules (Parser.Sentence symbols)*)
  Parser.deduce (-1) rules (Parser.Sentence symbols)

let timed_parse rules symbols =
  let t = Sys.time () in
  let chart = parse rules symbols in 
  let duration = (Sys.time ()) -. t in
  let goal_items = List.filter (Parser.is_goal (Parser.Sentence symbols)) chart in 
  let d = if (List.length goal_items) > 0 then duration else 0. -. duration in
    (d, chart)


let parse_with_intersection prefix sentence =
  let new_grammar = intersection_grammar (get_input_grammar Sys.argv.(1)) prefix in
  parse new_grammar sentence
  

(*
 * Bit of a hack here: the start symbol that makes a goal item a goal item depends on the length of the 
 * prefix, but the span that makes a goal item a goal item depends on the length of the sentence. So I 
 * have to put together a start symbol by hand out here and look at the insides of the items directly. 
 * Not sure what the best approach to fixing this is.
 *)

let print_tree item sentence =
  let rec print item level str =
    let backpointer = Chart.get_backpointer item in 
    let str = str ^ Printf.sprintf "%s/[" (Chart.to_string item sentence) in
    (match backpointer with 
      None -> str
      | Some (Some a, None) -> ((print !a (level+1) str) ^ (Printf.sprintf "]"))
      | Some (Some a, Some b) -> (print !b (level+1) ((print !a (level+1) str) ^ "],")) ^ (Printf.sprintf "]" )
      | _ -> failwith "Invalid Parent backpointer") in
  (print item 0 "") ^ "]"

let run_prefix_parser prefix sentence debug =
  let intersection_start_symbol = Printf.sprintf "S_0%d" (List.length prefix) in
  let is_goal item = ((Chart.get_nonterm item) = intersection_start_symbol) && (Chart.get_ranges item = [(RangeVal 0, RangeVal (List.length sentence))]) in
  let chart = parse_with_intersection prefix sentence in 

  let goal_items = List.filter (is_goal) chart in 
  print_int (List.length goal_items);
  let rec make_trees goals acc =
    match goals with
      [] -> acc
    | h::t ->  make_trees t ((print_tree h sentence)::acc) in
  let result = make_trees goal_items [] in
  (if debug then 
    List.iter (fun x -> Printf.printf "\n%s" (Chart.to_string x sentence)) chart);
  (if (List.length goal_items)>0 then 
    (Printf.printf "\nSUCCESS!\n";)
  else 
    Printf.printf "\nFAILED\n");
  result

let run_parser sentence debug gram_file =
  let chart = parse (get_input_grammar gram_file) sentence in 
  let goal_items = List.filter (Parser.is_goal (Parser.Sentence sentence)) chart in 
  let rec make_trees goals acc =
    match goals with
      [] -> acc
    | h::t ->  make_trees t ((print_tree h sentence)::acc) in
  let result = make_trees goal_items [] in
  (if debug then 
    List.iter (fun x -> Printf.printf "\n%s" (Chart.to_string x sentence)) chart);
  (if (List.length goal_items)>0 then 
    (Printf.printf "\nSUCCESS!\n";)
  else 
    Printf.printf "\nFAILED\n");
  result
  
    
let main () =
  begin
  let oc = open_out "maketree.pl" in
  try
     match Sys.argv.(2) with
        "-p" -> let prefix = Util.split ' ' Sys.argv.(3) in 
                let sentence = Util.split ' ' Sys.argv.(4) in
                run_prefix_parser prefix sentence false;
                ()
      | "-d" -> (match Sys.argv.(3) with 
                  "-p" -> let prefix = Util.split ' ' Sys.argv.(4) in 
                          let sentence = Util.split ' ' Sys.argv.(5) in
                          run_prefix_parser prefix sentence true;
                          ()
                  | _ ->  let sentence = Util.split ' ' Sys.argv.(3) in
                          run_parser sentence true Sys.argv.(1);
                          ())
      | "-o" -> (let lst = (match Sys.argv.(4) with
                              "-d" -> (match Sys.argv.(5) with 
                                         "-p" -> let prefix = Util.split ' ' Sys.argv.(6) in 
                                                 let sentence = Util.split ' ' Sys.argv.(7) in 
                                                 run_prefix_parser prefix sentence true
                                       | _ ->    let sentence = Util.split ' ' Sys.argv.(5) in 
                                                 run_parser sentence true Sys.argv.(1)) 
                            | "-p" -> let prefix = Util.split ' ' Sys.argv.(5) in 
                                      let sentence = Util.split ' ' Sys.argv.(6) in 
                                      run_prefix_parser prefix sentence false
                            | _ ->    let sentence = Util.split ' ' Sys.argv.(4) in
                                      run_parser sentence false Sys.argv.(1)) in 
                   Printf.fprintf oc "tikz_qtree(%s, '%s')." (List.nth lst 0) (Sys.argv.(3));
                   close_out oc;
                   let exit_code = Sys.command "prolog -q -s tikz_qtreeSWI.pl < maketree.pl" in
                   (if exit_code = 1 then Printf.printf "Error running tree drawer");
                let exit_code = Sys.command "rm maketree.pl" in 
                if exit_code = 1 then Printf.printf "Error deleting prolog tree file";)
       | _ -> let sentence = Util.split ' ' Sys.argv.(2) in
              run_parser sentence false Sys.argv.(1);
              ()
  with _ -> Printf.printf "Usage: mcfg grammar-file (-o output-file) (-d) (-p \"prefix\") \"sentence\"";
            Printf.printf "\nFlags in parentheses are optional\n"
  end

let _ = if (!Sys.interactive) then () else main () ;;
