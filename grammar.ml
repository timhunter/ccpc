open Util
open Rule

let get_input_grammar grammar_file =
  try 
    let channel = open_in grammar_file in 
    let lexbuf = Lexing.from_channel channel in 
    Read.mcfgrule Lexer.token lexbuf  
  with _ -> print_string ("Can't parse input mcfg file "^grammar_file^"\n"); []

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
  | ((RangeVal p, RangeVal q)::rs) -> build_symbol (sym ^ (Printf.sprintf "_%d-%d" p q)) rs
  | ((EpsVar, EpsVar)::rs) -> build_symbol (sym ^ (Printf.sprintf "Epsilon")) rs
  | _ -> failwith "Should not be mixing RangeVal with EpsVar!"

let result_matches f input_ranges expected_result =
  try
    (expected_result = Rule.apply f input_ranges concat_ranges)
  with
    RangesNotAdjacentException -> false

let make_new_rule sit_nonterm rights func range_lists weight =
  let new_rights = List.map2 build_symbol rights range_lists in
  let new_agenda_items = List.map2 Chart.create_item rights range_lists in
  (Rule.create_rule (sit_nonterm, new_rights, func, weight), new_agenda_items)

(* NB: There is a "bug" in Albro's dissertation where he describes this algorithm.
       On page 293, where he says "check whether f is well-defined", it should read something like 
       "check whether f is well-defined and evaluates to the ranges in the trigger item".
 *)
let intersection_rules_per_rule prefix chart item rule =
  let sit_nonterm = build_symbol (Chart.get_nonterm item) (Chart.get_ranges item) in
  match Rule.get_expansion rule with
  | PublicTerminating str ->
    ( match (Chart.get_ranges item) with
      | [(RangeVal i, RangeVal j)] -> if (i < j) && (not (List.map (List.nth prefix) (range i j) = [str])) then
                                         ([],[])
                                      else
                                         ([Rule.create_terminating (sit_nonterm, str, (Rule.get_weight rule))], [])
      | _ -> ([Rule.create_terminating (sit_nonterm, str, (Rule.get_weight rule))], [])
    )
  | PublicNonTerminating (nts', func) -> (* ((nt,nts), func) -> *)
    let route_matches_rule rule route = ((map_tr Chart.get_nonterm (fst route)) = Nelist.to_list nts') in
    let matching_routes = List.filter (route_matches_rule rule) (Chart.get_routes item chart) in
    let new_rule_for_route (items, _) = make_new_rule sit_nonterm (Nelist.to_list nts') func (map_tr Chart.get_ranges items) (Rule.get_weight rule) in
    let results_to_combine = (map_tr new_rule_for_route matching_routes) in
    let new_rules = map_tr fst results_to_combine in
    let new_agenda_items = concatmap_tr snd results_to_combine in
    (new_rules, new_agenda_items)

let new_intersection_grammar_rules orig_grammar prefix chart item =
  let relevant_rules = List.filter (fun rule -> (Rule.get_nonterm rule = Chart.get_nonterm item)) orig_grammar in
  let results_to_combine = map_tr (intersection_rules_per_rule prefix chart item) relevant_rules in               (* (rule list * item list) list *)
  let new_rules = concatmap_tr fst results_to_combine in
  let new_agenda_items = concatmap_tr snd results_to_combine in
  (new_rules, new_agenda_items)

let rec build_intersection_grammar orig_grammar prefix chart (agenda,i) grammar_so_far =
  if (i >= List.length agenda) then
    grammar_so_far
  else
    let trigger = List.nth agenda i in
    let (new_rules, new_agenda_items) = new_intersection_grammar_rules orig_grammar prefix chart trigger in
    build_intersection_grammar orig_grammar prefix chart (uniques (agenda @ new_agenda_items), i+1) (grammar_so_far @ new_rules)

let intersection_grammar (rules, start_symbol) symbols =
  let chart = Parser.deduce (-1) rules (Parser.Prefix symbols) in
  let goal_items = Chart.goal_items chart start_symbol (List.length symbols) in
  let new_start_symbol = Printf.sprintf "%s_0%d" start_symbol (List.length symbols) in
  let new_rules = build_intersection_grammar rules symbols chart (goal_items,0) [] in
  (new_rules, new_start_symbol)


