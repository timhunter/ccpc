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

let rec build_symbol sym ranges =
  match ranges with
    [] -> sym
  | ((RangeVal p, RangeVal q)::rs) -> build_symbol ((Printf.sprintf "<%d," p) ^ sym ^ (Printf.sprintf ",%d>" q)) rs
  | ((EpsVar, EpsVar)::rs) -> build_symbol (sym ^ (Printf.sprintf "Epsilon")) rs
  | _ -> failwith "Should not be mixing RangeVal with EpsVar!"

let make_new_rule sit_nonterm rights func range_lists weight =
  let new_rights = List.map2 build_symbol rights range_lists in
  let new_agenda_items = List.map2 Chart.create_item rights range_lists in
  (Rule.create_rule (sit_nonterm, new_rights, func, weight), new_agenda_items)

let new_intersection_grammar_rules prefix chart item =
  let sit_nonterm = build_symbol (Chart.get_nonterm item) (Chart.get_ranges item) in
  let routes = Chart.get_routes item chart in
  let make_rule_for_route ((items, rule, weight) : Chart.route) : (Rule.r * Chart.item list) option =
    match (Rule.get_expansion rule) with
    | PublicTerminating str -> (
        assert (items = []) ;   (* If this route used a terminating rule, there can't be any antecedent items *)
        match (Chart.get_ranges item) with
        | [(RangeVal i, RangeVal j)] -> if (i < j) && not (List.map (List.nth prefix) (range i j) = [str]) then (* I think maybe this could become an assert *)
                                           None
                                        else
                                           Some (Rule.create_terminating (sit_nonterm, str, Rule.get_weight rule), [])
        | _ -> Some (Rule.create_terminating (sit_nonterm, str, (Rule.get_weight rule)), [])
      )
    | PublicNonTerminating (nts,func) -> (
        assert (Nelist.to_list nts = map_tr Chart.get_nonterm items) ;
        Some make_new_rule sit_nonterm (map_tr Chart.get_nonterm items) func (map_tr Chart.get_ranges items) (Rule.get_weight rule)
      )
  in
  let results_to_combine : (Rule.r * Chart.item list) list = optlistmap make_rule_for_route routes in
  let new_rules = map_tr fst results_to_combine in
  let new_agenda_items = concatmap_tr snd results_to_combine in
  (new_rules, new_agenda_items)

let rec build_intersection_grammar prefix chart (agenda,i) grammar_so_far =
  if (i >= List.length agenda) then
    grammar_so_far
  else
    let trigger = List.nth agenda i in
    let (new_rules, new_agenda_items) = new_intersection_grammar_rules prefix chart trigger in
    build_intersection_grammar prefix chart (uniques (agenda @ new_agenda_items), i+1) (grammar_so_far @ new_rules)

let intersection_grammar (rules, start_symbol) symbols =
  let chart = Parser.deduce (-1) rules (Parser.Prefix symbols) in
  let goal_items = Chart.goal_items chart start_symbol (List.length symbols) in
  let new_start_symbol = Printf.sprintf "%s_0%d" start_symbol (List.length symbols) in
  let new_rules = build_intersection_grammar symbols chart (goal_items,0) [] in
  (new_rules, new_start_symbol)


