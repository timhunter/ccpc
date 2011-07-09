open Util
open Rule
open Parser
open Read 
open Chart
open Rational

(************************************************************************************************
Accepts as command-line args (i) a grammar file
                             (ii) a file containing a list of sentences.
Parses each of the sentences with the given grammar.
For each rule, keeps count of (i) number of time the rule is used
                              (ii) number of times the rule ``could have been'' used.
Outputs a weighted version of the input grammar, given prefixing `num / denom' to each rule.
************************************************************************************************)

let _START_SYMBOL_ = "S"

type rule_counter = (Rule.r, int) Hashtbl.t
type nonterm_counter = (string, int) Hashtbl.t

let increment counter_tbl key =
	let n = try (Hashtbl.find !counter_tbl key) with Not_found -> 0 in
	Hashtbl.replace !counter_tbl key (n+1)

(*** NB: This is just copied from main.ml for now. ***
 *** Should be put in some shared file sometime.   ***)
let get_input_grammar grammar_file =
    let channel = open_in grammar_file in 
    let lexbuf = Lexing.from_channel channel in 
    Read.mcfgrule Lexer.token lexbuf

let read_sentences filename =
	let channel = open_in filename in
	let rec get_all_lines ch acc =
		try
			get_all_lines ch ((input_line ch) :: acc)
		with End_of_file -> close_in ch ; acc
	in
	reverse_tr (get_all_lines channel [])

(* Find the rule matching a certain parent nonterm and a certain right-hand side *)
(* HACK WARNING: the last argument here is EITHER (i) a list of nonterminals, if we're looking for a nonterminating rule; 
                                               OR (ii) a list containing a single terminal, if we're looking for a terminating rule. *)
let find_rule rules parent rhs =
	let is_target r =
		if (Rule.get_nonterm r) <> parent then
			false
		else
			match (Rule.get_expansion r) with
			| PublicTerminating s -> (rhs = [s])
			| PublicNonTerminating (nts, f) -> ((Nelist.to_list nts) = rhs)
	in
	let results = List.filter is_target rules in
	if (List.length results) = 1 then
		List.hd results
	else
		let description = Printf.sprintf "item with parent %s and expansion %s" parent (String.concat "," rhs) in
		if (List.length results) < 1 then
			failwith (Printf.sprintf "Something's wrong: Couldn't find a rule for %s" description)
		else
			failwith (Printf.sprintf "Don't know what to do: Two distinct rules possible for %s" description)

let get_single_yield sentence item =
	match (Chart.get_ranges item) with
	| ((r1,r2)::[]) -> (
		match (r1,r2) with
		| (EpsVar, EpsVar) -> " "
		| (RangeVal i, RangeVal j) -> String.concat " " (List.map (List.nth sentence) (Util.range i j))
		| _ -> failwith "Should never mix EpsVar with RangeVal"
	)
	| _ -> failwith (Printf.sprintf "Something's wrong: Item passed to get_single_yield that has multiple yields: %s" (Chart.debug_str item))

let rec process_item sentence rules rule_uses nonterm_uses item =
	let nt = Chart.get_nonterm item in
	let children  =
		Chart.get_antecedents item
	in
	let rhs = if (children = []) then [get_single_yield sentence item] else (List.map Chart.get_nonterm children) in
	let r = find_rule rules nt rhs in
	increment rule_uses r ;
	increment nonterm_uses nt ;
	List.iter (process_item sentence rules rule_uses nonterm_uses) children

let process_sentence (rules : Rule.r list) rule_uses nonterm_uses (sentence : string) =
	let split_sentence = Util.split ' ' sentence in
	let chart = Parser.deduce (-1) rules (Parser.Sentence split_sentence) in
	let goal_items = List.filter (Parser.is_goal _START_SYMBOL_ (List.length split_sentence)) chart in
	if (goal_items = []) then
		Printf.eprintf "Warning: no parse found for sentence \"%s\"\n" sentence
	else
		List.iter (process_item split_sentence rules rule_uses nonterm_uses) goal_items

let run_training grammar_file sentence_file =

	let grammar = get_input_grammar grammar_file in
	let (sentences : string list) = read_sentences sentence_file in

	let rule_uses = ref (Hashtbl.create (List.length grammar)) in
	let nonterm_uses = ref (Hashtbl.create ((List.length grammar)/10)) in
	List.iter (process_sentence grammar rule_uses nonterm_uses) sentences ;

	let print_weighted_rule r =
		let num = try Hashtbl.find !rule_uses r with Not_found -> 0 in
		let denom = try Hashtbl.find !nonterm_uses (Rule.get_nonterm r) with Not_found -> 0 in
		Printf.printf "%d / %d    %s\n" num denom (Rule.to_string r)
	in
	List.iter print_weighted_rule grammar

let print_usage () =
	Printf.eprintf "Usage: %s <grammar-file> <sentence-file>\n" Sys.argv.(0)

let main () =
	if (Array.length Sys.argv = 3) then
		run_training Sys.argv.(1) Sys.argv.(2)
	else
		print_usage ()

let _ = if (!Sys.interactive) then () else main ()
